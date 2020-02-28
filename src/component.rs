use std::fmt::{Display, Error, Formatter};
use std::i32;
use std::panic::resume_unwind;
use std::str::FromStr;

use regex::Regex;

use crate::attribute::AttributeList;
use crate::attribute_pool::AttributePool;
use crate::util::to36String;

#[derive(Debug)]
pub struct ComponentList {
    pub inner: Vec<Operation>,
    dirty: bool,
}

/// a and s
pub type RawStringPair = (String, String);

impl ComponentList {
    pub fn new() -> Self {
        ComponentList { inner: vec![], dirty: false }
    }

    pub fn unpack(a: RawStringPair, pool: &AttributePool) -> Self {
        let regex = Regex::new("((?:[*^][0-9a-z]+)*)(?:\\|([0-9a-z]+))?([-+=])([0-9a-z]+)").unwrap();
        let matches = regex.captures_iter(&a.0);
        let mut n: i32 = 0;
        let mut ret = ComponentList { inner: vec![], dirty: false };
        let string = a.1;
        for x in matches {
            let chars = i32::from_str_radix(x.get(4).unwrap().as_str(), 36).unwrap();
            let opcode = OperationCode::from_str(x.get(3).unwrap().as_str()).unwrap();
            let lines: i32 = x.get(2).map(|m| i32::from_str_radix(m.as_str(), 36).unwrap()).unwrap_or(0);

            let char_bank = if opcode != OperationCode::KEEP {
                let string1 = string.chars().skip(n as usize).take(chars as usize).collect();
                n += chars;
                string1
            } else {
                "".to_string()
            };
            ret.push(Operation {
                op_code: opcode,
                chars: chars,
                lines: lines,
                attributes: AttributeList::unpack(x.get(1).unwrap().as_str().to_string(), &pool),
                char_bank: char_bank,
            });
        };

        ret
    }

    pub fn push(&mut self, operation: Operation) {
        if operation.chars > 0 {
            self.inner.push(operation);
            self.dirty = true;
        }
    }
    /// Reorders components to keep removals before insertions. Makes sens in tie operations to keep result consistent across clients.
    pub fn reorder(&mut self) {
        let mut res: Vec<Operation> = vec![];
        let mut insert_list = vec![];
        let mut keep_list = vec![];
        let mut remove_list = vec![];

        let mut last_op_code = OperationCode::KEEP;
        self.inner.iter().for_each(|op| {
            if op.op_code == OperationCode::KEEP && last_op_code != OperationCode::KEEP {
                res.append(&mut remove_list);
                res.append(&mut insert_list);
            } else if op.op_code != OperationCode::KEEP && last_op_code == OperationCode::KEEP {
                res.append(&mut keep_list);
            }
            let opcode = op.op_code;
            match opcode {
                OperationCode::KEEP => {
                    keep_list.push(Operation::copy(op));
                }
                OperationCode::INSERT => {
                    insert_list.push(Operation::copy(op));
                }
                OperationCode::REMOVE => {
                    remove_list.push(Operation::copy(op));
                }
            }
            last_op_code = opcode;
        });
//        for op in self.inner {
//
//        }

        res.append(&mut remove_list);
        res.append(&mut insert_list);
        res.append(&mut keep_list);
        self.dirty = false;
        self.inner = res;
    }

    /// ret is
    pub fn pack(&mut self, pool: &AttributePool) -> (String, String, i32) {
        let mut res_a = String::new();
        let mut res_s = String::new();
        let mut res_dlen = 0;

        let mut buf_a = String::new();
        let mut buf_s = String::new();
        let mut buf_dlen = 0;
        let mut buf_last: Option<Operation> = None;
        let mut buf_inner: Option<Operation> = None;

        self.reorder();
        for op in &self.inner {
            match &mut buf_last {
                Some(last) if last.op_code == op.op_code && last.attributes.equals(&op.attributes) => {
                    if op.lines > 0 {
                        if let Some(inner) = &mut buf_inner {
                            last.append(inner);
                        }
                        last.append(op);
                        buf_inner.take();
                    } else if last.lines == 0 {
                        last.append(op);
                    } else {
                        if let Some(inner) = &mut buf_inner {
                            inner.append(op);
                        } else {
                            buf_inner = Some(Operation::copy(op));
                        }
                    }
                }
                _ => {
                    let finalize = false;
                    if let Some(last) = &buf_last {
                        if finalize && last.op_code == OperationCode::KEEP && last.attributes.is_empty() {} else {
                            let operation_pack = last.pack(&pool);
                            res_a.push_str(&operation_pack.0);
                            res_s.push_str(&operation_pack.1);
                            res_dlen += operation_pack.2;

                            buf_last.take();

                            if let Some(inner) = &buf_inner {
                                let operation_pack = inner.pack(&pool);

                                res_a.push_str(&operation_pack.0);
                                res_s.push_str(&operation_pack.1);
                                res_dlen += operation_pack.2;
                            }
                            buf_inner.take();
                        }
                    }
                    buf_last = Some(Operation::copy(op));
                }
            }
        }
        let finalize = true;
        if let Some(last) = &buf_last {
            if finalize && last.op_code == OperationCode::KEEP && last.attributes.is_empty() {} else {
                let operation_pack = last.pack(&pool);
                res_a.push_str(&operation_pack.0);
                res_s.push_str(&operation_pack.1);
                res_dlen += operation_pack.2;

                buf_last.take();

                if let Some(inner) = &buf_inner {
                    let operation_pack = inner.pack(&pool);

                    res_a.push_str(&operation_pack.0);
                    res_s.push_str(&operation_pack.1);
                    res_dlen += operation_pack.2;
                }
                buf_inner.take();
            }
        }
        (res_a, res_s, res_dlen)
    }
    pub fn invert(&self) -> ComponentList {
        let mut ret = vec![];
        for x in &self.inner {
            let operation = x.invert();
            ret.push(operation);

        }
        ComponentList {
            inner: ret,
            dirty: false
        }
    }
}


#[derive(Debug, PartialEq, Copy, Clone, EnumString)]
pub enum OperationCode {
    #[strum(serialize = "+")]
    INSERT,
    #[strum(serialize = "-")]
    REMOVE,
    #[strum(serialize = "=")]
    KEEP,
}

impl Display for OperationCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            OperationCode::INSERT => {
                write!(f, "+");
            }
            OperationCode::REMOVE => {
                write!(f, "-");
            }
            OperationCode::KEEP => {
                write!(f, "=");
            }
        }
        Ok(())
    }
}


#[derive(Debug)]
pub struct Operation {
    pub op_code: OperationCode,
    pub chars: i32,
    pub lines: i32,
    pub attributes: AttributeList,
    pub char_bank: String,
}

impl Operation {
    pub fn new(op_code: OperationCode, chars: i32, lines: i32, attributes: AttributeList, char_bank: String) -> Self {
        Operation { op_code, chars, lines, attributes, char_bank }
    }

    pub fn composeAttributes(&mut self, other: &Operation) {
        let list = self.attributes.compose(&other.attributes, self.op_code == OperationCode::KEEP);
        self.attributes = list;
    }
    pub fn transformAttributes(&mut self, other: &Operation) {
        let list = self.attributes.transform(&other.attributes);
        self.attributes = list;
    }
    pub fn formatAttributes(&mut self, other: &Operation) {
        let list = self.attributes.format(&other.attributes);
        self.attributes = list;
    }
    pub fn invertAttributes(&mut self, other: &Operation) {
        let list = self.attributes.invert(Some(&other.attributes));
        self.attributes = list;
    }
    pub fn deltaLen(&self) -> i32 {
        match self.op_code {
            OperationCode::INSERT => self.chars,
            OperationCode::REMOVE => -1 * self.chars,
            OperationCode::KEEP => 0
        }
    }
    pub fn pack(&self, pool: &AttributePool) -> (String, String, i32) {
        let mut a = String::new();

        let attribute_pack = self.attributes.pack(pool);
        a.push_str(&attribute_pack);
        if self.lines > 0 {
            a.push_str(&format!("|{}", to36String(self.lines)));
        }
        a.push_str(&self.op_code.to_string());
        a.push_str(&to36String(self.chars));
        (a, self.char_bank.clone(), self.deltaLen())
    }

    pub fn append(&mut self, other: &Operation) {
        if other.chars == 0 {
            return;
        }

        if self.chars == 0 {
            self.op_code == other.op_code;
            self.attributes = other.attributes.clone();
        }
        self.chars += other.chars;
        self.lines += other.lines;
        self.char_bank.push_str(&other.char_bank);
    }

    pub fn invert(&self) -> Operation {
        let list = self.attributes.invert(None);
        let code = match self.op_code {
            OperationCode::INSERT => OperationCode::REMOVE,
            OperationCode::REMOVE => OperationCode::INSERT,
            OperationCode::KEEP => OperationCode::KEEP
        };
        Operation {
            op_code: code,
            chars: self.chars,
            lines: self.lines,
            attributes: list,
            char_bank: self.char_bank.clone()
        }
    }
}

impl Operation {
    pub fn copy(other: &Operation) -> Self {
        Self {
            op_code: other.op_code,
            chars: other.chars,
            lines: other.lines,
            attributes: other.attributes.clone(),
            char_bank: other.char_bank.clone(),
        }
    }
    pub fn trim_left(&mut self, n: i32, l: i32) {
        assert!(self.chars >= n && self.lines >= l);

        self.chars -= n;
        self.lines -= l;
        self.char_bank = self.char_bank.chars().skip(n as usize).collect();
    }
    pub fn trim_right(&mut self, n: i32, l: i32) {
        assert!(self.chars >= n && self.lines >= l);

        self.chars = n;
        self.lines = l;
        self.char_bank = self.char_bank.chars().take(n as usize).collect();
    }
}

#[derive(Debug, Clone, PartialEq, EnumString)]
pub enum OpAttributeCode {
    #[strum(serialize = "*")]
    FORMAT,
    #[strum(serialize = "^")]
    REMOVE,
}

impl Display for OpAttributeCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            OpAttributeCode::FORMAT => {
                write!(f, "*");
            }
            OpAttributeCode::REMOVE => {
                write!(f, "^");
            }
        }
        Ok(())
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct OpAttribute {
    pub opcode: OpAttributeCode,
    pub key: String,
    pub value: String,
}

impl OpAttribute {
    pub fn new(opcode: OpAttributeCode, key: String, value: String) -> Self {
        OpAttribute { opcode, key, value }
    }

    pub fn invert(&self) -> Self {
        let opcode = if self.opcode == OpAttributeCode::FORMAT { OpAttributeCode::REMOVE } else { OpAttributeCode::FORMAT };
        Self {
            opcode,
            key: self.key.clone(),
            value: self.value.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    mod component_list {
        use crate::attribute::AttributeList;
        use crate::component::{ComponentList, Operation, OperationCode};
        use crate::tests::pool;

        #[test]
        fn test_unpack_and_repack_with_pool() {
            let pool = pool();
            let raw_a = "-c*3*4+6|3=a^1^3*2*5+1=1-1+1*0+1=1-1+1|c=c=2*0|2=2-1=3+1";
            let raw_s = "12345678901212345611111111";

            let mut list = ComponentList::unpack(
                (
                    String::from(raw_a),
                    String::from(raw_s)
                ),
                &pool,
            );
            let (pack_a, pack_s, _) = list.pack(&pool);
            assert_eq!(pack_a, raw_a);
            assert_eq!(pack_s, raw_s);
        }

        #[test]
        fn test_pack() {
            fn pack(reason: impl Into<String>, list_args: Vec<(OperationCode, i32, i32, impl Into<String>, impl Into<String>)>, packed: impl Into<String>, delta_len: i32) {
                let pool1 = pool();
                let operation_list: Vec<Operation> = list_args.into_iter().map(|op_args| {
                    let list = AttributeList::unpack(op_args.3.into(), &pool1);
                    let operation = Operation::new(op_args.0, op_args.1, op_args.2, list, op_args.4.into());
                    operation
                }).collect();
                let mut list1 = ComponentList { inner: operation_list, dirty: false };
                let x = list1.pack(&pool1);
                assert_eq!(format!("{}${}", x.0, x.1), packed.into());
                assert_eq!(x.2, delta_len);
            }

            pack("can merge inline ops", vec![(OperationCode::INSERT, 1, 0, "*0", "a"), (OperationCode::INSERT, 2, 0, "*0", "bc")], "*0+3$abc", 3);
            pack("don't merge on different attribs", vec![(OperationCode::INSERT, 1, 0, "*1", "a"), (OperationCode::INSERT, 2, 0, "*0", "bc")], "*1+1*0+2$abc", 3);
            pack("don't merge on different opcodes", vec![(OperationCode::REMOVE, 1, 0, "*0", "a"), (OperationCode::INSERT, 2, 0, "*0", "bc")], "*0-1*0+2$abc", 1);
            pack("merge multiline and inline ops", vec![(OperationCode::INSERT, 1, 0, "*0", "a"), (OperationCode::INSERT, 2, 1, "*0", "b\n"), (OperationCode::INSERT, 2, 1, "*0", "c\n"), (OperationCode::INSERT, 2, 0, "*0", "de")], "*0|2+5*0+2$ab\nc\nde", 7);
            pack("drop trailing pure keep", vec![(OperationCode::INSERT, 1, 0, "*0", "a"), (OperationCode::KEEP, 2, 0, "", "")], "*0+1$a", 1);
            pack("keep formatting trailing keep", vec![(OperationCode::INSERT, 1, 0, "*0", "a"), (OperationCode::KEEP, 2, 0, "*1", "")], "*0+1*1=2$a", 1);

            pack("smart: put removes before inserts", vec![(OperationCode::INSERT, 2, 0, "", "ab"), (OperationCode::REMOVE, 2, 0, "", "cd")], "-2+2$cdab", 0);
            pack("smart: split by keep operation", vec![(OperationCode::INSERT, 2, 0, "", "ab"), (OperationCode::KEEP, 2, 0, "", ""), (OperationCode::REMOVE, 2, 0, "", "cd")], "+2=2-2$abcd", 0);
            pack("smart: remove final pure keeps", vec![(OperationCode::INSERT, 2, 0, "", "ab"), (OperationCode::KEEP, 2, 0, "", "")], "+2$ab", 2);
        }
    }

    mod operation {
        use crate::attribute::AttributeList;
        use crate::component::{Operation, OperationCode};
        use crate::tests::pool;

        #[test]
        fn test_pack() {
            let pool = pool();
            let operation = Operation::new(OperationCode::INSERT, 10, 2, AttributeList::unpack("*0*1".to_string(), &pool), "1234\n6789\n".to_string());
            let x = operation.pack(&pool);
            assert_eq!(x, ("*0*1|2+a".to_string(), "1234\n6789\n".to_string(), 10))
        }

        #[test]
        fn test_append() {
            let pool = pool();
            let mut operation = Operation::new(OperationCode::INSERT, 3, 1, AttributeList::unpack("*0".to_string(), &pool), "ab\nab\n".to_string());
            let other = Operation::new(OperationCode::INSERT, 3, 1, AttributeList::unpack("*0".to_string(), &pool), "ab\nab\n".to_string());

            operation.append(&other);
            assert_eq!(operation.op_code, OperationCode::INSERT);
            assert_eq!(operation.chars, 6);
            assert_eq!(operation.lines, 2);
            assert_eq!(operation.attributes.equals(&AttributeList::unpack("*0".to_string(), &pool)), true);
            assert_eq!(operation.char_bank, "ab\nab\nab\nab\n");
        }
    }
}