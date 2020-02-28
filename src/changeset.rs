use std::i32;
use std::iter::Peekable;
use std::slice::Iter;

use itertools::Itertools;
use regex::Regex;

use crate::attribute::AttributeList;
use crate::attribute_pool::AttributePool;
use crate::component::{ComponentList, Operation, OperationCode, RawStringPair};
use crate::util::to36String;

/// A changeset represents a change to a document.
#[derive(Debug)]
pub struct ChangeSet {
    old_length: i32,
    new_length: i32,
    ops: ComponentList,
}


impl ChangeSet {
    pub fn new(old_length: i32, new_length: i32, ops: ComponentList) -> Self {
        Self {
            old_length,
            new_length,
            ops,
        }
    }

    pub fn to_str(self) -> String {

        // TODO
        unimplemented!()
    }

    pub fn from_str(changeset: String, pool: &AttributePool) -> Self {
        let re = Regex::new("Z:([0-9a-z]+)([><])([0-9a-z]+)([^$]*)(\\$((?s:.)*))?").unwrap();
        let option = re.captures(&changeset);
        if let Some(match_) = option {
            let old_length = match_.get(1).unwrap().as_str();
            let old_length = i32::from_str_radix(old_length, 36).unwrap();
            let change_sign: i32 = if match_.get(2).unwrap().as_str().eq(">") { 1 } else { -1 };
            let chang_mag = i32::from_str_radix(match_.get(3).unwrap().as_str(), 36).unwrap();

            let new_length = old_length + change_sign * chang_mag;
            let ops = match_.get(4).map(|m| m.as_str().to_string()).unwrap_or_default();
            let char_bank = match_.get(6).map(|m| m.as_str().to_string()).unwrap_or_default();

            let ops = ComponentList::unpack((ops, char_bank), pool);
            Self {
                old_length,
                new_length,
                ops,
            }
        } else {
            unimplemented!()
        }
    }

    pub fn apply(&self, content: &str) -> String {
        assert_eq!(self.old_length, content.len() as i32);
        let mut assm = String::new();
        let mut chars = content.chars();
        for operation in &self.ops.inner {
            match operation.op_code {
                OperationCode::INSERT => {
                    assm.push_str(&operation.char_bank);
                }
                OperationCode::REMOVE => {
                    chars = chars.dropping(operation.chars as usize);
                }
                OperationCode::KEEP => {
                    chars.by_ref().take(operation.chars as usize).for_each(|c| assm.push(c));
                }
            }
        }
        chars.for_each(|c| assm.push(c));
        assm
    }

    fn zipNext(op: &mut Option<Operation>, part: &mut Option<Operation>, iter: &mut Peekable<Iter<Operation>>) {
        if op.is_none() {
            if let Some(part_op) = part {
                *op = Some(Operation::copy(part_op));
                *part = None;
            } else if let Some(o) = iter.next() {
                *op = Some(Operation::copy(o));
            }
        }
    }
    pub fn zip(
        list1: ComponentList,
        list2: ComponentList,
        need_split_func: fn(&Operation, &Operation) -> bool,
        mut func: impl FnMut(&mut Option<Operation>, &mut Option<Operation>, &mut Option<Operation>) -> (),
    ) -> ComponentList {
        let mut iter1 = list1.inner.iter().peekable();
        let mut iter2 = list2.inner.iter().peekable();
        let mut res = ComponentList::new();
        let mut op1: Option<Operation> = None;
        let mut op1part: Option<Operation> = None;
        let mut op2: Option<Operation> = None;
        let mut op2part: Option<Operation> = None;
        let mut op_out: Option<Operation> = None;

        while op1.is_some() || op1part.is_some() || iter1.peek().is_some() || op2.is_some() || op2part.is_some() || iter2.peek().is_some() {
            ChangeSet::zipNext(&mut op1, &mut op1part, &mut iter1);
            ChangeSet::zipNext(&mut op2, &mut op2part, &mut iter2);

            if let (Some(op1_inner), Some(op2_inner)) = (&mut op1, &mut op2) {
                // pre-splitting into equal slices greatly reduces
                // number of code branches and makes code easier to read
                let split = need_split_func(op1_inner, op2_inner);

                if split && op1_inner.chars > op2_inner.chars {
                    let mut operation = Operation::copy(op1_inner);
                    operation.trim_left(op2_inner.chars, op2_inner.lines);
                    op1part = Some(operation);

                    op1_inner.trim_right(op2_inner.chars, op2_inner.lines);
                } else if split && op1_inner.chars < op2_inner.chars {
                    let mut operation = Operation::copy(op2_inner);
                    operation.trim_left(op1_inner.chars, op1_inner.lines);
                    op2part = Some(operation);
                    op2_inner.trim_right(op1_inner.chars, op1_inner.lines);
                }
            }
            func(&mut op1, &mut op2, &mut op_out);


            if let Some(inner) = op_out.take() {
                res.push(inner);
            }
        }

        return res;
    }

    pub fn transform(mut self, mut other: ChangeSet, reverse_insert_order: bool) -> ChangeSet {
        assert_eq!(self.old_length, other.old_length);
        self.ops.reorder();
        other.ops.reorder();
        let mut dlen = 0;
        let list = ChangeSet::zip(
            self.ops,
            other.ops,
            |this_op, other_op| {
                // INSERTs are handled unsplitted, always
                let has_insert = this_op.op_code == OperationCode::INSERT || other_op.op_code == OperationCode::INSERT;
                // KEEPs can be reduced by REMOVEs or extended by INSERTs
                let has_keep = this_op.op_code == OperationCode::KEEP || other_op.op_code == OperationCode::KEEP;
                // REMOVEs can reduce KEEPs other REMOVEs
                let has_remove = this_op.op_code == OperationCode::REMOVE || other_op.op_code == OperationCode::REMOVE;
                // in both situation we can split ops into equal slices
                (has_keep || has_remove) && !has_insert
            },
            move |this_op, other_op, op_out| {
                match (&this_op, &other_op) {
                    (Some(inner1), Some(inner2))
                    if inner1.op_code == OperationCode::INSERT || inner2.op_code == OperationCode::INSERT => {
                        let left;

                        let mut this_char: String = inner1.char_bank.chars().take(1).collect();
                        let mut other_char: String = inner2.char_bank.chars().take(1).collect();

                        if inner1.op_code != inner2.op_code {
                            // the op that does insert goes first
                            left = (inner2.op_code == OperationCode::INSERT);
                        } else if (this_char.eq("\n") || other_char.eq("\n")) && this_char.ne(&other_char) {
                            // insert string that doesn't start with a newline first
                            // to not break up lines
                            left = other_char.ne("\n");
                        } else {
                            left = reverse_insert_order == false;
                        }

                        if left {
                            // other op goes first
                            *op_out = Some(Operation::new(OperationCode::KEEP, inner2.chars, inner2.lines, AttributeList::empty(), String::new()));

                            other_op.take();
                        } else {
                            *op_out = Some(Operation::copy(inner1));
                            this_op.take();
                        }
                    }
                    (Some(inner1), Some(inner2)) if inner1.op_code != OperationCode::INSERT && inner2.op_code == OperationCode::REMOVE => {
                        // block 5
                        this_op.take();
                        other_op.take();
                    }
                    (Some(inner1), Some(inner2)) if inner1.op_code == OperationCode::REMOVE && inner2.op_code == OperationCode::KEEP => {
                        // block 2 3 5
                        let mut operation = Operation::copy(inner1);
                        operation.composeAttributes(inner2);
                        *op_out = Some(operation);
                        this_op.take();
                        other_op.take();
                    }
                    (Some(inner1), Some(inner2)) if inner1.op_code == OperationCode::KEEP && inner2.op_code == OperationCode::KEEP => {
                        // block 2 4 5
                        let mut operation = Operation::copy(inner1);
                        operation.transformAttributes(inner2);
                        *op_out = Some(operation);
                        this_op.take();
                        other_op.take();
                    }
                    (Some(inner1), None) => {
                        // block 2 5
                        let mut operation = Operation::copy(inner1);
                        *op_out = Some(operation);
                        this_op.take();
                        other_op.take();
                    }
                    (None, Some(inner2)) => {
                        // block 5
                        this_op.take();
                        other_op.take();
                    }
                    (None, None) => {
                        // block 5
                        this_op.take();
                        other_op.take();
                    }
                    _ => {
                        dbg!(this_op);
                        dbg!(other_op);
                        unreachable!()
                    }
                }

                if let Some(opOut_inner) = op_out {
                    dlen += opOut_inner.deltaLen();
                }
            });
        ChangeSet::new(other.new_length, other.new_length + dlen, list)
    }

    pub fn compose(self, next: ChangeSet, pool: AttributePool) -> ChangeSet {
        unimplemented!()
    }
    pub fn pack(&mut self, pool: &AttributePool) -> String {
//        let x = self.ops.pack();
        let (ops_a, ops_s, ops_delta_len) = self.ops.pack(pool);
        let old_len = to36String(self.old_length);
        let change_sign = if ops_delta_len >= 0 { '>' } else { '<' };
        let delta_len = to36String(ops_delta_len.abs());

        let mut packed_cs = format!("Z:{old_len}{change_sign}{delta_len}{ops}", old_len = old_len, change_sign = change_sign, delta_len = delta_len, ops = ops_a);
        if ops_s.len() > 0 {
            packed_cs.push_str(&format!("${}", ops_s));
        }
        packed_cs
    }
    pub fn invert(&self) -> ChangeSet {
        let invert_component_list = self.ops.invert();
        ChangeSet {
            old_length: self.new_length,
            new_length: self.old_length,
            ops: invert_component_list
        }
    }
}


#[cfg(test)]
mod test {
    use crate::changeset::ChangeSet;
    use crate::tests::pool;

    #[test]
    fn test_from_str() {
        let raw_changeset = "Z:z>4|2=m=b-1+2+3$abcde";
        let set = ChangeSet::from_str(String::from(raw_changeset), &pool());
        assert_eq!(35, set.old_length);
        assert_eq!(39, set.new_length);
    }

    #[test]
    fn test_transform() {
        fn transform(case: impl Into<String>, cs1: impl Into<String>, cs2: impl Into<String>, reverse: bool, expected: impl Into<String>) {
            let pool = pool();
            let cs1 = ChangeSet::from_str(cs1.into(), &pool);
            let cs2 = ChangeSet::from_str(cs2.into(), &pool);
            let mut transform_cs = cs1.transform(cs2, reverse);
            assert_eq!(transform_cs.pack(&pool), expected.into(), "{}", case.into());
        }

        transform("insert tie break left", "Z:0>2+2$ab", "Z:0>2+2$cd", false, "Z:2>2=2+2$ab");
        transform("insert tie break right", "Z:0>2+2$cd", "Z:0>2+2$ab", true, "Z:2>2+2$cd");
        transform("insert tie break by newline", "Z:0>2+2$ab", "Z:0>1|1+1$\n", false, "Z:1>2+2$ab");
        transform("insert tie break by newline (no affect of side=right)", "Z:0>2+2$ab", "Z:0>1|1+1$\n", true, "Z:1>2+2$ab");
        transform("insert tie break left when both newlines", "Z:0>2|1+1+1$\na", "Z:0>2|1+1+1$\nb", false, "Z:2>2|1=1=1|1+1+1$\na");
        transform("insert tie break right when both newlines", "Z:0>2|1+1+1$\nb", "Z:0>2|1+1+1$\na", true, "Z:2>2|1+1+1$\nb");
        transform("tie break when one of the ops is insert", "Z:2>1+1$a", "Z:2<1-1$b", false, "Z:1>1+1$a");
        transform("tie break when one of the ops is insert (no affect of side=right)", "Z:2>1+1$a", "Z:2<1-1$b", true, "Z:1>1+1$a");

        transform("remove when part was removed", "Z:8<4-4$abcd", "Z:8<2-2$ab", false, "Z:6<2-2$cd");
        transform("remove part when all was removed", "Z:8<2-2$ab", "Z:8<4-4$abcd", false, "Z:4>0");
        transform("remove from other keep", "Z:8<2-2$ab", "Z:8>0*0=4", false, "Z:8<2*0-2$ab");
        transform("keep affected by remove", "Z:8>2=8+2$ab", "Z:8<4-4$abcd", false, "Z:4>2=4+2$ab");
        transform("keep collapsed by remove", "Z:8>2=4+2$ab", "Z:8<6-6$abcdef", false, "Z:2>2+2$ab");
        transform("keep moved by bigger insert", "Z:4>1|1=4+1$a", "Z:4>5+5$bcdef", false, "Z:9>1|1=9+1$a");
    }

    #[test]
    fn test_pack() {
        let pool = pool();
        let raw_changeset = "Z:z>4|2=m=b-1+2+3$abcde";
        let expected_changeset = "Z:z>4|2=m=b-1+5$abcde";
        let mut set = ChangeSet::from_str(String::from(expected_changeset), &pool);
        let packed_changeset = set.pack(&pool);
        assert_eq!(packed_changeset, expected_changeset);
    }

    #[test]
    fn test_apply() {
        fn apply(reason: impl Into<String>, cs: impl Into<String>, content: impl Into<String>, expected: impl Into<String>) {
            let pool = pool();
            let mut set = ChangeSet::from_str(cs.into(), &pool);
            let string = set.apply(content.into().as_str());
            assert_eq!(string, expected.into(), "{}", reason.into());
        }
        apply("basic apply", "Z:0>1+1$a", "", "a",);
        apply("basic apply", "Z:z>4|2=m=b-1+2+3$oabcde", "Hello World\n\n Hello World Hello Wor", "Hello World\n\n Hello World Hello Wabcder",);
    }
    #[test]
    fn test_invert() {
        let pool1 = pool();
        let set = ChangeSet::from_str("Z:a>1=1^3*0=1*0|1=2*1=4*2|1-2*1|1+3$h\nij\n".to_string(), &pool1);
        let raw_content = "12345678h\n";
        let apply_content = set.apply(raw_content);
        let inverted_cs = set.invert();
        let inverted_content = inverted_cs.apply(apply_content.as_str());
        assert_eq!(inverted_content, raw_content);
    }
}