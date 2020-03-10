use std::str::FromStr;

use regex::Regex;

use crate::attribute_pool::AttributePool;
use crate::component::{OpAttribute, OpAttributeCode, RawStringPair};
use crate::util::to36String;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
pub struct AttributeList {
    inner: Vec<OpAttribute>,
}

impl AttributeList {
    pub fn empty() -> Self {
        AttributeList { inner: vec![] }
    }
    pub fn new(inner: Vec<OpAttribute>) -> Self {
        AttributeList { inner }
    }
}

impl AttributeList {
    pub fn unpack(a: String, pool: &AttributePool) -> Self {
        let mut ret = Self { inner: vec![] };
        let regex = Regex::new("([*^])([0-9a-z]+)").unwrap();
        let matches = regex.captures_iter(&a);
        for x in matches {
            let attribute_id: i32 = i32::from_str_radix(x.get(2).unwrap().as_str(), 36).unwrap();
            let option = &pool.getAttrib(attribute_id).unwrap();
            let x1 = option.clone();

            let opcode = x.get(1).unwrap().as_str();
            let opcode = OpAttributeCode::from_str(opcode).unwrap();

            let attribute = OpAttribute {
                opcode,
                key: x1.0,
                value: x1.1,
            };
            ret.inner.push(attribute)
        }
        ret
    }
    pub fn add(&mut self, key: String, value: String) {
        self.inner
            .push(OpAttribute::new(OpAttributeCode::FORMAT, key, value));
    }

    pub fn equals(&self, other: &AttributeList) -> bool {
        let list1 = &self.inner;
        let list2 = &other.inner;
        if list1.len() == list2.len() {
            let l = list1.len();
            let mut m = 0;
            for i in 0..l {
                let old_m = m;
                for j in 0..l {
                    if list1[i].eq(&list2[j]) {
                        m += 1;
                        break;
                    }
                }
                if old_m == m {
                    break;
                }
            }

            m == l
        } else {
            false
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn merge(&self, other: AttributeList) -> AttributeList {
        let mut c = self.clone();
        let this_len = c.inner.len();
        let vec = &other.inner;

        for i in 0..this_len {
            let new_op = &vec[i];
            let mut found = false;
            let mut j = 0;
            while !found && j < this_len {
                let op = &c.inner[j];
                if op.opcode == new_op.opcode && op.key == new_op.key && op.value != new_op.value {
                    c.inner.remove(j);
                    c.inner.insert(j, new_op.clone());
                    found = true;
                } else if op.opcode != new_op.opcode
                    && op.key == new_op.key
                    && op.value == new_op.value
                {
                    panic!(" cannot merge mutual ops, use compose or format instead");
                }

                j += 1;
            }
            if !found {
                c.inner.push(new_op.clone());
            }
        }

        c
    }

    pub fn compose(&self, other: &AttributeList, is_composition: bool) -> AttributeList {
        let mut list = self.clone();
        let mut this_len = list.inner.len();
        for i in 0..other.inner.len() {
            let other_op = &other.inner[i];

            let mut found = false;

            let mut j = 0;
            while !found && j < this_len {
                let this_op = &list.inner[i];
                assert!(
                    !this_op.eq(other_op),
                    "trying to compose identical opattributes {}",
                    other_op.key
                );
                if this_op.opcode != other_op.opcode
                    && this_op.key == other_op.key
                    && this_op.value == other_op.value
                {
                    list.inner.remove(j);
                    this_len -= 1;
                    found = true;
                }
                j += 1;
            }
            if !found {
                list.inner.push(other_op.clone());
            }
        }
        list
    }
    pub fn transform(&self, other: &AttributeList) -> AttributeList {
        let mut res = vec![];

        for i in 0..self.inner.len() {
            let thisOp = &self.inner[i];
            let mut skip = false;
            let mut j = 0;
            while !skip && j < other.inner.len() {
                let otherOp = &other.inner[j];
                if thisOp.eq(otherOp) {
                    skip = true;
                } else if thisOp.key == otherOp.key
                    && thisOp.opcode == otherOp.opcode
                    && thisOp.opcode == OpAttributeCode::FORMAT
                {
                    if thisOp.value.ne(&otherOp.value) {
                        res.push(OpAttribute::new(
                            OpAttributeCode::REMOVE,
                            otherOp.key.clone(),
                            otherOp.value.clone(),
                        ));
                    }
                    skip = true;
                } else if (thisOp.key == otherOp.key
                    && thisOp.value == otherOp.value
                    && thisOp.opcode != otherOp.opcode)
                    || (thisOp.key == otherOp.key
                        && thisOp.value != otherOp.value
                        && thisOp.opcode == OpAttributeCode::REMOVE)
                {
                    panic!("invalid operation for transform");
                }
                j += 1;
            }
            if !skip {
                res.push(thisOp.clone());
            }
        }
        AttributeList::new(res)
    }

    pub fn format(&self, other: &AttributeList) -> AttributeList {
        let mut res = vec![];
        for i in 0..other.inner.len() {
            let format_op = &other.inner[i];
            let mut skip = false;
            let mut j = 0;
            while !skip && j < self.inner.len() {
                let this_op = &self.inner[j];
                if format_op.key == this_op.key && format_op.value == this_op.value {
                    if format_op.opcode == OpAttributeCode::REMOVE {
                        res.push(format_op.clone());
                    }
                    skip = true;
                } else if format_op.key == this_op.key
                    && format_op.value != this_op.value
                    && format_op.opcode == OpAttributeCode::FORMAT
                {
                    res.push(OpAttribute::new(
                        OpAttributeCode::REMOVE,
                        this_op.key.clone(),
                        this_op.value.clone(),
                    ));
                    res.push(format_op.clone());
                    skip = true;
                }

                j += 1;
            }
            if !skip && format_op.opcode == OpAttributeCode::FORMAT {
                res.push(format_op.clone());
            }
        }

        AttributeList::new(res)
    }
    pub fn invert(&self, other: Option<&AttributeList>) -> AttributeList {
        let mut res = vec![];
        for i in 0..self.inner.len() {
            let op = &self.inner[i];

            let mut found = false;
            if let Some(other) = other {
                for j in 0..other.inner.len() {
                    if op.eq(&other.inner[j]) {
                        found = true;
                        break;
                    }
                }
            }
            if !found {
                res.push(op.invert());
            } else {
                res.push(op.clone());
            }
        }
        AttributeList::new(res)
    }

    pub fn pack(&self, pool: &AttributePool) -> String {
        let mut mapped = vec![];
        for x in &self.inner {
            let i = pool.getAttributeId(&x.key, &x.value);
            mapped.push((x, i));
        }

        mapped.sort_by(|a, b| {
            if a.0.opcode == b.0.opcode {
                a.1.cmp(&b.1)
            } else {
                if a.0.opcode == OpAttributeCode::FORMAT {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
        });

        let mut s = String::new();

        for i in mapped {
            s.push_str(&format!("{}{}", i.0.opcode, to36String(i.1)));
        }
        s
    }
}

#[cfg(test)]
mod test {
    use crate::attribute::AttributeList;
    use crate::attribute_pool::AttributePool;
    use crate::component::{OpAttribute, OpAttributeCode};
    use crate::tests::pool;

    #[test]
    fn test_unpack_attribute() {
        let mut pool1 = AttributePool::new();
        pool1.putAttrib(("moo", "zoo"), false);
        pool1.putAttrib(("foo", "bar"), false);
        let list = AttributeList::unpack("^0*1".to_string(), &pool1);

        assert_eq!(list.inner.len(), 2);
        assert_eq!(
            list.inner.get(0).unwrap(),
            &OpAttribute {
                opcode: OpAttributeCode::REMOVE,
                key: "moo".to_string(),
                value: "zoo".to_string(),
            }
        );
        assert_eq!(
            list.inner.get(1).unwrap(),
            &OpAttribute {
                opcode: OpAttributeCode::FORMAT,
                key: "foo".to_string(),
                value: "bar".to_string(),
            }
        );
    }

    #[test]
    fn test_invert() {}

    #[test]
    fn test_pack() {
        fn pack_test_base(
            raw: impl Into<String>,
            expected: impl Into<String>,
            reason: impl Into<String>,
        ) {
            let pool1 = pool();
            let list = AttributeList::unpack(raw.into(), &pool1);
            let pack_ret = list.pack(&pool1);
            assert_eq!(pack_ret, expected.into(), "{}", reason.into());
        }
        pack_test_base("*1*0", "*0*1", "sort Ns");
        pack_test_base("*0^2", "^2*0", "reorder removes before formats");
    }
}
