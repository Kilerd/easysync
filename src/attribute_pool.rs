use std::collections::HashMap;

pub struct AttributePool {
    numToAttrib: HashMap<i32, (String, String)>,
    attribToNum: HashMap<String, i32>,
    nextNum: i32,
}

impl AttributePool {
    pub fn new() -> Self {
        AttributePool {
            numToAttrib: Default::default(),
            attribToNum: Default::default(),
            nextNum: 0,
        }
    }

    pub fn putAttrib(&mut self, attrib: (impl Into<String>,impl Into<String>), dont_add_if_absent: bool) -> i32 {
        let key = attrib.0.into();
        let value = attrib.1.into();
        let str: String = format!("{},{}", &key, &value);
        if let Some(index) = self.attribToNum.get(&str) {
            return *index;
        }
        if dont_add_if_absent {
            return -1;
        }

        self.attribToNum.insert(str, self.nextNum);
        self.numToAttrib.insert(self.nextNum, (key, value));
        self.nextNum += 1;
        return self.nextNum;
    }

    pub fn getAttrib(&self, num: i32) -> Option<(String,String)> {
        self.numToAttrib.get(&num).map(|v| v.clone())
    }

    pub fn getAttribKey(self, num: i32) -> Option<String> {
        self.numToAttrib
            .get(&num)
            .map(|v| v.0.clone())
    }
    pub fn getAttribValue(self, num: i32) -> Option<String> {
        self.numToAttrib
            .get(&num)
            .map(|v| v.1.clone())
    }
    pub fn getAttributeId(&self, k: &str, v:&str) -> i32 {
        *self.attribToNum.get(&format!("{},{}", k, v)).unwrap()
    }
}