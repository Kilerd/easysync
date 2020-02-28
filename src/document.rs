use crate::component::ComponentList;
use crate::attribute::AttributeList;

pub struct Document {
    ops: ComponentList,
    doc: String,
    len: i32,
    lines: Vec<String>,
    pool: Vec<String>,
    author: AttributeList
}

impl Document {
    pub fn from_str(text:impl Into<String>) {


    }
}