#[macro_use]
extern crate strum_macros;

mod attribute;
mod attribute_pool;
pub mod changeset;
mod component;
mod document;
mod position;
mod util;

#[cfg(test)]
mod tests {
    use crate::attribute_pool::AttributePool;

    pub fn pool() -> AttributePool {
        let mut pool1 = AttributePool::new();
        pool1.putAttrib(("bold", "true"), false);
        pool1.putAttrib(("author", "1"), false);
        pool1.putAttrib(("underline", "true"), false);
        pool1.putAttrib(("foo", "bar"), false);
        pool1.putAttrib(("list", "1"), false);
        pool1.putAttrib(("color", "#333"), false);
        pool1
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
