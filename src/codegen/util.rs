
use std::sync::RwLock;

pub fn fresh_name() -> String {
    let mut name_n = NAME_NUMBER.write().unwrap();
    *name_n += 1;
    format!(".generated.name.{}", *name_n)
}

lazy_static! {
    static ref NAME_NUMBER: RwLock<i32> = RwLock::new(0);
}

