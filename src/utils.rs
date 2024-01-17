#[macro_export]
macro_rules! pluralize {
    ($num: expr) => {
        if $num > 1 {
            "s"
        } else {
            ""
        }
    };
}
