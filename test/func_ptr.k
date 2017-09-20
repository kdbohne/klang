fn foo(x i64) -> i64 {
    x * 2
}

fn bar(x i64) -> i64 {
    x + 5
}

fn baz() {
    // empty
}

fn main() {
    let f fn(i64) -> i64;
    f = foo;

//    let v fn();
//    v = baz;

//    let g fn(i64) -> i64 = bar;

//    let h = bar;
}
