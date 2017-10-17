fn add(a $T, b T) -> T {
    a + b
}

fn copy(x $T) -> T {
    let y T;
    y = x;
    y
}

fn main() {
    let a0 = add(3, 4);
    let a1 = add(5, 7);
    let long_identifier = add(3.0, 4.0);
    let really_long_identifier = add(5.0, 7.0);
        let indented_line = add(8.0, 12.0);

    let c0 = copy(3);
    let c1 = copy(3.0);
}
