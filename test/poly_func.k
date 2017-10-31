fn add(a $T, b T) -> T {
    a + b
}

fn add2(a $T, b i64) -> T {
    a + b
}

fn non_poly_add(a i64, b i64) -> i64 {
    a + b
}

fn copy(x $T) -> T {
    let y T;
    y = x;

/*
    let z = y;
    let w = z;

    let a = non_poly_add(z, 3);
    let ap = add(z, 3);
    */

    y
}

fn main() {
//    let x = non_poly_add(3.0, "test");

//    let a0 = add(3, 4);
//    let a1 = add(5, 7);
    let a2 = add(5, 7.0);
//    let a3 = add(9.0, 11);
    /*
    let long_identifier = add(3.0, 4.0);
    let really_long_identifier = add(5.0, 7.0);
        let indented_line = add(8.0, 12.0);
        */

//    let c0 = copy(3);
//    let c1 = copy(3.0);
}
