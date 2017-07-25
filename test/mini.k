struct Vec3 {
    x i64;
    y i64;
    z i64;
};

fn three() -> i64 {
    3
}

fn six() -> i64 {
    three() + three()
}

fn add_three(a i64) -> i64 {
    a + three()
}

fn add(a i64, b i64) -> i64 {
    a + b
}

fn main() {
    let x = 3;
    let y = 4;
    let z = add(x, y);
    let w = add(add(z, y), add(add(3, 4), x));
    add(5, 6);

    let k = add(x, y) + add(5, z);

    let v Vec3;

    let f = add_three(14);
    let g = three() + six();
    let hp = &f;
    let h = *hp;
}