struct Vec3 {
    x i64;
    y i64;
    z i64;
};

fn add(a i64, b i64) -> i64 {
    a + b
}

fn main() {
    let x = 3;
    let y = 4;
    let z = add(x, y);
    let w = add(add(z, y), add(add(3, 4), x));

    let v Vec3;
}