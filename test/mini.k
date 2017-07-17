/*
struct Vec3 {
    w u8;
    x i64;
    y i64;
    z i64;
};

struct A {
    x u8;
    y u16;
    z u8;
};

fn main() {
    let v Vec3;
    v.x = 5;
    v.y = 12;
    v.z = 13;
}
*/

fn add(a i64, b i64) -> i64 {
    a + b
}

fn main() {
    let x = 4;
    let y = 16;
    let z = add(x, y);

    x = y + z;
}