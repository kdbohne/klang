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

    let a = v.x + 3;
    let b = v.y + v.z;

    let r = if v.x == 5 {
        26
    } else {
        13
    };
}