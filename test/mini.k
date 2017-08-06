fn main() {
    let x = 0;
    let y = 5;
    while x < y {
        x = x + 1;
    };
}

/*
fn main() {
    let x = 0;
    loop {
        if x == 5 {
            x = 4;
            break;
        };

        x = x + 1;
        let y = 3;
        y = y + x;
    };

    x = 3;
}
*/

/*
struct Foo {
    p *i64;
};

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

    let f = add_three(14);
    let g = three() + six();
    let hp = &f;
    let h = *hp;

    let v Vec3;
    v.x = 8;
    v.y = 2;
    v.z = 4;

    let a = v.x * 2 + 3;
    v.y = a - 1;
    v.z = v.y * v.x;

    if a == 1 {
        a = x + 5;
    };

    if x == 6 {
        x = x + 1;
        if x == 7 {
            x = 8;
        };

        if x == 8 {
            x = 9;
        } else {
            x = 10;
        };
    };

    let c = 4;
    if c == 4 {
        c = 5;
    } else {
        c = 6;
    };

    let d = if c == 5 {
        let m = 4 + 2;
        m * 2
    } else {
        5
    };
}
*/