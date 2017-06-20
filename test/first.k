/*
extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;
extern fn malloc(bytes i64) -> *u8;

fn inc(x i64) -> i64
{
    x + 1
}

fn add(a i64, b i64) -> i64
{
    a + b
}

fn factorial(n i64) -> i64
{
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}
*/

struct Vec3
{
    x f32;
    y f32;
    z f32;
};

struct Foo
{
    v Vec3;
    t i64;
};

struct String
{
    data *i8;
    len i64;
};

fn string_make(data *i8) -> String
{
    let s String;
    s.data = data;
    s.len = 0;

    let p = data;
    loop {
        if *p == cast(i8) 0 {
            break;
        };

        s.len = s.len + 1;
        p = p + 1;
    };

    s
}

fn main()
{
    /*
    let v Vec3;
    v.x = 3.14159;
    v.y = 22.84;
    v.z = v.x + v.y;

    let vx = v.x;
    let vy = v.y;
    vy = v.x;
    let vz = vx + vy;
    v.z = vz;

    let f Foo;
    f.v = v;
//    f.v.x = 41.23;
    f.t = 97;
    */

    let s = string_make(cast(*i8) "test string!");

    /*
    let m = malloc(256);
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "120\n", cast(*i64) 4, cast(*i64) 0, cast(*i64) 0);
    */
}
