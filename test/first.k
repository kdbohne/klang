extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;
extern fn malloc(bytes i64) -> *u8;

/*
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
    let xx i8;
    xx = 129;
    xx = -128;

    let x i32;
    let y i64 = 42;
    let z = 17;

    x = 43;

    let t1 = 3;
    let t2 = 2;
    let t3 = t1 + t2;

    let a = 4;
    let b = &a;
    let c = *b;

    let v Vec3;
    v.x = 3.14159;
    v.y = 22.84;
    v.z = v.x + v.y;

    let s = string_make("test string!");

    let m = malloc(256);
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "120\n", cast(*i64) 4, cast(*i64) 0, cast(*i64) 0);

    /*
    let a = 5;
    let b = &a;
    let c = b + &a;
    let d = &b;
    let e = &d;
    let f = &e + 3;
    */

    /*
    x := factorial(5);
    if x == 120 {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "120\n", cast(*i64) 4, cast(*i64) 0, cast(*i64) 0);
    };
    */

    /*
    x := 3;
    y := (x = 4) + 3;

    if add(81, 256) == inc(add(inc(299), 36)) {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "asdf", cast(*i64) 4, cast(*i64) 0, cast(*i64) 0);
    };
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "\n", cast(*i64) 1, cast(*i64) 0, cast(*i64) 0);
    */

    /*
    x := 3;
    y := inc(x);
    z := add(x, y);

    f := 3.14159;

    x := 3;
    xp := &x;
    xpp := &xp;

    x = 8;

    str := "hello!\n";
    stri := cast(*i64) str;
    strp := &str;

    ptr := &f;
    ptri := cast(*i64) ptr;

    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "test\n", cast(*i64) 5, cast(*i64) 0, cast(*i64) 0);
    */

    /*
    a :=
    {
        b := 3;
        b2 := b * b;

        b2 * b2
    };

    /*
    a =
    {
        b := 4;
        b2 := b * b;

        b2 + b2
    };
    */

    if a == 80 {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "true", cast(*i64) 4, cast(*i64) 0, cast(*i64) 0);
    } else if a == 81 {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "true2", cast(*i64) 5, cast(*i64) 0, cast(*i64) 0);
    } else {
        syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "false", cast(*i64) 5, cast(*i64) 0, cast(*i64) 0);
    };
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) "\n", cast(*i64) 1, cast(*i64) 0, cast(*i64) 0);
    */
}
