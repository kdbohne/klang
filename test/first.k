extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;
extern fn malloc(bytes u64) -> *c_void;

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
    data *u8;
    len i64;
};

fn string_len(str *u8) -> i64
{
    let len = 0;

    let p = str;
    loop {
        if *p == cast(u8) 0 {
            break;
        };

        len = len + 1;
        p = p + 1;
    };

    len
}

fn string_make(str *u8) -> String
{
    let s String;
    s.len = string_len(str);
    s.data = cast(*u8) malloc(cast(u64) (s.len + 1));

    let p = s.data;
    for i in 0..s.len {
        *p = *str;

        p = p + 1;
        str = str + 1;
    };

    s
}

fn write(data *u8, len i64)
{
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) data, cast(*i64) len, cast(*i64) 0, cast(*i64) 0);
}

fn print_string(str String)
{
    write(str.data, str.len);
}

fn print_string_literal(str *u8)
{
    let len = string_len(str);
    write(str, len);
}

fn print_i64(x i64)
{
    let cap = 128;
    let len = 0;
    let buf = cast(*u8) malloc(cast(u64) cap);

    if x < 0 {
        print_string_literal("-");
        x = -x;
    };

    // HACK: no character literals yet
    let zero = 48;

    let p = buf;
    loop {
        let d = x % 10;
        x = x / 10;

        let c = d + zero;
        *p = cast(u8) c;

        p = p + 1;
        len = len + 1;

        if x == 0 {
            break;
        };
    };

    let l = buf;
    let r = p - 1;
    while l < r {
        let tmp = *l;
        *l = *r;
        *r = tmp;

        l = l + 1;
        r = r - 1;
    };

    write(buf, len);
}

fn main()
{
    print_i64(123456789);
    print_string_literal("\n");

    print_i64(-347);
    print_string_literal("\n");

    print_i64(238947 - 123 + 45 * 34);
    print_string_literal("\n");
}
