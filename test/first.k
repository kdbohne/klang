extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;
extern fn malloc(bytes u64) -> *c_void;
extern fn free(mem *c_void);

extern fn bit_and(a u64, b u64) -> u64;
extern fn bit_or(a u64, b u64) -> u64;
extern fn bit_shift_left(x u64, amount u64) -> u64;
extern fn bit_shift_right(x u64, amount u64) -> u64;

struct Vec3 {
    x f32;
    y f32;
    z f32;
};

struct String {
    data *u8;
    len i64;
};

fn string_len(str *u8) -> i64 {
    let len = 0;

    let p = str;
    loop {
        if *p == 0 {
            break;
        };

        len = len + 1;
        p = p + 1;
    };

    len
}

fn string_make(str *u8) -> String {
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

fn write(data *u8, len i64) {
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) data, cast(*i64) len, cast(*i64) 0, cast(*i64) 0);
}

fn print_string(str String) {
    write(str.data, str.len);
}

fn print_string_literal(str *u8) {
    let len = string_len(str);
    write(str, len);
}

fn print_u64(x u64) {
    let cap = 128;
    let len = 0;
    let buf = cast(*u8) malloc(cast(u64) cap);

    // HACK: no character literals yet
    let zero u64 = 48;

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

    free(cast(*c_void) buf);
}

fn print_i64(x i64) {
    if x < 0 {
        print_string_literal("-");
        x = -x;
    };

    print_u64(cast(u64) x);
}

fn print_bits(xp *u64, n i64) {
    let x = *xp;

    for i in 0..n {
        let shift = cast(u64) (n - i - 1);

        let mask = bit_shift_left(1, shift);
        let bit = bit_and(x, mask);
        bit = bit_shift_right(bit, shift);

        print_u64(bit);
    };
}

fn print_f32(x f32) {
    let xp = cast(*u64) &x;

    let sign = bit_and(*xp, 0b10000000000000000000000000000000);
    if sign != 0 {
        print_string_literal("-");
    };

    let exp_u64 = bit_and(*xp, 0b01111111100000000000000000000000);
    exp_u64 = bit_shift_right(exp_u64, 23);

    let exp = cast(i64) exp_u64;
    exp = exp - 127;

    let man = bit_and(*xp, 0b00000000011111111111111111111111);
    man = bit_or(man,      0b00000000100000000000000000000000);

    let pt = 23;
    loop {
        if bit_and(man, 1) != 0 {
            break;
        };

        man = bit_shift_right(man, 1);
        pt = pt - 1;
    };
    pt = pt - exp;

    let whole = bit_shift_right(man, cast(u64) pt);
    print_u64(whole);

    print_string_literal(".");

    man = bit_shift_left(man, cast(u64) (32 - pt));
    man = bit_shift_right(man, cast(u64) (32 - pt));

    let frac = 0.0;
    for i in 0..pt {
        let mask = bit_shift_left(1, cast(u64) (pt - i - 1));
        let bit = 0;
        if bit_and(man, mask) != 0 {
            bit = 2;
        };

        for j in 0..i {
            bit = 2 * bit;
        };

        if bit != 0 {
            frac = frac + 1.0 / cast(f32) bit;
        };
    };

    loop {
        if frac == 0.0 {
            break;
        };

        frac = frac * 10.0;

        let d = cast(u64) frac;
        if d != 0 {
            print_u64(d);
        };

        frac = frac - cast(f32) d;
    };
}

fn main() {
    print_i64(123456789);
    print_string_literal("\n");

    print_i64(-347);
    print_string_literal("\n");

    print_i64(238947 - 123 + 45 * 34);
    print_string_literal("\n");
    print_i64(240354);
    print_string_literal("\n");

    print_string_literal("\n");

    print_f32(12.625);
    print_string_literal("\n");
    print_f32(3.141592654);
    print_string_literal("\n");
    print_f32(-3.141592654);
    print_string_literal("\n");
}
