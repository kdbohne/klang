extern fn syscall5(num *i64, arg1 *i64, arg2 *i64, arg3 *i64, arg4 *i64, arg5 *i64) -> i64;

fn add(a i64, b i64) -> i64 {
    a + b
}

fn write(data *u8, len i64) {
    syscall5(cast(*i64) 1, cast(*i64) 1, cast(*i64) data, cast(*i64) len, cast(*i64) 0, cast(*i64) 0);
}

/*
fn string_len(str *u8) -> i64 {
    let len = 0;

    let p = str;
    loop {
    /*
        if *p == 0 {
            break;
        };

        len = len + 1;
        p = p + 1;
    */
    };

    len
}

fn print_string_literal(str *u8) {
    let len = string_len(str);
    write(str, len);
}
*/

/*
fn foo(ptr *i64) {
    *ptr = 42;
}
*/

fn main() {
//    print_string_literal("this is a test string!\n");
    let x = 0;
    let y = 0;

    write("test string!\n", 13);

    if x {
        y = 3;
    } else {
        y = 4;
    };

    if x == 0 {
        x = 1;
    } else {
        x = 2;
    };

    let z = if x == 2 {
        6
    } else {
        7
    };

    let a = 0;
    let b = 0;
    if a == 0 {
        b = 1;
    } else if a == 1 {
        b = 2;
    } else if a == 2 {
        b = 3;
    } else {
        b = 4;
    };

/*
    let x = 0;
    foo(&x);
*/
}
