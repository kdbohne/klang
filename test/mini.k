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
        if *p == 0 {
            break;
        };

        len = len + 1;
        p = p + 1;
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

fn thirteen() -> i64 {
    13
}

fn main() {
//    let x = 3 + 4;
//    let y = x + 14;
//    let z = add(x, y);

    write("test string?\n", add(5, 8));
    write("test string?\n", 5 + 8);
    write("test string?\n", thirteen());

/*
    let x = 0;
    foo(&x);
*/
}
