extern fn malloc(bytes u64) -> *c_void;
extern fn free(mem *c_void);

extern fn bit_and(a u64, b u64) -> u64;
extern fn bit_or(a u64, b u64) -> u64;
extern fn bit_shift_left(x u64, amount u64) -> u64;
extern fn bit_shift_right(x u64, amount u64) -> u64;

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