module string {
    struct String {
        data *u8;
        len i64;
    }

    fn len(str *u8) -> i64 {
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

    fn from_raw(str *u8) -> String {
        let s String;
        s.len = len(str);
        s.data = cast(*u8) malloc(cast(u64) (s.len + 1));

        let p = s.data;
        for i in 0..s.len {
            *p = *str;

            p = p + 1;
            str = str + 1;
        };

        s
    }
}
