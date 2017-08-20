fn public_test(x i64) -> i64 {
    x * 2
}

module test {
    struct Thing {
        x i64;
        y i64;
    };

    fn make_thing(x i64, y i64) -> Thing {
        let t Thing;
        t.x = x;
        t.y = y;
        t
    }

    fn do_something(t *Thing) -> i64 {
        t.x + t.y
    }

    module nest {
        struct Bar {
            z f32;
            t Thing;
        };

        fn make_bar(z f32, t Thing) -> Bar {
            let b Bar;
            b.z = z;
            b.t = t;
            b
        }
    }
}