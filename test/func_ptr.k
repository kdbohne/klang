import "core.k";
import "print.k";

fn foo(x i64) -> i64 {
    x * 2
}

fn bar(x i64) -> i64 {
    x + 5
}

fn baz() {
    // empty
}

fn test(s *u8) {
    print_string_literal("[test] ");
    print_string_literal(s);
}

fn main() {
    let f fn(*u8);

    for i in 0..10 {
        f = if i % 2 {
            test
        } else {
            print_string_literal
        };

        f("this is a string\n");
    };

//    let v fn();
//    v = baz;

//    let g fn(i64) -> i64 = bar;

//    let h = bar;
}
