import "core.k";
import "string.k";
import "print.k";

fn sum(x i64[]) -> i64 {
    let y = x[0];
    let sum = 0;
    for i in 0..x.count {
        sum += x[i];
    };

    sum
}

fn main() {
    let x i64[5];
    x[0] = 1;
    x[1] = 3;
    x[2] = 5;
    x[3] = 7;
    x[4] = 9;

    let n = 0;
    for i in 0..x.count {
        n += 1;
    };

    print_string_literal("n: ");
    print_i64(n);
    print_string_literal("\n");

    let s = sum(x);
    print_i64(s);
    print_string_literal("\n");

    /*
    let x i64[4];
    x[0] = 0;
    x[1] = 1;
    x[2] = 2;
    x[3] = 3;
    x[4] = 4;
    x[-1] = 5;
    x[-2] = 6;
    x[2342] = 7;

    let y i64[3][4];
    y[0] = x;
    y[0][0] = 0;
    y[0][1] = 1;
    y[0][2] = 2;
    y[1][0] = 3;
    y[1][1] = 4;
    y[1][2] = 5;
    y[2][0] = 6;
    y[2][1] = 7;
    y[2][2] = 8;
    y[3][-1] = 8;
    y[2][4] = 8;
    */
}
