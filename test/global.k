import "core.k";
import "print.k";

let gg i64;
let gh i64;
let gi i64;
let gj i64;
let gk i64;

fn test() {
    gg = 2;
}

fn main() {
    let x = 3;
    test();
    let y = x + gg;

    gh = x;
    gj = y + gh;

    print_i64(gj);
    print_string_literal("\n");
}
