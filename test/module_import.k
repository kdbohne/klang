import "module.k";

fn main() {
    let x = 4;

    let t0 test::Thing;
    t0.x = 3;
    t0.y = 5;

    let t1 = test::make_thing(t0.x, t0.y + 1);

    let z = test::do_something(&t0);
    let w = z + test::do_something(&t1);

    let a = test::nest::make_bar(3.14, t0);
    let b = a.z + 6.9105;

    let c = public_test(8);
}