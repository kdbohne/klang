import "core.k";
import "print.k";

struct Vec3 {
    x f32;
    y f32;
    z f32;
};

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
