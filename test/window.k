import "core.k";
import "print.k";
import "gl.k";

fn main() {
    let win = gl::create_window("floating", 1280, 720);
    gl::load_funcs();

    print_string_literal("Created window of size ");
    print_i64(win.width);
    print_string_literal("x");
    print_i64(win.height);
    print_string_literal("\n");

    loop {
        gl::swap_buffers(win);
    };

    gl::destroy_window(win);
}
