import "core.k";
import "string.k";
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

    gl::ClearColor(1.0, 0.0, 1.0, 1.0);
    gl::Clear(gl::COLOR_BUFFER_BIT);

    let input Input;

    loop {
        gl::poll_events(win, &input);
        for i in 0..256 {
            if input.keys[i] {
                print_i64(i);
                print_string_literal("\n");
            };
        };

        gl::Clear(gl::COLOR_BUFFER_BIT);
        gl::swap_buffers(win);
    };

    gl::destroy_window(win);
}
