import "core.k";
import "print.k";
import "gl.k";

fn main() {
    let win = gl::create_window("floating", 1280, 720);
    gl::load_funcs();

    loop {
        gl::swap_buffers(win);
    };

    gl::destroy_window(win);
}