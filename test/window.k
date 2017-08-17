extern fn malloc(bytes u64) -> *c_void;
extern fn gl_create_window(title *u8, width i64, height i64) -> *c_void;
extern fn gl_destroy_window(window *c_void);
extern fn gl_swap_buffers(window *c_void);

struct GlWindow {
    width i64;
    height i64;
};

fn main() {
    let win = cast(*GlWindow) gl_create_window("floating", 1280, 720);

    loop {
        gl_swap_buffers(cast(*c_void) win);
    };

    gl_destroy_window(cast(*c_void) win);
}