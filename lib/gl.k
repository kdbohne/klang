extern fn gl_create_window(title *u8, width i64, height i64) -> *c_void;
extern fn gl_destroy_window(window *c_void);
extern fn gl_swap_buffers(window *c_void);

module gl {
    struct Window {
        width i64;
        height i64;
    };

    let COLOR_BUFFER_BIT u32 = 0x00004000;

    let Clear fn(u32);
    let ClearColor fn(f32, f32, f32, f32);

    fn create_window(title *u8, width i64, height i64) -> *Window {
        let win = cast(*Window) gl_create_window(title, width, height);
        win
    }

    fn destroy_window(win *Window) {
        gl_destroy_window(cast(*c_void) win);
    }

    fn swap_buffers(win *Window) {
        gl_swap_buffers(cast(*c_void) win);
    }

    fn load_funcs() {
        let dll = dll_load("libGL.so.1");
        if !dll {
            print_string_literal("Failed to load libGL.\n");
            return;
        };

        print_string_literal("Loading function pointers...\n");

        Clear = cast(fn(u32)) dll_get_symbol(dll, "glClear");
        ClearColor = cast(fn(f32, f32, f32, f32)) dll_get_symbol(dll, "glClearColor");

        // TODO: check for null function pointers

        dll_unload(dll);
    }
}
