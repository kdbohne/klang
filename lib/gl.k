extern fn gl_create_window(title *u8, width i64, height i64) -> *c_void;
extern fn gl_destroy_window(window *c_void);
extern fn gl_swap_buffers(window *c_void);

module gl {
    struct Window {
        width i64;
        height i64;
    };

    struct Funcs {
        Clear fn(u32);
    };

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

        dll_unload(dll);
    }
}
