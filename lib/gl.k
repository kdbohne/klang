struct Input {
    keys u8[256];

    mouse_buttons u8[8];
    mouse_down_positions i32[8][2];

    mouse_position i32[2];
}

extern fn platform_create_window(title *u8, width i64, height i64) -> *c_void;
extern fn platform_destroy_window(window *c_void);
extern fn platform_swap_buffers(window *c_void);
extern fn platform_poll_events(window *c_void, input *Input);

module gl {
    struct Window {
        width i64;
        height i64;
    }

    let COLOR_BUFFER_BIT u32 = 0x00004000;

    let Clear fn(u32);
    let ClearColor fn(f32, f32, f32, f32);
    let Viewport fn(i32, i32, i32, i32);
    let UseProgram fn(u32);
    let CreateProgram fn(void) -> u32;
    let LinkProgram fn(u32);
    let CreateShader fn(u32) -> u32;
    let ShaderSource fn(u32, i32, **i8, *i32);
    let CompileShader fn(u32);
    let GetShaderiv fn(u32, u32, *i32);
    let GetShaderInfoLog fn(u32, i32, *i32, *i8);
    let AttachShader fn(u32, u32);
    let DetachShader fn(u32, u32);
    let DeleteShader fn(u32);
    let GetProgramiv fn(u32, u32, *i32);
    let DrawArrays fn(u32, i32, i32);
    let DrawElements fn(u32, i32, u32, *c_void);
    let Enable fn(u32);
    let Disable fn(u32);
    let GenVertexArrays fn(i32, *u32);
    let BindVertexArray fn(u32);
    let GenBuffers fn(i32, *u32);
    let BindBuffer fn(u32, u32);
    let BufferData fn(u32, i64, *c_void, u32);
    let BufferSubData fn(u32, i64, i64, *c_void);
    let EnableVertexAttribArray fn(u32);
    let VertexAttribPointer fn(u32, i32, u32, u8, i32, *c_void);
    let GetActiveUniform fn(u32, u32, i32, *i32, *i32, *u32, *i8) -> i32;
    let GetUniformLocation fn(u32, *i8) -> i32;
    let Uniform1i fn(i32, i32);
    let Uniform1f fn(i32, f32);
    let Uniform3f fn(i32, f32, f32, f32);
    let UniformMatrix4fv fn(i32, i32, u8, *f32);
    let PolygonMode fn(u32, u32);
    let GenTextures fn(i32, *u32);
    let BindTexture fn(u32, u32);
    let TexParameteri fn(u32, u32, i32);
    let TexParameteriv fn(u32, u32, *i32);
    let TexImage2D fn(u32, i32, i32, i32, i32, i32, u32, u32, *c_void);
    let ActiveTexture fn(u32);
    let GenFramebuffers fn(i32, *u32);
    let FramebufferTexture2D fn(u32, u32, u32, u32, i32);
    let CheckFramebufferStatus fn(u32) -> u32;
    let BlendFunc fn(u32, u32);
    let CullFace fn(u32);
    let StencilMask fn(u32);
    let StencilFunc fn(u32, i32, u32);
    let StencilOp fn(u32, u32, u32);
    let StencilOpSeparate fn(u32, u32, u32, u32);
    let BindFramebuffer fn(u32, u32);
    let ColorMask fn(u8, u8, u8, u8);
    let DepthMask fn(u8);

    fn create_window(title *u8, width i64, height i64) -> *Window {
        let win = cast(*Window) platform_create_window(title, width, height);
        win
    }

    fn destroy_window(win *Window) {
        platform_destroy_window(cast(*c_void) win);
    }

    fn swap_buffers(win *Window) {
        platform_swap_buffers(cast(*c_void) win);
    }

    fn load_funcs() {
        let dll = dll_load("libGL.so.1");
        if !dll {
            print_string_literal("Failed to load libGL.\n");
            return;
        };

        Clear = cast(fn(u32)) dll_get_symbol(dll, "glClear");
        ClearColor = cast(fn(f32, f32, f32, f32)) dll_get_symbol(dll, "glClearColor");
        Viewport = cast(fn(i32, i32, i32, i32)) dll_get_symbol(dll, "glViewport");
        UseProgram = cast(fn(u32)) dll_get_symbol(dll, "glUseProgram");
        CreateProgram = cast(fn(void) -> u32) dll_get_symbol(dll, "glCreateProgram");
        LinkProgram = cast(fn(u32)) dll_get_symbol(dll, "glLinkProgram");
        CreateShader = cast(fn(u32) -> u32) dll_get_symbol(dll, "glCreateShader");
        ShaderSource = cast(fn(u32, i32, **i8, *i32)) dll_get_symbol(dll, "glShaderSource");
        CompileShader = cast(fn(u32)) dll_get_symbol(dll, "glCompileShader");
        GetShaderiv = cast(fn(u32, u32, *i32)) dll_get_symbol(dll, "glGetShaderiv");
        GetShaderInfoLog = cast(fn(u32, i32, *i32, *i8)) dll_get_symbol(dll, "glGetShaderInfoLog");
        AttachShader = cast(fn(u32, u32)) dll_get_symbol(dll, "glAttachShader");
        DetachShader = cast(fn(u32, u32)) dll_get_symbol(dll, "glDetachShader");
        DeleteShader = cast(fn(u32)) dll_get_symbol(dll, "glDeleteShader");
        GetProgramiv = cast(fn(u32, u32, *i32)) dll_get_symbol(dll, "glGetProgramiv");
        DrawArrays = cast(fn(u32, i32, i32)) dll_get_symbol(dll, "glDrawArrays");
        DrawElements = cast(fn(u32, i32, u32, *c_void)) dll_get_symbol(dll, "glDrawElements");
        Enable = cast(fn(u32)) dll_get_symbol(dll, "glEnable");
        Disable = cast(fn(u32)) dll_get_symbol(dll, "glDisable");
        GenVertexArrays = cast(fn(i32, *u32)) dll_get_symbol(dll, "glGenVertexArrays");
        BindVertexArray = cast(fn(u32)) dll_get_symbol(dll, "glBindVertexArray");
        GenBuffers = cast(fn(i32, *u32)) dll_get_symbol(dll, "glGenBuffers");
        BindBuffer = cast(fn(u32, u32)) dll_get_symbol(dll, "glBindBuffer");
        BufferData = cast(fn(u32, i64, *c_void, u32)) dll_get_symbol(dll, "glBufferData");
        BufferSubData = cast(fn(u32, i64, i64, *c_void)) dll_get_symbol(dll, "glBufferSubData");
        EnableVertexAttribArray = cast(fn(u32)) dll_get_symbol(dll, "glEnableVertexAttribArray");
        VertexAttribPointer = cast(fn(u32, i32, u32, u8, i32, *c_void)) dll_get_symbol(dll, "glVertexAttribPointer");
        GetActiveUniform = cast(fn(u32, u32, i32, *i32, *i32, *u32, *i8) -> i32) dll_get_symbol(dll, "glGetActiveUniform");
        GetUniformLocation = cast(fn(u32, *i8) -> i32) dll_get_symbol(dll, "glGetUniformLocation");
        Uniform1i = cast(fn(i32, i32)) dll_get_symbol(dll, "glUniform1i");
        Uniform1f = cast(fn(i32, f32)) dll_get_symbol(dll, "glUniform1f");
        Uniform3f = cast(fn(i32, f32, f32, f32)) dll_get_symbol(dll, "glUniform3f");
        UniformMatrix4fv = cast(fn(i32, i32, u8, *f32)) dll_get_symbol(dll, "glUniformMatrix4fv");
        PolygonMode = cast(fn(u32, u32)) dll_get_symbol(dll, "glPolygonMode");
        GenTextures = cast(fn(i32, *u32)) dll_get_symbol(dll, "glGenTextures");
        BindTexture = cast(fn(u32, u32)) dll_get_symbol(dll, "glBindTexture");
        TexParameteri = cast(fn(u32, u32, i32)) dll_get_symbol(dll, "glTexParameteri");
        TexParameteriv = cast(fn(u32, u32, *i32)) dll_get_symbol(dll, "glTexParameteriv");
        TexImage2D = cast(fn(u32, i32, i32, i32, i32, i32, u32, u32, *c_void)) dll_get_symbol(dll, "glTexImage2D");
        ActiveTexture = cast(fn(u32)) dll_get_symbol(dll, "glActiveTexture");
        GenFramebuffers = cast(fn(i32, *u32)) dll_get_symbol(dll, "glGenFramebuffers");
        FramebufferTexture2D = cast(fn(u32, u32, u32, u32, i32)) dll_get_symbol(dll, "glFramebufferTexture2D");
        CheckFramebufferStatus = cast(fn(u32) -> u32) dll_get_symbol(dll, "glCheckFramebufferStatus");
        BlendFunc = cast(fn(u32, u32)) dll_get_symbol(dll, "glBlendFunc");
        CullFace = cast(fn(u32)) dll_get_symbol(dll, "glCullFace");
        StencilMask = cast(fn(u32)) dll_get_symbol(dll, "glStencilMask");
        StencilFunc = cast(fn(u32, i32, u32)) dll_get_symbol(dll, "glStencilFunc");
        StencilOp = cast(fn(u32, u32, u32)) dll_get_symbol(dll, "glStencilOp");
        StencilOpSeparate = cast(fn(u32, u32, u32, u32)) dll_get_symbol(dll, "glStencilOpSeparate");
        BindFramebuffer = cast(fn(u32, u32)) dll_get_symbol(dll, "glBindFramebuffer");
        ColorMask = cast(fn(u8, u8, u8, u8)) dll_get_symbol(dll, "glColorMask");
        DepthMask = cast(fn(u8)) dll_get_symbol(dll, "glDepthMask");

        // TODO: check for null function pointers

        dll_unload(dll);
    }
}
