#include "core.h"
#include "dll.h"

#include <locale.h>
#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <GL/glx.h>

typedef struct {
    // User-level interface.
    i64 width;
    i64 height;
    bool closed;

    // Internal members.
    Display *xlib_display;
    Window xlib_window;
    Colormap xlib_colormap;
    XIM xlib_input_method;
    Atom xlib_wm_delete_message;

    GLXContext glx_context;

    bool xkb_supported;
} GlWindow;

typedef GLXContext (*glXCreateContextAttribsARBProc)(Display *, GLXFBConfig, GLXContext, Bool, const int *);
static glXCreateContextAttribsARBProc glXCreateContextAttribsARB;

static const char *glx_extension_list;

typedef enum {
    KEY_UNKNOWN       =  -1,

    KEY_ESCAPE        =   0,
    KEY_ENTER         =   1,
    KEY_TAB           =   2,
    KEY_BACKSPACE     =   3,
    KEY_INSERT        =   4,
    KEY_DELETE        =   5,
    KEY_RIGHT         =   6,
    KEY_LEFT          =   7,
    KEY_DOWN          =   8,
    KEY_UP            =   9,
    KEY_PAGE_UP       =  10,
    KEY_PAGE_DOWN     =  11,
    KEY_HOME          =  12,
    KEY_END           =  13,
    KEY_CAPS_LOCK     =  14,
    KEY_SCROLL_LOCK   =  15,
    KEY_NUM_LOCK      =  16,
    KEY_PRINT_SCREEN  =  17,
    KEY_PAUSE         =  18,
    KEY_LEFT_SHIFT    =  19,
    KEY_LEFT_CONTROL  =  20,
    KEY_LEFT_ALT      =  21,
    KEY_LEFT_SUPER    =  22,
    KEY_RIGHT_SHIFT   =  23,
    KEY_RIGHT_CONTROL =  24,
    KEY_RIGHT_ALT     =  25,
    KEY_RIGHT_SUPER   =  26,

    // Printable ASCII values
    KEY_SPACE         =  32,
    KEY_APOSTROPHE    =  39,
    KEY_COMMA         =  44,
    KEY_MINUS         =  45,
    KEY_PERIOD        =  46,
    KEY_SLASH         =  47,
    KEY_0             =  48,
    KEY_1             =  49,
    KEY_2             =  50,
    KEY_3             =  51,
    KEY_4             =  52,
    KEY_5             =  53,
    KEY_6             =  54,
    KEY_7             =  55,
    KEY_8             =  56,
    KEY_9             =  57,
    KEY_SEMICOLON     =  59,
    KEY_EQUALS        =  61,
    KEY_A             =  65,
    KEY_B             =  66,
    KEY_C             =  67,
    KEY_D             =  68,
    KEY_E             =  69,
    KEY_F             =  70,
    KEY_G             =  71,
    KEY_H             =  72,
    KEY_I             =  73,
    KEY_J             =  74,
    KEY_K             =  75,
    KEY_L             =  76,
    KEY_M             =  77,
    KEY_N             =  78,
    KEY_O             =  79,
    KEY_P             =  80,
    KEY_Q             =  81,
    KEY_R             =  82,
    KEY_S             =  83,
    KEY_T             =  84,
    KEY_U             =  85,
    KEY_V             =  86,
    KEY_W             =  87,
    KEY_X             =  88,
    KEY_Y             =  89,
    KEY_Z             =  90,
    KEY_LEFT_BRACKET  =  91,
    KEY_BACKSLASH     =  92,
    KEY_RIGHT_BRACKET =  93,
    KEY_GRAVE         =  96,

    KEY_F1            =  97,
    KEY_F2            =  98,
    KEY_F3            =  99,
    KEY_F4            = 100,
    KEY_F5            = 101,
    KEY_F6            = 102,
    KEY_F7            = 103,
    KEY_F8            = 104,
    KEY_F9            = 105,
    KEY_F10           = 106,
    KEY_F11           = 107,
    KEY_F12           = 108,

    // TODO: keypad

    // NOTE: the maximum keycode value cannot exceed the length Input.keys
    // array! (256 as of 14 Oct 2016)
} Keycode;

typedef enum {
    MOUSE_LEFT   = 1,
    MOUSE_RIGHT  = 2,
    MOUSE_MIDDLE = 3,
} MouseButton;

typedef struct {
    u8 keys[256];

    // @TODO: how many?
    u8 mouse_buttons[8];
    i32 mouse_down_positions[8][2];

    i32 mouse_position[2];
} Input;

// TODO: this is just copied from the OpenGL wiki (right?); rewrite it
#include <string.h>
static bool is_extension_supported(const char *extension) {
    assert(glx_extension_list);

    /* Extension names should not have spaces. */
    const char *where = strchr(extension, ' ');
    if (where || *extension == '\0') {
        return false;
    }

    const char *start = glx_extension_list;

    while (true) {
        where = strstr(start, extension);

        if (!where) {
            break;
        }

        const char *terminator = where + strlen(extension);

        if (where == start || *(where - 1) == ' ') {
            if (*terminator == ' ' || *terminator == '\0') {
                return true;
            }
        }

        start = terminator;
    }

    return false;
}

static void *gl_get_symbol(char *symbol)
{
    void *libgl = dll_load("libGL.so.1");
    if (!libgl)
        return NULL;

    // TODO: dll_unload?

    // TODO: use dlsym as fallback instead of just failing
    PFNGLXGETPROCADDRESSPROC glx_get_proc_address = (PFNGLXGETPROCADDRESSPROC)dll_get_symbol(libgl, "glXGetProcAddressARB");
    if (!glx_get_proc_address)
    {
        fprintf(stderr, "Error: Failed to load glXGetProcAddressARB.\n");
        return NULL;
    }

    void *function = (void *)glx_get_proc_address((const GLubyte *)symbol);
    if (!function)
    {
        fprintf(stderr, "Error: Failed to load OpenGL symbol: \"%s\"\n", symbol);
        return NULL;
    }

    return function;
}

static void debug_message_callback(u32 source, u32 type, u32 id, u32 severity, i32 length, const char *message, const void *user_param)
{
    if (severity == GL_DEBUG_SEVERITY_NOTIFICATION)
        return;

#if 0
    const char *severity_string = ((severity == GL_DEBUG_SEVERITY_LOW) ? "low" :
                                   (severity == GL_DEBUG_SEVERITY_MEDIUM) ? "medium" :
                                   (severity == GL_DEBUG_SEVERITY_HIGH) ? "high" :
                                   (severity == GL_DEBUG_SEVERITY_NOTIFICATION) ? "notification" :
                                   "unknown");
#endif

    fprintf(stderr, "%s\n", message);
    assert(severity != GL_DEBUG_SEVERITY_HIGH);
}

static int dummy_error_handler(Display *display, XErrorEvent *event) {
//    fprintf(stderr, "Error: dummy_error_handler: event type %u\n", event->type);
    return 0;
}

static GLXContext create_glx_context(Display *xlib_display, Window *xlib_window, GLXFBConfig *fb_cfg) {
    // TODO: check for other libGL names?
    void *libgl = dll_load("libGL.so.1");
    if (!libgl) {
        return NULL;
    }

    PFNGLXGETPROCADDRESSPROC glx_get_proc_address = (PFNGLXGETPROCADDRESSPROC)dll_get_symbol(libgl, "glXGetProcAddressARB");
    if (!glx_get_proc_address) {
        fprintf(stderr, "Error: Failed to load glXGetProcAddressARB.\n");
        return NULL;
    }

    dll_unload(libgl);

    glx_extension_list = glXQueryExtensionsString(xlib_display, DefaultScreen(xlib_display));
//    fprintf(stderr, "Error: %s\n", glx_extension_list);

    // Change the xlib error handler to avoid terminating immediately when encountering an error.
    XErrorHandler old_handler = XSetErrorHandler(&dummy_error_handler);

    // Attempt to use GLX_ARB_create_context for control over the OpenGL context version.
    glXCreateContextAttribsARB = (glXCreateContextAttribsARBProc)glx_get_proc_address((const GLubyte *)"glXCreateContextAttribsARB");

    // Create a fallback OpenGL context if 3.0+ is not supported.
    // TODO: test this?
    if (!is_extension_supported("GLX_ARB_create_context") || !glXCreateContextAttribsARB) {
#if 1
        GLXContext glx_context = glXCreateNewContext(xlib_display, *fb_cfg, GLX_RGBA_TYPE, 0, True);
        if (!glx_context) {
            fprintf(stderr, "Error: Failed to create fallback OpenGL context.\n");
            return NULL;
        }

        return glx_context;
#else
        fprintf(stderr, "Error: GLX_ARB_create_context is not supported.\n");
        return false;
#endif
    }

    static i32 glx_context_attribs[] = {
        GLX_CONTEXT_MAJOR_VERSION_ARB, 3,
        GLX_CONTEXT_MINOR_VERSION_ARB, 3,
        GLX_CONTEXT_FLAGS_ARB,         GLX_CONTEXT_DEBUG_BIT_ARB | GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB,
        GLX_CONTEXT_PROFILE_MASK_ARB,  GLX_CONTEXT_CORE_PROFILE_BIT_ARB,
        None
    };

    // Create the 3.3 Core context.
    GLXContext glx_context = glXCreateContextAttribsARB(xlib_display, *fb_cfg, NULL, True, glx_context_attribs);

    // Flush all errors.
    XSync(xlib_display, False);

    // Restore the original error handler.
    XSetErrorHandler(old_handler);

    if (!glXIsDirect(xlib_display, glx_context)) {
        fprintf(stderr, "Error: Received indirect GLX context.\n");
        return NULL;
    }

    // TODO: glXMakeContextCurrent vs glXMakeCurrent
//    if (!glXMakeCurrent(xlib_display, *xlib_window, *glx_context)) {
    if (!glXMakeContextCurrent(xlib_display, *xlib_window, *xlib_window, glx_context)) {
        fprintf(stderr, "Error: Failed to make GLX context current.\n");
        return NULL;
    }

    return glx_context;
}

void *platform_create_window(char *title, i64 width, i64 height) {
    Display *xlib_display = XOpenDisplay(NULL);
    if (!xlib_display) {
        fprintf(stderr, "Error: XOpenDisplay failed.\n");
        return NULL;
    }

    // HACK: taken from glfw/xlib_init.c: _glfwPlatformInit()
    if (strcmp(setlocale(LC_CTYPE, NULL), "C") == 0) {
        setlocale(LC_CTYPE, "");
    }

    XIM xlib_input_method = NULL;
    if (XSupportsLocale()) {
        XSetLocaleModifiers("");

        // TODO: check for usable IM style, like GLFW does?
        xlib_input_method = XOpenIM(xlib_display, 0, NULL, NULL);

        if (!xlib_input_method) {
            XCloseIM(xlib_input_method);
        }
    }

    i32 glx_major, glx_minor;
    if (!glXQueryVersion(xlib_display, &glx_major, &glx_minor) || (glx_major < 1) || (glx_minor < 3)) {
        fprintf(stderr, "Error: glXQueryVersion failed: %d.%d\n", glx_major, glx_minor);
        return NULL;
    }

    const i32 target_samples = 0;

    static i32 visual_attribs[] = {
        GLX_X_RENDERABLE, True,
        GLX_RENDER_TYPE, GLX_RGBA_BIT,
        GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
        GLX_X_VISUAL_TYPE, GLX_TRUE_COLOR,

        GLX_DOUBLEBUFFER, True,
        GLX_STEREO, False,

        GLX_LEVEL, 0,
        GLX_AUX_BUFFERS, 0,

//        GLX_FBCONFIG_ID, GLX_DONT_CARE,
//        GLX_BUFFER_SIZE, ?,

        GLX_RED_SIZE, 8,
        GLX_GREEN_SIZE, 8,
        GLX_BLUE_SIZE, 8,
        GLX_ALPHA_SIZE, 8,
        GLX_DEPTH_SIZE, 24,
        GLX_STENCIL_SIZE, 8,

        GLX_SAMPLES, target_samples,
//        GLX_SAMPLE_BUFFERS, 0,

        None
    };

    // Generate a list of suitable framebuffer configs from the visual_attribs list.
    i32 fb_cfg_count;
    GLXFBConfig *fb_cfgs = glXChooseFBConfig(xlib_display, DefaultScreen(xlib_display), visual_attribs, &fb_cfg_count);
    if (!fb_cfgs) {
        fprintf(stderr, "Error: Failed to find suitable framebuffer config.\n");
        return NULL;
    }

    // Find the best matching framebuffer config.
    i32 best_fb_cfg_index = -1;
//    i32 max_samples = -1;
    for (i32 i = 0; i < fb_cfg_count; ++i) {
        XVisualInfo *visual_info = glXGetVisualFromFBConfig(xlib_display, fb_cfgs[i]);
        if (visual_info) {
            i32 samples, sample_buffers;
            glXGetFBConfigAttrib(xlib_display, fb_cfgs[i], GLX_SAMPLES, &samples);
            glXGetFBConfigAttrib(xlib_display, fb_cfgs[i], GLX_SAMPLE_BUFFERS, &sample_buffers);

//            print("fbconfig %d: visual ID 0x%2x: SAMPLE_BUFFERS = %d, SAMPLES = %d\n", i, visual_info->visualid, sample_buffers, samples);

            if (samples <= target_samples) {
                best_fb_cfg_index = i;
                XFree(visual_info);
                break;
            }
#if 0
            if ((sample_buffers != 0) && (samples > max_samples)) {
                best_fb_cfg_index = i;
                max_samples = samples;
            }
#endif
        }

        XFree(visual_info);
    }

    if (best_fb_cfg_index < 0) {
        fprintf(stderr, "Error: Warning: no matching framebuffer config found; using fallback config.\n");
        best_fb_cfg_index = 0;
    }

    GLXFBConfig best_fb_cfg = fb_cfgs[best_fb_cfg_index];
    XFree(fb_cfgs);

    XVisualInfo *visual_info = glXGetVisualFromFBConfig(xlib_display, best_fb_cfg);
    Window root_win = RootWindow(xlib_display, visual_info->screen);

    Colormap xlib_colormap = XCreateColormap(xlib_display, root_win, visual_info->visual, AllocNone);

    XSetWindowAttributes window_attribs;
    window_attribs.colormap          = xlib_colormap;
    window_attribs.background_pixmap = None;
    window_attribs.border_pixel      = 0;
    window_attribs.event_mask        = StructureNotifyMask |
                                       EnterWindowMask |
                                       LeaveWindowMask |
                                       FocusChangeMask |
                                       ExposureMask |
                                       PointerMotionMask |
                                       ButtonPressMask |
                                       ButtonReleaseMask |
                                       OwnerGrabButtonMask |
                                       KeyPressMask |
                                       KeyReleaseMask;

    // Create the Xlib window.
    Window xlib_window = XCreateWindow(xlib_display, root_win,
                                       0, 0, width, height, 0, visual_info->depth, InputOutput,
                                       visual_info->visual,
                                       CWColormap | CWBorderPixel | CWEventMask, &window_attribs);

    XFree(visual_info);

    if (!xlib_window) {
        fprintf(stderr, "Error: Failed to create Xlib window.\n");
        return NULL;
    }

    XMapWindow(xlib_display, xlib_window);

    // Set window name.
    XClassHint *hint = XAllocClassHint();
    hint->res_name = (char *)title;
    hint->res_class = (char *)title;

    XSetClassHint(xlib_display, xlib_window, hint);
    XFree(hint);

//    XStoreName(xlib_display, xlib_window, title);

#ifdef X_HAVE_UTF8_STRING
    Xutf8SetWMProperties(xlib_display, xlib_window,
                         title, title,
                         NULL, 0,
                         NULL, NULL, NULL);
#else
    XmbSetWMProperties(xlib_display, xlib_window,
                       title, title,
                       NULL, 0,
                       NULL, NULL, NULL);
#endif

    XChangeProperty(xlib_display, xlib_window,
                    XInternAtom(xlib_display, "_NET_WM_NAME", False),
                    XInternAtom(xlib_display, "UTF8_STRING", False), 8,
                    PropModeReplace,
                    (unsigned char *)title, strlen(title));

    XChangeProperty(xlib_display, xlib_window,
                    XInternAtom(xlib_display, "_NET_WM_ICON_NAME", False),
                    XInternAtom(xlib_display, "UTF8_STRING", False), 8,
                    PropModeReplace,
                    (unsigned char *)title, strlen(title));

    // Register a listener for WM_DELETE_WINDOW messages.
    Atom xlib_wm_delete_message = XInternAtom(xlib_display, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(xlib_display, xlib_window, &xlib_wm_delete_message, 1);

#if 0
    // Create a blank cursor for use with capture mouse mode.
    char blank_data[8] = {0};
    XColor black_color = {0};
    Pixmap blank_pixmap = XCreateBitmapFromData(xlib_display, xlib_window, blank_data, 8, 8);
    Cursor xlib_blank_cursor = XCreatePixmapCursor(xlib_display, blank_pixmap, blank_pixmap, &black_color, &black_color, 0, 0);
    XFreePixmap(xlib_display, blank_pixmap);
#endif

    // Ensure that all Xlib work is finished before continuing.
    XFlush(xlib_display);

    i32 xkb_major = 1;
    i32 xkb_minor = 0;
    i32 xkb_major_opcode, xkb_event_base, xkb_error_base;
    Bool xkb_supported = XkbQueryExtension(xlib_display, &xkb_major_opcode, &xkb_event_base, &xkb_error_base, &xkb_major, &xkb_minor);
    if (xkb_supported) {
        // TODO: XkbSetDetectableAutoRepeat?
    }

    // Create glx context.
    GLXContext glx_context = create_glx_context(xlib_display, &xlib_window, &best_fb_cfg);
    if (!glx_context) {
        fprintf(stderr, "Error: Failed to create GLX context.\n");
        return NULL;
    }

    PFNGLDEBUGMESSAGECALLBACKARBPROC glDebugMessageCallbackARB = (PFNGLDEBUGMESSAGECALLBACKARBPROC)gl_get_symbol("glDebugMessageCallbackARB");
    if (glDebugMessageCallbackARB) {
        glDebugMessageCallbackARB((GLDEBUGPROCARB)&debug_message_callback, NULL);
    }

    GlWindow *win = malloc(sizeof(GlWindow));
    win->width = width;
    win->height = height;

    win->xlib_display = xlib_display;
    win->xlib_window = xlib_window;
    win->xlib_colormap = xlib_colormap;
    win->xlib_input_method = xlib_input_method;
    win->xlib_wm_delete_message = xlib_wm_delete_message;

    win->glx_context = glx_context;

    win->xkb_supported = xkb_supported;

    return (void *)win;
}

void platform_destroy_window(void *window)
{
    GlWindow *win = (GlWindow *)window;

    // TODO

    free(win);
}

void platform_swap_buffers(void *window)
{
    GlWindow *win = (GlWindow *)window;
    glXSwapBuffers(win->xlib_display, win->xlib_window);
}

static i32 get_keycode(void *window, u32 keycode)
{
    GlWindow *win = (GlWindow *)window;

    i32 symbol;
    if (win->xkb_supported) {
        // TODO: handle different shift levels (4th parameter)
        symbol = XkbKeycodeToKeysym(win->xlib_display, keycode, 0, 0);
    }
    else
    {
        i32 dummy;
        KeySym *symbols = XGetKeyboardMapping(win->xlib_display, keycode, 1, &dummy);
        symbol = symbols[0];

        // TODO: avoid doing this allocation every time?
        XFree(symbols);

#if 0
        error("Xkb 1.0 is not supported.\n");
        assert(false);
#endif
    }

    switch (symbol) {
        case XK_Escape:       return KEY_ESCAPE;
        case XK_Return:       return KEY_ENTER;
        case XK_Tab:          return KEY_TAB;
        case XK_BackSpace:    return KEY_BACKSPACE;
        case XK_Insert:       return KEY_INSERT;
        case XK_Delete:       return KEY_DELETE;
        case XK_Right:        return KEY_RIGHT;
        case XK_Left:         return KEY_LEFT;
        case XK_Down:         return KEY_DOWN;
        case XK_Up:           return KEY_UP;
        case XK_Page_Up:      return KEY_PAGE_UP;
        case XK_Page_Down:    return KEY_PAGE_DOWN;
        case XK_Home:         return KEY_HOME;
        case XK_End:          return KEY_END;
        case XK_Caps_Lock:    return KEY_CAPS_LOCK;
        case XK_Scroll_Lock:  return KEY_SCROLL_LOCK;
        case XK_Num_Lock:     return KEY_NUM_LOCK;
        case XK_Print:        return KEY_PRINT_SCREEN;
        case XK_Pause:        return KEY_PAUSE;
        case XK_Shift_L:      return KEY_LEFT_SHIFT;
        case XK_Control_L:    return KEY_LEFT_CONTROL;
        case XK_Alt_L:        return KEY_LEFT_ALT;
        case XK_Super_L:      return KEY_LEFT_SUPER;
        case XK_Shift_R:      return KEY_RIGHT_SHIFT;
        case XK_Control_R:    return KEY_RIGHT_CONTROL;
        case XK_Alt_R:        return KEY_RIGHT_ALT;
        case XK_Super_R:      return KEY_RIGHT_SUPER;

        case XK_space:        return KEY_SPACE;
        case XK_apostrophe:   return KEY_APOSTROPHE;
        case XK_comma:        return KEY_COMMA;
        case XK_minus:        return KEY_MINUS;
        case XK_period:       return KEY_PERIOD;
        case XK_slash:        return KEY_SLASH;
        case XK_0:            return KEY_0;
        case XK_1:            return KEY_1;
        case XK_2:            return KEY_2;
        case XK_3:            return KEY_3;
        case XK_4:            return KEY_4;
        case XK_5:            return KEY_5;
        case XK_6:            return KEY_6;
        case XK_7:            return KEY_7;
        case XK_8:            return KEY_8;
        case XK_9:            return KEY_9;
        case XK_semicolon:    return KEY_SEMICOLON;
        case XK_equal:        return KEY_EQUALS;
        case XK_a:            return KEY_A;
        case XK_b:            return KEY_B;
        case XK_c:            return KEY_C;
        case XK_d:            return KEY_D;
        case XK_e:            return KEY_E;
        case XK_f:            return KEY_F;
        case XK_g:            return KEY_G;
        case XK_h:            return KEY_H;
        case XK_i:            return KEY_I;
        case XK_j:            return KEY_J;
        case XK_k:            return KEY_K;
        case XK_l:            return KEY_L;
        case XK_m:            return KEY_M;
        case XK_n:            return KEY_N;
        case XK_o:            return KEY_O;
        case XK_p:            return KEY_P;
        case XK_q:            return KEY_Q;
        case XK_r:            return KEY_R;
        case XK_s:            return KEY_S;
        case XK_t:            return KEY_T;
        case XK_u:            return KEY_U;
        case XK_v:            return KEY_V;
        case XK_w:            return KEY_W;
        case XK_x:            return KEY_X;
        case XK_y:            return KEY_Y;
        case XK_z:            return KEY_Z;
        case XK_bracketleft:  return KEY_LEFT_BRACKET;
        case XK_backslash:    return KEY_BACKSLASH;
        case XK_bracketright: return KEY_RIGHT_BRACKET;
        case XK_grave:        return KEY_GRAVE;

        case XK_F1:           return KEY_F1;
        case XK_F2:           return KEY_F2;
        case XK_F3:           return KEY_F3;
        case XK_F4:           return KEY_F4;
        case XK_F5:           return KEY_F5;
        case XK_F6:           return KEY_F6;
        case XK_F7:           return KEY_F7;
        case XK_F8:           return KEY_F8;
        case XK_F9:           return KEY_F9;
        case XK_F10:          return KEY_F10;
        case XK_F11:          return KEY_F11;
        case XK_F12:          return KEY_F12;

        default:              break;
    }

    fprintf(stderr, "Error: Converting invalid keycode: %u\n", keycode);
    return KEY_UNKNOWN;
}

void platform_poll_events(void *window, Input *input)
{
    GlWindow *win = (GlWindow *)window;

    i32 event_count = XPending(win->xlib_display);
    if (event_count <= 0) {
        return;
    }

    XEvent event;
    for (i32 i = 0; i < event_count; ++i) {
        XNextEvent(win->xlib_display, &event);

        if (XFilterEvent(&event, win->xlib_window)) {
            continue;
        }

        switch (event.type) {
            case ClientMessage: {
                if ((Atom)event.xclient.data.l[0] == win->xlib_wm_delete_message) {
                    win->closed = true;
                }

                break;
            }
            case KeyPress: {
                i32 keycode = get_keycode(win, event.xkey.keycode);
//                print("KeyPress: %d\n", keycode);

                if (keycode != KEY_UNKNOWN) {
                    input->keys[keycode] |= 0x1;
                }

                break;
            }
            case KeyRelease: {
                i32 keycode = get_keycode(win, event.xkey.keycode);
//                print("KeyRelease: %u\n", keycode);

                if (keycode != KEY_UNKNOWN) {
                    input->keys[keycode] &= ~0x1;
                }

                break;
            }
            case ButtonPress: {
                switch (event.xbutton.button) {
                    // TODO: reduce duplication here
                    case Button1: {
                        input->mouse_buttons[MOUSE_LEFT] |= 0x1;
                        input->mouse_down_positions[MOUSE_LEFT][0] = event.xbutton.x;
                        input->mouse_down_positions[MOUSE_LEFT][1] = event.xbutton.y;
                        break;
                    }
                    case Button2: {
                        input->mouse_buttons[MOUSE_MIDDLE] |= 0x1;
                        input->mouse_down_positions[MOUSE_MIDDLE][0] = event.xbutton.x;
                        input->mouse_down_positions[MOUSE_MIDDLE][1] = event.xbutton.y;
                        break;
                    }
                    case Button3: {
                        input->mouse_buttons[MOUSE_RIGHT] |= 0x1;
                        input->mouse_down_positions[MOUSE_RIGHT][0] = event.xbutton.x;
                        input->mouse_down_positions[MOUSE_RIGHT][1] = event.xbutton.y;
                        break;
                    }
                    case Button4: {
                        // FIXME
//                        input->mouse_scroll_delta.y += 1.0f;
                        break;
                    }
                    case Button5: {
                        // FIXME
//                        input->mouse_scroll_delta.y -= 1.0f;
                        break;
                    }
#if 0
                    // TODO: Button6/Button7 are undeclared identifiers; fix if needed?
                    case Button6: {
                        input->mouse_scroll_delta.x += 1.0f;
                        break;
                    }
                    case Button7: {
                        input->mouse_scroll_delta.x -= 1.0f;
                        break;
                    }
#endif
                    default: {
                        break;
                    }
                }

                break;
            }
            case ButtonRelease: {
                // TODO: this is copied and pasted from the ButtonPress case above.
                switch (event.xbutton.button) {
                    // TODO: reduce duplication here
                    case Button1: {
                        input->mouse_buttons[MOUSE_LEFT] &= ~0x1;
                        input->mouse_down_positions[MOUSE_LEFT][0] = 0;
                        input->mouse_down_positions[MOUSE_LEFT][1] = 0;
                        break;
                    }
                    case Button2: {
                        input->mouse_buttons[MOUSE_MIDDLE] &= ~0x1;
                        input->mouse_down_positions[MOUSE_MIDDLE][0] = 0;
                        input->mouse_down_positions[MOUSE_MIDDLE][1] = 0;
                        break;
                    }
                    case Button3: {
                        input->mouse_buttons[MOUSE_RIGHT] &= ~0x1;
                        input->mouse_down_positions[MOUSE_RIGHT][0] = 0;
                        input->mouse_down_positions[MOUSE_RIGHT][1] = 0;
                        break;
                    }
                    default: {
                        break;
                    }
                }
                break;
            }
            case MotionNotify: {
                input->mouse_position[0] = event.xmotion.x;
                input->mouse_position[1] = event.xmotion.y;
#if 0
                if (win->should_capture_cursor) {
                    // Ignore XWarpPointer events.
                    i32 warp_x = win->width / 2;
                    i32 warp_y = win->height / 2;
                    if ((event.xmotion.x == warp_x) && (event.xmotion.y == warp_y)) {
                        break;
                    }

                    vec2 delta = vec2_make(event.xmotion.x, event.xmotion.y) - vec2_make(warp_x, warp_y);
                    input->mouse_delta += delta;
                    input->mouse_position += delta;
                } else {
                    vec2 old_position = input->mouse_position;
                    input->mouse_position = vec2_make(event.xmotion.x, event.xmotion.y);
                    input->mouse_delta += input->mouse_position - old_position;
                }
#endif

                break;
            }
            case FocusIn: {
#if 0
                win->focused = true;
                if (win->should_capture_cursor) {
                    capture_cursor_internal(win);
                }
#endif
                break;
            }
            case FocusOut: {
#if 0
                win->focused = false;
                release_cursor_internal(win);
#endif
                break;
            }
            default: {
                break;
            }
        }
    }
}
