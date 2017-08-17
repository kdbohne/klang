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

    // @TODO: use dlsym as fallback instead of just failing
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

void *gl_create_window(char *title, i64 width, i64 height) {
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

void gl_destroy_window(void *window)
{
    GlWindow *win = (GlWindow *)window;

    // TODO

    free(win);
}

void gl_swap_buffers(void *window)
{
    GlWindow *win = (GlWindow *)window;
    glXSwapBuffers(win->xlib_display, win->xlib_window);
}
