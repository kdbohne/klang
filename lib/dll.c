#include "core.h"
#include "dll.h"

#include <dlfcn.h>

void *dll_load(char *path) {
    // TODO: flags? RTLD_LAZY might be useful for some cases
    void *dll = dlopen(path, RTLD_NOW);
    if (!dll) {
        char *message = dlerror();
        fprintf(stderr, "Failed to load DLL at %s:\n    %s\n", path, message);

        free(message);

        return NULL;
    }

    return dll;
}

void dll_unload(void *dll) {
    assert(dll);

    i32 error = dlclose(dll);

    // TODO: print name/path
    // TODO: dlerror
    if (error != 0) {
        fprintf(stderr, "Failed to unload DLL.\n");
    }
}

void *dll_get_symbol(void *dll, char *symbol) {
    assert(dll);

    void *address = dlsym(dll, symbol);
    if (!address) {
        fprintf(stderr, "Failed to get symbol: \"%s\"\n", symbol);
        return NULL;
    }

    return address;
}
