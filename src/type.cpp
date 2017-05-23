#include "type.h"
#include "core/string.h"
#include "ast.h"

// TODO: size?
TypeDefn global_type_defns[512];
i32 global_type_defns_count;

void register_type_defn(const char *name)
{
    assert(global_type_defns_count < (i32)(sizeof(global_type_defns) / sizeof(global_type_defns[0])));

    if (get_type_defn(name) != NULL)
    {
        // TODO: error message
        assert(false);
        return;
    }

    TypeDefn *defn = &global_type_defns[global_type_defns_count++];
    defn->name = string_duplicate(name);
}

TypeDefn *get_type_defn(const char *name)
{
    // TODO: optimize if needed
    for (int i = 0; i < global_type_defns_count; ++i)
    {
        TypeDefn *defn = &global_type_defns[i];
        if (strings_match(defn->name, name))
            return defn;
    }

    return NULL;
}

bool type_check(AstRoot *root)
{
    register_type_defn("i32");
    register_type_defn("i64");
    register_type_defn("f32");
    register_type_defn("f64");
    register_type_defn("string");

    return true;
}
