#include "type.h"
#include "core/string.h"
#include "ast.h"

// TODO: size?
TypeDefn global_type_defns[512];
i32 global_type_defns_count;

void register_type_defn(const char *name)
{
    assert(global_type_defns_count < (i32)(sizeof(global_type_defns) / sizeof(global_type_defns[0])));

    TypeDefn *defn = &global_type_defns[global_type_defns_count++];
    defn->name = string_duplicate(name);
}

bool type_check(AstRoot *root)
{
    return true;
}
