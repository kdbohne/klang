#include "type.h"
#include "ast.h"

enum TypeKind : u32
{
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_STR,

    TYPE_PTR,

    TYPE_STRUCT,
    TYPE_FN,

    TYPE_NULL,
};

// TODO: this clashes with IntType in ast.h. Is this okay, or should one of
// them be renamed?
/*
enum IntType : u32
{
    INT_I8,
    INT_I16,
    INT_I32,
    INT_I64,
    INT_U8,
    INT_U16,
    INT_U32,
    INT_U64,
};
*/

struct TypeInt
{
    IntType type;
};

enum FloatType : u32
{
    FLOAT_F32,
    FLOAT_F64,
};

struct TypeFloat
{
    FloatType type;
};

struct TypeStr
{
    // FIXME
    i64 dummy;
};

struct TypePtr
{
    i64 id;
    i64 depth;
};

struct TypeStruct
{
    // FIXME
    i64 dummy;
};

struct TypeFn
{
    // FIXME
    i64 dummy;
};

struct Type
{
    TypeKind kind;

    union
    {
        TypeInt int_;
        TypeFloat float_;
        TypeStr str;
        TypePtr ptr;
        TypeStruct struct_;
        TypeFn fn;
    };
};

static Array<Type> global_types;

static i64 type_i8;
static i64 type_i16;
static i64 type_i32;
static i64 type_i64;
static i64 type_u8;
static i64 type_u16;
static i64 type_u32;
static i64 type_u64;
static i64 type_f32;
static i64 type_f64;
static i64 type_void;
static i64 type_c_void;

static i64 make_int(IntType type)
{
    i64 id = global_types.count;

    Type *t = global_types.next();
    t->kind = TYPE_INT;
    t->int_.type = type;

    return id;
}

static i64 make_float(FloatType type)
{
    i64 id = global_types.count;

    Type *t = global_types.next();
    t->kind = TYPE_FLOAT;
    t->float_.type = type;

    return id;
}

static i64 make_void()
{
    i64 id = global_types.count;

    Type *t = global_types.next();
    t->kind = TYPE_NULL;

    return id;
}

static i64 make_ptr(i64 type_id, i64 depth)
{
    i64 id = global_types.count;

    Type *t = global_types.next();
    t->kind = TYPE_PTR;
    t->ptr.id = type_id;
    t->ptr.depth = depth;

    return id;
}

bool type_check(AstRoot *ast)
{
    type_i8 = make_int(INT_I8);
    type_i16 = make_int(INT_I16);
    type_i32 = make_int(INT_I32);
    type_i64 = make_int(INT_I64);
    type_u8 = make_int(INT_U8);
    type_u16 = make_int(INT_U16);
    type_u32 = make_int(INT_U32);
    type_u64 = make_int(INT_U64);
    type_f32 = make_float(FLOAT_F32);
    type_f64 = make_float(FLOAT_F64);
    type_void = make_void();
    type_c_void = make_ptr(type_void, 1);

    return true;
}
