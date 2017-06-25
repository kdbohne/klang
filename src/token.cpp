#include "token.h"

const char *token_type_strings[]
{
    "ident",
    "num",
    "str",

    "open paren",
    "close paren",
    "open brace",
    "close brace",

    "semi",
    "comma",
    "dot",
    "dot dot",

    "plus",
    "minus",
    "asterisk",
    "slash",
    "eq",
    "percent",

    "and",

    "lt",
    "le",
    "gt",
    "ge",
    "eq eq",
//    "colon eq",
    "r arrow",

    "key fn",
    "key extern",
    "key cast",
    "key if",
    "key else",
    "key struct",
    "key let",
    "key loop",
    "key break",
    "key for",
    "key in",
    "key while",

    "eof",
    "unknown",
};
