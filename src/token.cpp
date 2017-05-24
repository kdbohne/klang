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

    "plus",
    "minus",
    "asterisk",
    "slash",

    "and",

    "colon eq",
    "r arrow",

    "key fn",
    "key extern",
    "key cast",

    "eof",
    "unknown",
};
