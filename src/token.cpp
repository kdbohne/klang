#include "token.h"

const char *token_type_names[]
{
    "ident",
    "num",
    "str",

    "open paren",
    "close paren",
    "open brace",
    "close brace",
    "open bracket",
    "close bracket",

    "semi",
    "comma",
    "dot",
    "dot dot",
    "colon colon",

    "plus",
    "minus",
    "asterisk",
    "slash",
    "eq",
    "percent",

    "plus eq",
    "minus eq",
    "asterisk eq",
    "slash eq",

    "and",
    "not",

    "lt",
    "le",
    "gt",
    "ge",
    "eq eq",
    "ne",

    "r arrow",

    "dollar",

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
    "key import",
    "key module",
    "key return",

    "eof",
    "unknown",
};
