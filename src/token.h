#pragma once

#include "common.h"

enum TokenType : u32
{
    TOKEN_IDENT,
    TOKEN_NUM,
    TOKEN_STR,

    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_OPEN_BRACE,
    TOKEN_CLOSE_BRACE,

    TOKEN_SEMI,

    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_ASTERISK,
    TOKEN_SLASH,

    TOKEN_COLON_EQ,
    TOKEN_R_ARROW,

    TOKEN_KEY_FN,

    TOKEN_EOF,
    TOKEN_UNKNOWN,
};

struct Token
{
    TokenType type;

    char *string;
    i32 length;
};
