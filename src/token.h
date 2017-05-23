#pragma once

#include "core/common.h"

enum TokenType : u32
{
    TOK_IDENT,
    TOK_NUM,
    TOK_STR,

    TOK_OPEN_PAREN,
    TOK_CLOSE_PAREN,
    TOK_OPEN_BRACE,
    TOK_CLOSE_BRACE,

    TOK_SEMI,
    TOK_COMMA,

    TOK_PLUS,
    TOK_MINUS,
    TOK_ASTERISK,
    TOK_SLASH,

    TOK_COLON_EQ,
    TOK_R_ARROW,

    TOK_KEY_FN,
    TOK_KEY_EXTERN,

    TOK_EOF,
    TOK_UNKNOWN,
};
extern const char *token_type_strings[];

struct Token
{
    TokenType type = TOK_UNKNOWN;

    char *str = NULL;
    i32 len = 0;
};
