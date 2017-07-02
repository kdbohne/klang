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
    TOK_DOT,
    TOK_DOT_DOT,

    TOK_PLUS,
    TOK_MINUS,
    TOK_ASTERISK,
    TOK_SLASH,
    TOK_EQ,
    TOK_PERCENT,

    TOK_AND,

    TOK_LT,
    TOK_LE,
    TOK_GT,
    TOK_GE,
    TOK_EQ_EQ,
    TOK_NE,

    TOK_R_ARROW,

    TOK_KEY_FN,
    TOK_KEY_EXTERN,
    TOK_KEY_CAST,
    TOK_KEY_IF,
    TOK_KEY_ELSE,
    TOK_KEY_STRUCT,
    TOK_KEY_LET,
    TOK_KEY_LOOP,
    TOK_KEY_BREAK,
    TOK_KEY_FOR,
    TOK_KEY_IN,
    TOK_KEY_WHILE,

    TOK_EOF,
    TOK_UNKNOWN,
};
extern const char *token_type_names[];

enum TokenFlags
{
    TOKEN_IS_FLOAT = 0x1,
    TOKEN_IS_HEX = 0x2,
};

struct Token
{
    u32 flags = 0;

    TokenType type = TOK_UNKNOWN;

    char *str = NULL;
    i32 len = 0;

    // Location info for error reporting.
    File file;
    i32 line = 0;
    i32 col = 0;
};
