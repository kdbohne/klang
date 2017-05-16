#include "lex.h"

struct Lexer
{
    char *cursor;

    i32 line;
    i32 column;
};

static bool is_number(char c)
{
    return (c >= '0') && (c <= '9');
}

static bool is_letter(char c)
{
    return ((c >= 'a') && (c <= 'z')) ||
           ((c >= 'A') && (c <= 'Z'));
}

static bool is_newline(char c)
{
    return (c == '\n') || (c == '\r');
}

static bool is_whitespace(char c)
{
    return (c == ' ') || (c == '\t') || is_newline(c);
}

static void advance(Lexer *lexer)
{
    if (is_newline(*lexer->cursor))
    {
        ++lexer->line;
        lexer->column = 0;
    }

    ++lexer->cursor;
    ++lexer->column;
}

static void advance_by(Lexer *lexer, int count)
{
    for (int i = 0; i < count; ++i)
        advance(lexer);
}

static void eat_block_comment(Lexer *lexer)
{
    int depth = 0;

    while (true)
    {
        if ((*lexer->cursor == '/') && (lexer->cursor[1] == '*'))
        {
            ++depth;
            advance_by(lexer, 2);
        }
        else if ((*lexer->cursor == '*') && (lexer->cursor[1] == '/'))
        {
            --depth;
            advance_by(lexer, 2);

            if (depth == 0)
                break;
        }
        else
        {
            advance(lexer);
        }
    }
}

static void eat_whitespace(Lexer *lexer)
{
    while (true)
    {
        if (is_whitespace(*lexer->cursor))
        {
            advance(lexer);
        }
        else if ((*lexer->cursor == '/') && (lexer->cursor[1] == '/'))
        {
            while (!is_newline(*lexer->cursor))
                advance(lexer);

            advance(lexer);
        }
        else if ((*lexer->cursor == '/') && (lexer->cursor[1] == '*'))
        {
            eat_block_comment(lexer);
        }
        else
        {
            break;
        }
    }
}

static bool token_matches(Token token, const char *str)
{
    while (token.length > 0)
    {
        if (!(*str) || (*token.string != *str))
            return false;

        --token.length;

        ++token.string;
        ++str;
    }

    // The strings match up to the length of the token, but
    // 'string' could continue on, meaning they do not match.
    return !(*str);
}

static Token get_token(Lexer *lexer)
{
    eat_whitespace(lexer);

    Token token = {};
    token.type = TOKEN_UNKNOWN;
    token.length = 1;
    token.string = lexer->cursor;

    char c = *lexer->cursor;
    advance(lexer);

    switch (c)
    {
        case '\0': { token.type = TOKEN_EOF;         break; }
        case '(':  { token.type = TOKEN_OPEN_PAREN;  break; }
        case ')':  { token.type = TOKEN_CLOSE_PAREN; break; }
        case '{':  { token.type = TOKEN_OPEN_BRACE;  break; }
        case '}':  { token.type = TOKEN_CLOSE_BRACE; break; }
        case ';':  { token.type = TOKEN_SEMI;        break; }

        case '+':  { token.type = TOKEN_PLUS;        break; }
        case '*':  { token.type = TOKEN_ASTERISK;    break; }
        case '/':  { token.type = TOKEN_SLASH;       break; }

        case '-': 
        {
            if (*lexer->cursor == '>')
            {
                token.type = TOKEN_R_ARROW;
                token.length = 2;

                advance(lexer);
            }
            else
            {
                token.type = TOKEN_MINUS;
            }

            break;
        }

        case ':':
        {
            if (*lexer->cursor == '=')
            {
                token.type = TOKEN_COLON_EQ;
                token.length = 2;

                advance(lexer);
            }
            else
            {
                assert(false);
            }

            break;
        }

        case '"':
        {
            while (*lexer->cursor != '"')
                advance(lexer);

            advance(lexer);

            token.type = TOKEN_STR;
            token.length = lexer->cursor - token.string - 1;
            ++token.string;

            break;
        }

        default:
        {
            if (is_letter(c) || (c == '_'))
            {
                while (is_letter(*lexer->cursor) || is_number(*lexer->cursor) || (*lexer->cursor == '_'))
                    advance(lexer);

                token.type = TOKEN_IDENT;
                token.length = lexer->cursor - token.string;

                if (token_matches(token, "fn"))
                    token.type = TOKEN_KEY_FN;
            }
            else if (is_number(c))
            {
                bool is_float = false;
                while (true)
                {
                    if (!is_number(*lexer->cursor) && (*lexer->cursor != '.'))
                        break;

                    if (*lexer->cursor == '.')
                    {
                        if (is_float)
                            assert(false);

                        is_float = true;
                    }

                    advance(lexer);
                }

                if (is_float)
                {
                    // FIXME: set flag
                }

                token.type = TOKEN_NUM;
                token.length = lexer->cursor - token.string;
            }
            else
            {
                token.type = TOKEN_UNKNOWN;
            }

            break;
        }
    }

    return token;
}

Array<Token> lex_file(char *source)
{
    Array<Token> tokens;

    Lexer lexer;
    lexer.cursor = source;
    lexer.line = 1;
    lexer.column = 1;

    while (true)
    {
        Token token = get_token(&lexer);
        tokens.add(token);

        if (token.type == TOKEN_EOF)
            break;
    }

    return tokens;
}
