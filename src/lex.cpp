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
    while (token.len > 0)
    {
        if (!(*str) || (*token.str != *str))
            return false;

        --token.len;

        ++token.str;
        ++str;
    }

    // The strings match up to the len of the token, but
    // 'string' could continue on, meaning they do not match.
    return !(*str);
}

static Token get_token(Lexer *lexer)
{
    eat_whitespace(lexer);

    Token token = {};
    token.type = TOK_UNKNOWN;
    token.len = 1;
    token.str = lexer->cursor;

    char c = *lexer->cursor;
    advance(lexer);

    switch (c)
    {
        case '\0': { token.type = TOK_EOF;         break; }
        case '(':  { token.type = TOK_OPEN_PAREN;  break; }
        case ')':  { token.type = TOK_CLOSE_PAREN; break; }
        case '{':  { token.type = TOK_OPEN_BRACE;  break; }
        case '}':  { token.type = TOK_CLOSE_BRACE; break; }
        case ';':  { token.type = TOK_SEMI;        break; }
        case ',':  { token.type = TOK_COMMA;       break; }

        case '+':  { token.type = TOK_PLUS;        break; }
        case '*':  { token.type = TOK_ASTERISK;    break; }
        case '/':  { token.type = TOK_SLASH;       break; }

        case '-': 
        {
            if (*lexer->cursor == '>')
            {
                token.type = TOK_R_ARROW;
                token.len = 2;

                advance(lexer);
            }
            else
            {
                token.type = TOK_MINUS;
            }

            break;
        }

        case ':':
        {
            if (*lexer->cursor == '=')
            {
                token.type = TOK_COLON_EQ;
                token.len = 2;

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

            token.type = TOK_STR;
            token.len = lexer->cursor - token.str - 1;
            ++token.str;

            break;
        }

        default:
        {
            if (is_letter(c) || (c == '_'))
            {
                while (is_letter(*lexer->cursor) || is_number(*lexer->cursor) || (*lexer->cursor == '_'))
                    advance(lexer);

                token.type = TOK_IDENT;
                token.len = lexer->cursor - token.str;

                // TODO: optimize
                if (token_matches(token, "fn"))
                    token.type = TOK_KEY_FN;
                else if (token_matches(token, "extern"))
                    token.type = TOK_KEY_EXTERN;
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

                token.type = TOK_NUM;
                token.len = lexer->cursor - token.str;
            }
            else
            {
                token.type = TOK_UNKNOWN;
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

        if (token.type == TOK_EOF)
            break;
    }

    return tokens;
}
