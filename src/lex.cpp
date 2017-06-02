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

static bool token_matches(Token tok, const char *str)
{
    while (tok.len > 0)
    {
        if (!(*str) || (*tok.str != *str))
            return false;

        --tok.len;

        ++tok.str;
        ++str;
    }

    // The strings match up to the len of the token, but
    // 'string' could continue on, meaning they do not match.
    return !(*str);
}

static Token get_token(Lexer *lexer)
{
    eat_whitespace(lexer);

    Token tok = {};
    tok.type = TOK_UNKNOWN;
    tok.len = 1;
    tok.str = lexer->cursor;

    char c = *lexer->cursor;
    advance(lexer);

    switch (c)
    {
        case '\0': { tok.type = TOK_EOF;         break; }
        case '(':  { tok.type = TOK_OPEN_PAREN;  break; }
        case ')':  { tok.type = TOK_CLOSE_PAREN; break; }
        case '{':  { tok.type = TOK_OPEN_BRACE;  break; }
        case '}':  { tok.type = TOK_CLOSE_BRACE; break; }
        case ';':  { tok.type = TOK_SEMI;        break; }
        case ',':  { tok.type = TOK_COMMA;       break; }

        case '&':  { tok.type = TOK_AND;         break; }

        case '+':  { tok.type = TOK_PLUS;        break; }
        case '*':  { tok.type = TOK_ASTERISK;    break; }
        case '/':  { tok.type = TOK_SLASH;       break; }

        case '=':
        {
            if (*lexer->cursor == '=')
            {
                tok.type = TOK_EQ_EQ;
                tok.len = 2;

                advance(lexer);
            }
            else
            {
                tok.type = TOK_EQ;
            }

            break;
        }

        case '-':
        {
            if (*lexer->cursor == '>')
            {
                tok.type = TOK_R_ARROW;
                tok.len = 2;

                advance(lexer);
            }
            else
            {
                tok.type = TOK_MINUS;
            }

            break;
        }

        case ':':
        {
            if (*lexer->cursor == '=')
            {
                tok.type = TOK_COLON_EQ;
                tok.len = 2;

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
            tok.type = TOK_STR;

            int escape_count = 0;
            while (*lexer->cursor != '"')
            {
                if (*lexer->cursor == '\\')
                    ++escape_count;

                advance(lexer);
            }
            advance(lexer);

            int raw_len = lexer->cursor - tok.str - 2;
            char *raw_str = tok.str + 1;

            // TODO: leak, free after parsing
            tok.len = raw_len - escape_count;
            tok.str = (char *)malloc(tok.len + 1);

            char *str = tok.str;
            for (int i = 0; i < raw_len; ++i)
            {
                if ((i < raw_len - 1) && (raw_str[i] == '\\'))
                {
                    char e = raw_str[i + 1];
                    switch (e)
                    {
                        case 'n': { *str++ = '\n'; break; }
                        case 'r': { *str++ = '\r'; break; }
                        case 't': { *str++ = '\t'; break; }
                        default:
                        {
                            assert(false);
                            break;
                        }
                    }

                    ++i;
                }
                else
                {
                    *str++ = raw_str[i];
                }
            }

            tok.str[tok.len] = '\0';

            break;
        }

        default:
        {
            if (is_letter(c) || (c == '_'))
            {
                while (is_letter(*lexer->cursor) || is_number(*lexer->cursor) || (*lexer->cursor == '_'))
                    advance(lexer);

                tok.type = TOK_IDENT;
                tok.len = lexer->cursor - tok.str;

                // TODO: optimize
                if (token_matches(tok, "fn"))
                    tok.type = TOK_KEY_FN;
                else if (token_matches(tok, "extern"))
                    tok.type = TOK_KEY_EXTERN;
                else if (token_matches(tok, "cast"))
                    tok.type = TOK_KEY_CAST;
                else if (token_matches(tok, "if"))
                    tok.type = TOK_KEY_IF;
                else if (token_matches(tok, "else"))
                    tok.type = TOK_KEY_ELSE;
                else if (token_matches(tok, "struct"))
                    tok.type = TOK_KEY_STRUCT;
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
                    tok.flags |= TOKEN_IS_FLOAT;

                tok.type = TOK_NUM;
                tok.len = lexer->cursor - tok.str;
            }
            else
            {
                tok.type = TOK_UNKNOWN;
            }

            break;
        }
    }

    return tok;
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
        Token tok = get_token(&lexer);
        tokens.add(tok);

        if (tok.type == TOK_EOF)
            break;
    }

    return tokens;
}
