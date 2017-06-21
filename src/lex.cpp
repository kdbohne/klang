#include "lex.h"

struct Lexer
{
    char *cur;

    File file;
    i32 line;
    i32 col;
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

static void advance(Lexer *lex)
{
    if (is_newline(*lex->cur))
    {
        ++lex->line;
        lex->col = 0;
    }

    ++lex->cur;
    ++lex->col;
}

static void advance_by(Lexer *lex, int count)
{
    for (int i = 0; i < count; ++i)
        advance(lex);
}

static void eat_block_comment(Lexer *lex)
{
    int depth = 0;

    while (true)
    {
        if ((*lex->cur == '/') && (lex->cur[1] == '*'))
        {
            ++depth;
            advance_by(lex, 2);
        }
        else if ((*lex->cur == '*') && (lex->cur[1] == '/'))
        {
            --depth;
            advance_by(lex, 2);

            if (depth == 0)
                break;
        }
        else
        {
            advance(lex);
        }
    }
}

static void eat_whitespace(Lexer *lex)
{
    while (true)
    {
        if (is_whitespace(*lex->cur))
        {
            advance(lex);
        }
        else if ((*lex->cur == '/') && (lex->cur[1] == '/'))
        {
            while (!is_newline(*lex->cur))
                advance(lex);

            advance(lex);
        }
        else if ((*lex->cur == '/') && (lex->cur[1] == '*'))
        {
            eat_block_comment(lex);
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

static Token get_token(Lexer *lex)
{
    eat_whitespace(lex);

    Token tok = {};
    tok.type = TOK_UNKNOWN;
    tok.len = 1;
    tok.str = lex->cur;

    tok.file = lex->file;
    tok.line = lex->line;
    tok.col = lex->col;

    char c = *lex->cur;
    advance(lex);

    switch (c)
    {
        case '\0': { tok.type = TOK_EOF;         break; }
        case '(':  { tok.type = TOK_OPEN_PAREN;  break; }
        case ')':  { tok.type = TOK_CLOSE_PAREN; break; }
        case '{':  { tok.type = TOK_OPEN_BRACE;  break; }
        case '}':  { tok.type = TOK_CLOSE_BRACE; break; }
        case ';':  { tok.type = TOK_SEMI;        break; }
        case ',':  { tok.type = TOK_COMMA;       break; }
        case '.':  { tok.type = TOK_DOT;         break; }

        case '&':  { tok.type = TOK_AND;         break; }

        case '+':  { tok.type = TOK_PLUS;        break; }
        case '*':  { tok.type = TOK_ASTERISK;    break; }
        case '/':  { tok.type = TOK_SLASH;       break; }

        case '<':
        {
            if (*lex->cur == '=')
            {
                tok.type = TOK_LE;
                tok.len = 2;

                advance(lex);
            }
            else
            {
                tok.type = TOK_LT;
            }

            break;
        }
        case '>':
        {
            if (*lex->cur == '=')
            {
                tok.type = TOK_GE;
                tok.len = 2;

                advance(lex);
            }
            else
            {
                tok.type = TOK_GT;
            }

            break;
        }

        case '=':
        {
            if (*lex->cur == '=')
            {
                tok.type = TOK_EQ_EQ;
                tok.len = 2;

                advance(lex);
            }
            else
            {
                tok.type = TOK_EQ;
            }

            break;
        }

        case '-':
        {
            if (*lex->cur == '>')
            {
                tok.type = TOK_R_ARROW;
                tok.len = 2;

                advance(lex);
            }
            else
            {
                tok.type = TOK_MINUS;
            }

            break;
        }

#if 0
        case ':':
        {
            if (*lex->cur == '=')
            {
                tok.type = TOK_COLON_EQ;
                tok.len = 2;

                advance(lex);
            }
            else
            {
                assert(false);
            }

            break;
        }
#endif

        case '"':
        {
            tok.type = TOK_STR;

            int escape_count = 0;
            while (*lex->cur != '"')
            {
                if (*lex->cur == '\\')
                    ++escape_count;

                advance(lex);
            }
            advance(lex);

            int raw_len = lex->cur - tok.str - 2;
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
                while (is_letter(*lex->cur) || is_number(*lex->cur) || (*lex->cur == '_'))
                    advance(lex);

                tok.type = TOK_IDENT;
                tok.len = lex->cur - tok.str;

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
                else if (token_matches(tok, "let"))
                    tok.type = TOK_KEY_LET;
                else if (token_matches(tok, "loop"))
                    tok.type = TOK_KEY_LOOP;
                else if (token_matches(tok, "break"))
                    tok.type = TOK_KEY_BREAK;
            }
            else if (is_number(c))
            {
                bool is_float = false;
                while (true)
                {
                    if (!is_number(*lex->cur) && (*lex->cur != '.'))
                        break;

                    if (*lex->cur == '.')
                    {
                        if (is_float)
                            assert(false);

                        is_float = true;
                    }

                    advance(lex);
                }

                if (is_float)
                    tok.flags |= TOKEN_IS_FLOAT;

                tok.type = TOK_NUM;
                tok.len = lex->cur - tok.str;
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

Array<Token> lex_file(File file)
{
    Array<Token> tokens;

    Lexer lexer;
    lexer.cur = file.src;
    lexer.file = file;
    lexer.line = 1;
    lexer.col = 1;

    while (true)
    {
        Token tok = get_token(&lexer);
        tokens.add(tok);

        if (tok.type == TOK_EOF)
            break;
    }

    return tokens;
}
