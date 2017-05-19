#include "parse.h"
#include "core/common.h"
#include "ast.h"

struct Parser
{
    Array<Token> *tokens;
    i32 index;
};

static TokenType peek(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    return parser->tokens->data[parser->index].type;
}

static Token next(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    return parser->tokens->data[parser->index++];
}

static Token expect(Parser *parser, TokenType type)
{
    Token token = next(parser);
    if (token.type != type)
    {
        assert(false);
    }

    return token;
}

static void eat(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    ++parser->index;
}

static bool eat_optional(Parser *parser, TokenType type)
{
    if (peek(parser) == type)
    {
        eat(parser);
        return true;
    }

    return false;
}

static BinOp get_bin_op(TokenType type)
{
    switch (type)
    {
        case TOK_PLUS:     return BIN_ADD;
        case TOK_MINUS:    return BIN_SUB;
        case TOK_ASTERISK: return BIN_MUL;
        case TOK_SLASH:    return BIN_DIV;
        default:
        {
            assert(false);
            return (BinOp)0;
        }
    }
}

static AstExpr *parse_expr(Parser *parser)
{
    AstExpr *lhs = NULL;

    Token tok = next(parser);
    switch (tok.type)
    {
        case TOK_IDENT:
        {
            lhs = make_ident(tok);
            break;
        }
        case TOK_NUM:
        {
            // FIXME: check for int vs float
            lhs = make_lit_int(tok);
//            lhs = make_lit_float(tok);

            break;
        }
        case TOK_STR:
        {
            lhs = make_lit_str(tok);
            break;
        }
        case TOK_OPEN_PAREN:
        {
            eat(parser);
            lhs = parse_expr(parser);
            expect(parser, TOK_CLOSE_PAREN);

            break;
        }
        default:
        {
            assert(false);
            break;
        }
    }

    TokenType next = peek(parser);
    switch (next)
    {
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_ASTERISK:
        case TOK_SLASH:
        {
            eat(parser);

            AstExpr *rhs = parse_expr(parser);
            BinOp op = get_bin_op(next);

            return make_bin(lhs, rhs, op);
        }
        case TOK_OPEN_PAREN:
        {
            eat(parser);

            AstFuncCall *call = ast_alloc(AstFuncCall);
            call->name = static_cast<AstIdent *>(lhs);
            assert(call->name);

            if (peek(parser) != TOK_CLOSE_PAREN)
            {
                while (true)
                {
                    if (peek(parser) == TOK_CLOSE_PAREN)
                        break;

                    eat_optional(parser, TOK_COMMA);

                    AstExpr *arg = parse_expr(parser);
                    call->args.add(arg);
                }
            }
            expect(parser, TOK_CLOSE_PAREN);

            break;
        }
        default:
        {
            break;
        }
    }

    return lhs;
}

static AstStmt *parse_stmt(Parser *parser)
{
    AstExpr *expr = parse_expr(parser);

    if (peek(parser) == TOK_COLON_EQ)
    {
        eat(parser);

        AstStmtDecl *decl = ast_alloc(AstStmtDecl);
        decl->lhs = expr;
        decl->rhs = parse_expr(parser);

        expect(parser, TOK_SEMI);

        return decl;
    }

    if (peek(parser) == TOK_SEMI)
    {
        eat(parser);

        AstStmtSemi *stmt = ast_alloc(AstStmtSemi);
        stmt->expr = expr;
        
        return stmt;
    }
    else
    {
        AstStmtExpr *stmt = ast_alloc(AstStmtExpr);
        stmt->expr = expr;

        return stmt;
    }
}

static AstBlock *parse_block(Parser *parser)
{
    AstBlock *block = ast_alloc(AstBlock);

    expect(parser, TOK_OPEN_BRACE);
    while (true)
    {
        if (peek(parser) == TOK_CLOSE_BRACE)
            break;

        AstStmt *stmt = parse_stmt(parser);

        if (AstStmtExpr *se = static_cast<AstStmtExpr *>(stmt))
            block->expr = se->expr;
        else
            block->stmts.add(stmt);
    }
    expect(parser, TOK_CLOSE_BRACE);

    return block;
}

static AstFunc *parse_func(Parser *parser)
{
    AstFunc *func = ast_alloc(AstFunc);

    expect(parser, TOK_KEY_FN);
    Token ident = expect(parser, TOK_IDENT);

    // Parse parameter list.
    expect(parser, TOK_OPEN_PAREN);
    while (true)
    {
        if (peek(parser) == TOK_CLOSE_PAREN)
            break;

        Token name_tok = next(parser);
        Token type_tok = next(parser);

        AstIdent *name = make_ident(name_tok);
        AstIdent *type = make_ident(type_tok);
        func->params.add(name);
        func->params.add(type);

        eat_optional(parser, TOK_COMMA);
    }
    expect(parser, TOK_CLOSE_PAREN);

    if (eat_optional(parser, TOK_R_ARROW))
    {
        Token ret = next(parser);
        func->ret = make_ident(ret);
    }

    func->block = parse_block(parser);

    return func;
}

void parse_file(AstRoot *root, Array<Token> *tokens)
{
    Parser parser;
    parser.tokens = tokens;
    parser.index = 0;

    while (true)
    {
        if (peek(&parser) == TOK_EOF)
            break;

        AstFunc *func = parse_func(&parser);
        root->funcs.add(func);
    }
}
