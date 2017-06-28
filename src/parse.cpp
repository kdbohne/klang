#include "parse.h"
#include "core/common.h"
#include "ast.h"

struct Parser
{
    Array<Token> *tokens;
    i32 index;
};

#include <stdio.h>
#include <stdlib.h>
#define report_error(str__, tok__, ...) \
do { \
    fprintf(stderr, "(%s:%d:%d) Parse error: " str__ "\n", tok__.file.path, tok__.line, tok__.col, __VA_ARGS__); \
    fprintf(stderr, "    %.*s\n", tok__.len, tok__.str); \
    fprintf(stderr, "\nExiting.\n"); \
    exit(1); \
} while (0)

static AstExprBlock *parse_block(Parser *parser);

static Token peek(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    return parser->tokens->data[parser->index];
}

static Token next(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    return parser->tokens->data[parser->index++];
}

static Token expect(Parser *parser, TokenType type)
{
    Token tok = next(parser);
    if (tok.type != type)
    {
        report_error("Expected token \"%s\", got \"%s\".",
                     tok,
                     token_type_names[type],
                     token_type_names[tok.type]);
    }

    return tok;
}

static void eat(Parser *parser)
{
    assert(parser->index < parser->tokens->count);
    ++parser->index;
}

static bool eat_optional(Parser *parser, TokenType type)
{
    if (peek(parser).type == type)
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
        case TOK_PERCENT:  return BIN_MOD;
        case TOK_LT:       return BIN_LT;
        case TOK_LE:       return BIN_LE;
        case TOK_GT:       return BIN_GT;
        case TOK_GE:       return BIN_GE;
        case TOK_EQ_EQ:    return BIN_EQ;
        default:
        {
            assert(false);
            return BIN_ERR;
        }
    }
}

static UnOp get_un_op(TokenType type)
{
    switch (type)
    {
        case TOK_AND:      return UN_ADDR;
        case TOK_ASTERISK: return UN_DEREF;
        case TOK_MINUS:    return UN_NEG;
        default:
        {
            assert(false);
            return UN_ERR;
        }
    }
}

static AstExprType *parse_type(Parser *parser)
{
    int pointer_depth = 0;
    while (eat_optional(parser, TOK_ASTERISK))
        ++pointer_depth;

    Token type_tok = expect(parser, TOK_IDENT);

    AstExprType *type = ast_alloc(AstExprType);
    type->name = make_ident(type_tok);
    type->pointer_depth = pointer_depth;

    return type;
}

static AstExpr *parse_expr(Parser *parser, bool is_unary = false)
{
    AstExpr *lhs = NULL;

    Token tok = next(parser);
    switch (tok.type)
    {
        case TOK_IDENT:
        {
            lhs = make_ident(tok);

            // Function call.
            if (peek(parser).type == TOK_OPEN_PAREN)
            {
                eat(parser);

                AstExprCall *call = ast_alloc(AstExprCall);
                call->name = static_cast<AstExprIdent *>(lhs);
                assert(call->name);

                if (peek(parser).type != TOK_CLOSE_PAREN)
                {
                    while (true)
                    {
                        if (peek(parser).type == TOK_CLOSE_PAREN)
                            break;

                        eat_optional(parser, TOK_COMMA);

                        AstExpr *arg = parse_expr(parser);
                        call->args.add(arg);
                    }
                }
                expect(parser, TOK_CLOSE_PAREN);

                lhs = call;
            }

            break;
        }
        case TOK_NUM:
        {
            if (tok.flags & TOKEN_IS_FLOAT)
                lhs = make_lit_float(tok);
            else
                lhs = make_lit_int(tok);

            break;
        }
        case TOK_STR:
        {
            lhs = make_lit_str(tok);
            break;
        }
        case TOK_OPEN_PAREN:
        {
            lhs = parse_expr(parser);
            expect(parser, TOK_CLOSE_PAREN);

            break;
        }
        case TOK_OPEN_BRACE:
        {
            lhs = parse_block(parser);
            break;
        }
        case TOK_KEY_CAST:
        {
            expect(parser, TOK_OPEN_PAREN);
            AstExprType *type = parse_type(parser);
            expect(parser, TOK_CLOSE_PAREN);

            AstExpr *expr = parse_expr(parser);

            AstExprCast *cast = ast_alloc(AstExprCast);
            cast->type = type;
            cast->expr = expr;

            lhs = cast;

            break;
        }
        case TOK_KEY_IF:
        {
            AstExprIf *if_expr = ast_alloc(AstExprIf);
            if_expr->cond = parse_expr(parser);

            expect(parser, TOK_OPEN_BRACE);
            if_expr->block = parse_block(parser);

            if (eat_optional(parser, TOK_KEY_ELSE))
                if_expr->else_expr = parse_expr(parser);

            lhs = if_expr;

            break;
        }
        case TOK_KEY_LOOP:
        {
            AstExprLoop *loop = ast_alloc(AstExprLoop);

            expect(parser, TOK_OPEN_BRACE);
            loop->block = parse_block(parser);

            lhs = loop;

            break;
        }
        case TOK_KEY_BREAK:
        {
            AstExprBreak *brk = ast_alloc(AstExprBreak);
            return brk;
        }
        case TOK_KEY_FOR:
        {
            AstExprFor *for_ = ast_alloc(AstExprFor);

            for_->it = parse_expr(parser);
            expect(parser, TOK_KEY_IN);
            for_->range = parse_expr(parser);

            expect(parser, TOK_OPEN_BRACE);
            for_->block = parse_block(parser);

            lhs = for_;

            break;
        }
        case TOK_KEY_WHILE:
        {
            AstExprWhile *while_ = ast_alloc(AstExprWhile);

            while_->cond = parse_expr(parser);

            expect(parser, TOK_OPEN_BRACE);
            while_->block = parse_block(parser);

            lhs = while_;

            break;
        }

        // Unary operators.
        // Add more here!
        case TOK_AND:
        case TOK_ASTERISK:
        case TOK_MINUS:
        {
            UnOp op = get_un_op(tok.type);
            AstExpr *expr = parse_expr(parser, true);

            if (expr->type == AST_EXPR_LIT)
            {
                auto lit = static_cast<AstExprLit *>(expr);
                lit->value_int.negative = true;
            }

            auto un = ast_alloc(AstExprUn);
            un->op = op;
            un->expr = expr;

            lhs = un;

            break;
        }

        default:
        {
            report_error("Expected lhs expression, got \"%s\"",
                         tok,
                         token_type_names[tok.type]);
            break;
        }
    }

    while (true)
    {
        if (peek(parser).type != TOK_DOT)
            break;

        eat(parser);

        Token ident = expect(parser, TOK_IDENT);

        AstExprField *field = ast_alloc(AstExprField);
        field->expr = lhs;
        field->name = make_ident(ident);

        lhs = field;
    }

    if (is_unary)
        return lhs;

    Token next = peek(parser);
    switch (next.type)
    {
        // Binary operators.
        case TOK_PLUS:
        case TOK_MINUS:
        case TOK_ASTERISK:
        case TOK_SLASH:
        case TOK_PERCENT:
        case TOK_LT:
        case TOK_LE:
        case TOK_GT:
        case TOK_GE:
        case TOK_EQ_EQ:
        {
            eat(parser);

            AstExpr *rhs = parse_expr(parser);
            BinOp op = get_bin_op(next.type);

            auto bin = ast_alloc(AstExprBin);
            bin->lhs = lhs;
            bin->rhs = rhs;
            bin->op = op;

            copy_loc(bin, next);

            return bin;
        }
        case TOK_EQ:
        {
            eat(parser);

            AstExprAssign *assign = ast_alloc(AstExprAssign);
            assign->lhs = lhs;
            assign->rhs = parse_expr(parser);

            copy_loc(assign, next);

            return assign;
        }
        case TOK_DOT_DOT:
        {
            eat(parser);

            AstExprRange *range = ast_alloc(AstExprRange);
            range->start = lhs;
            range->end = parse_expr(parser);

            copy_loc(range, next);

            return range;
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
    if (eat_optional(parser, TOK_KEY_LET))
    {
        // TODO: multiple decls, patterns, etc.
        // NOTE: can't use parse_expr() here, because this case:
        //            let x = 3;
        //       would parse the whole assignment instead of just
        //       the ident (pattern in the future?).
        //
        //       TODO: use parse_expr() anyway, extract the data
        //       from the assignment node?
        Token ident = expect(parser, TOK_IDENT);
        AstExpr *bind = make_ident(ident);

        AstExprType *type = NULL;
        if ((peek(parser).type != TOK_EQ) && (peek(parser).type != TOK_SEMI))
            type = parse_type(parser);

        AstExpr *rhs = NULL;
        if (eat_optional(parser, TOK_EQ))
            rhs = parse_expr(parser);

        AstStmtDecl *decl = ast_alloc(AstStmtDecl);
        decl->bind = bind;
        decl->type = type;
        decl->rhs = rhs;

        expect(parser, TOK_SEMI);

        return decl;
    }

    AstExpr *expr = parse_expr(parser);
    if (eat_optional(parser, TOK_SEMI))
    {
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

static AstExprBlock *parse_block(Parser *parser)
{
    AstExprBlock *block = ast_alloc(AstExprBlock);

    while (true)
    {
        if (peek(parser).type == TOK_CLOSE_BRACE)
            break;

        AstStmt *stmt = parse_stmt(parser);
        block->stmts.add(stmt);
    }
    expect(parser, TOK_CLOSE_BRACE);

    if (block->stmts.count > 0)
    {
        AstStmt *last = block->stmts[block->stmts.count - 1];
        if (last->type == AST_STMT_EXPR)
        {
            AstStmtExpr *se = static_cast<AstStmtExpr *>(last);
            block->expr = se->expr;

            --block->stmts.count;
        }
    }

    return block;
}

static AstExprParam *parse_param(Parser *parser)
{
    Token name_tok = expect(parser, TOK_IDENT);

    AstExprParam *param = ast_alloc(AstExprParam);
    param->name = make_ident(name_tok);
    param->type = parse_type(parser);

    return param;
}

static AstFunc *parse_func(Parser *parser)
{
    AstFunc *func = ast_alloc(AstFunc);

    if (eat_optional(parser, TOK_KEY_EXTERN))
        func->flags |= FUNC_EXTERN;

    expect(parser, TOK_KEY_FN);
    Token ident = expect(parser, TOK_IDENT);
    func->name = make_ident(ident);

    // Parse parameter list.
    expect(parser, TOK_OPEN_PAREN);
    while (true)
    {
        if (peek(parser).type == TOK_CLOSE_PAREN)
            break;

        AstExprParam *param = parse_param(parser);
        func->params.add(param);

        eat_optional(parser, TOK_COMMA);
    }
    expect(parser, TOK_CLOSE_PAREN);

    if (eat_optional(parser, TOK_R_ARROW))
        func->ret = parse_type(parser);

    // Extern functions are just a declaration; there is no body.
    if (func->flags & FUNC_EXTERN)
    {
        expect(parser, TOK_SEMI);
        return func;
    }

    expect(parser, TOK_OPEN_BRACE);
    func->block = parse_block(parser);

    return func;
}

static AstStructField *parse_field(Parser *parser)
{
    Token name_tok = expect(parser, TOK_IDENT);

    AstStructField *field = ast_alloc(AstStructField);
    field->name = make_ident(name_tok);
    field->type = parse_type(parser);

    return field;
}

AstStruct *parse_struct(Parser *parser)
{
    AstStruct *struct_ = ast_alloc(AstStruct);

    expect(parser, TOK_KEY_STRUCT);
    Token ident = expect(parser, TOK_IDENT);
    struct_->name = make_ident(ident);

    expect(parser, TOK_OPEN_BRACE);
    while (true)
    {
        if (peek(parser).type == TOK_CLOSE_BRACE)
            break;

        AstStructField *field = parse_field(parser);
        field->index = struct_->fields.count;
        struct_->fields.add(field);

        expect(parser, TOK_SEMI);
    }
    expect(parser, TOK_CLOSE_BRACE);
    expect(parser, TOK_SEMI);

    return struct_;
}

void parse_file(AstRoot *root, Array<Token> *tokens)
{
    Parser parser;
    parser.tokens = tokens;
    parser.index = 0;

    while (true)
    {
        Token tok = peek(&parser);
        if (tok.type == TOK_EOF)
            break;

        if ((tok.type == TOK_KEY_FN) || (tok.type == TOK_KEY_EXTERN))
        {
            AstFunc *func = parse_func(&parser);
            root->funcs.add(func);
        }
        else if (tok.type == TOK_KEY_STRUCT)
        {
            AstStruct *struct_ = parse_struct(&parser);
            root->structs.add(struct_);
        }
        else
        {
            report_error("Invalid top-level token of type \"%s\". Only structs and functions are allowed.",
                         tok,
                         token_type_names[tok.type]);
        }
    }
}
