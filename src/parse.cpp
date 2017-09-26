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

#define report_error_no_arg(str__, tok__) report_error(str__"%s", tok__, "")

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
        case TOK_NE:       return BIN_NE;
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
        case TOK_NOT:      return UN_NOT;
        default:
        {
            assert(false);
            return UN_ERR;
        }
    }
}

static AstExpr *parse_expr(Parser *parser, bool is_unary = false);

// Two cases:
//     1) Single identifier
//     2) Identifiers separated by double-colons
static AstExpr *parse_ident_or_path(Parser *parser)
{
    Token tok = expect(parser, TOK_IDENT);
    if (peek(parser).type != TOK_COLON_COLON)
    {
        // Just a single identifier; no path.
        AstExprIdent *ident = make_ident(tok);
        return ident;
    }

    AstExprPath *path = ast_alloc(AstExprPath);
    path->segments.add(make_ident(tok));

    while (peek(parser).type == TOK_COLON_COLON)
    {
        eat(parser);
        tok = expect(parser, TOK_IDENT);

        AstExprIdent *seg = make_ident(tok);
        path->segments.add(seg);
    }

    return path;
}

static AstType *parse_type(Parser *parser)
{
    // Function pointer.
    if (eat_optional(parser, TOK_KEY_FN))
    {
        AstType *type = ast_alloc(AstType);
        type->is_func_ptr = true;

        expect(parser, TOK_OPEN_PAREN);
        while (peek(parser).type != TOK_CLOSE_PAREN)
        {
            AstType *param = parse_type(parser);
            type->params.add(param);

            if (peek(parser).type != TOK_CLOSE_PAREN)
                expect(parser, TOK_COMMA);
        }
        expect(parser, TOK_CLOSE_PAREN);

        if (eat_optional(parser, TOK_R_ARROW))
            type->ret = parse_type(parser);

        return type;
    }

    int ptr_depth = 0;
    while (eat_optional(parser, TOK_ASTERISK))
        ++ptr_depth;

    AstType *type = ast_alloc(AstType);
    type->expr = parse_ident_or_path(parser);
    type->ptr_depth = ptr_depth;

    return type;
}

static AstExprBin *parse_cond(Parser *parser)
{
    assert(parser->index > 0);
    Token tok = parser->tokens->data[parser->index - 1];

    // If the condition is a single expression, make an implicit
    // comparison: != 0.
    AstExpr *cond = parse_expr(parser);
    if (cond->ast_type != AST_EXPR_BIN)
    {
        // TODO: match expression's type
        AstExprLit *zero = ast_alloc(AstExprLit);
        zero->scope = cond->scope;
        zero->lit_type = LIT_INT;
        zero->value_int.type = INT_I64;
        zero->value_int.value = 0;
        zero->value_int.flags = 0;

        AstExprBin *bin_cond = ast_alloc(AstExprBin);
        bin_cond->scope = cond->scope;
        bin_cond->lhs = cond;
        bin_cond->rhs = zero;
        bin_cond->op = BIN_NE;

        copy_loc(bin_cond, tok);
        return bin_cond;
    }
    else
    {
        AstExprBin *bin_cond = static_cast<AstExprBin *>(cond);
        return bin_cond;
    }
}

static AstExpr *parse_expr(Parser *parser, bool is_unary)
{
    AstExpr *lhs = NULL;

    Token tok = next(parser);
    switch (tok.type)
    {
        case TOK_IDENT:
        {
            parser->index -= 1;
            lhs = parse_ident_or_path(parser);

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
            AstExprParen *paren = ast_alloc(AstExprParen);

            paren->expr = parse_expr(parser);
            expect(parser, TOK_CLOSE_PAREN);

            lhs = paren;

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
            AstType *type = parse_type(parser);
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

            if_expr->cond = parse_cond(parser);

            expect(parser, TOK_OPEN_BRACE);
            if_expr->block = parse_block(parser);

            if (eat_optional(parser, TOK_KEY_ELSE))
                if_expr->else_expr = parse_expr(parser);

            lhs = if_expr;

            copy_loc(if_expr, tok);

            break;
        }
        case TOK_KEY_LOOP:
        {
            AstExprLoop *loop = ast_alloc(AstExprLoop);

            expect(parser, TOK_OPEN_BRACE);
            loop->block = parse_block(parser);

            if (loop->block->stmts.count == 0)
                report_error_no_arg("Empty loop block.", tok);

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

            while_->cond = parse_cond(parser);

            expect(parser, TOK_OPEN_BRACE);
            while_->block = parse_block(parser);

            lhs = while_;

            break;
        }
        case TOK_KEY_RETURN:
        {
            AstExprReturn *ret = ast_alloc(AstExprReturn);
            return ret;
        }

        // Unary operators.
        // Add more here!
        case TOK_AND:
        case TOK_ASTERISK:
        case TOK_MINUS:
        case TOK_NOT:
        {
            UnOp op = get_un_op(tok.type);
            AstExpr *expr = parse_expr(parser, true);

            if (expr->ast_type == AST_EXPR_LIT)
            {
                auto lit = static_cast<AstExprLit *>(expr);
                lit->value_int.flags |= INT_IS_NEGATIVE;

                // Drop the unary minus for negative literals.
                // They are already flagged as negative.
                lhs = lit;
                break;
            }

            auto un = ast_alloc(AstExprUn);
            un->op = op;
            un->expr = expr;

            copy_loc(un, tok);

            lhs = un;

            break;
        }

        case TOK_PLUS_EQ:
        case TOK_MINUS_EQ:
        case TOK_ASTERISK_EQ:
        case TOK_SLASH_EQ:
        {
            // FIXME
            assert(false);
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

    // Parse field expression(s).
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

    // Function call.
    if (peek(parser).type == TOK_OPEN_PAREN)
    {
        assert((lhs->ast_type == AST_EXPR_IDENT) || (lhs->ast_type == AST_EXPR_PATH) || (lhs->ast_type == AST_EXPR_FIELD));

        eat(parser);

        // TODO: clean this up?
        AstExprCall *call = ast_alloc(AstExprCall);
        call->name = static_cast<AstExprIdent *>(lhs);
        assert(call->name);

        while (peek(parser).type != TOK_CLOSE_PAREN)
        {
            AstExpr *arg = parse_expr(parser);
//            assert(arg->file.src);
            call->args.add(arg);

            if (peek(parser).type != TOK_CLOSE_PAREN)
                expect(parser, TOK_COMMA);
        }
        expect(parser, TOK_CLOSE_PAREN);

        lhs = call;
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
        case TOK_NE:
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

static void parse_stmt(Parser *parser, AstExprBlock *block)
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

        AstType *type = NULL;
        if ((peek(parser).type != TOK_EQ) && (peek(parser).type != TOK_SEMI))
            type = parse_type(parser);

        AstExpr *rhs = NULL;
        if (eat_optional(parser, TOK_EQ))
            rhs = parse_expr(parser);

        AstStmtDecl *decl = ast_alloc(AstStmtDecl);
        decl->bind = bind;
        decl->type = type;

        block->stmts.add(decl);

        // Desugar the assignment into a separate statement.
        //     e.g. let x = 5    =>    let x i64;
        //                             x = 5;
        if (rhs)
        {
            decl->desugared_rhs = rhs;

            AstExprAssign *assign = ast_alloc(AstExprAssign);
            assign->lhs = bind;
            assign->rhs = rhs;
            assign->flags |= ASSIGN_IS_DECL_DESUGARED_RHS;

            copy_loc(assign, ident);

            AstStmtSemi *semi = ast_alloc(AstStmtSemi);
            semi->expr = assign;

            // TODO: copy_loc for statement?

            block->stmts.add(semi);
        }

        expect(parser, TOK_SEMI);
    }
    else
    {
        AstExpr *expr = parse_expr(parser);
        if (eat_optional(parser, TOK_SEMI))
        {
            AstStmtSemi *stmt = ast_alloc(AstStmtSemi);
            stmt->expr = expr;

            block->stmts.add(stmt);
        }
        else
        {
            AstStmtExpr *stmt = ast_alloc(AstStmtExpr);
            stmt->expr = expr;

            block->stmts.add(stmt);
        }
    }
}

static AstExprBlock *parse_block(Parser *parser)
{
    AstExprBlock *block = ast_alloc(AstExprBlock);

    while (true)
    {
        if (peek(parser).type == TOK_CLOSE_BRACE)
            break;

        parse_stmt(parser, block);
    }
    expect(parser, TOK_CLOSE_BRACE);

    if (block->stmts.count > 0)
    {
        AstStmt *last = block->stmts[block->stmts.count - 1];
        if (last->ast_type == AST_STMT_EXPR)
        {
            AstStmtExpr *se = static_cast<AstStmtExpr *>(last);
            block->expr = se->expr;

            --block->stmts.count;
        }
    }

    return block;
}

static AstParam *parse_param(Parser *parser)
{
    Token name_tok = expect(parser, TOK_IDENT);

    AstParam *param = ast_alloc(AstParam);
    param->name = make_ident(name_tok);
    param->type = parse_type(parser);

    return param;
}

static AstFunc *parse_func(Parser *parser)
{
    AstFunc *func = ast_alloc(AstFunc);

    if (eat_optional(parser, TOK_KEY_EXTERN))
        func->flags |= FUNC_IS_EXTERN;

    expect(parser, TOK_KEY_FN);
    Token ident = expect(parser, TOK_IDENT);
    func->name = make_ident(ident);

    // Parse parameter list.
    expect(parser, TOK_OPEN_PAREN);
    while (peek(parser).type != TOK_CLOSE_PAREN)
    {
        AstParam *param = parse_param(parser);
        func->params.add(param);

        if (peek(parser).type != TOK_CLOSE_PAREN)
            expect(parser, TOK_COMMA);
    }
    expect(parser, TOK_CLOSE_PAREN);

    if (eat_optional(parser, TOK_R_ARROW))
        func->ret = parse_type(parser);

    // Extern functions are just a declaration; there is no body.
    if (func->flags & FUNC_IS_EXTERN)
    {
        expect(parser, TOK_SEMI);
        return func;
    }

    expect(parser, TOK_OPEN_BRACE);

    func->block = parse_block(parser);
    func->block->flags |= BLOCK_IS_FUNC_BLOCK;

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

    root->current_module = root->global_module;

    while (true)
    {
        Token tok = peek(&parser);
        if (tok.type == TOK_EOF)
            break;

        assert(root->current_module);
        Module *mod = root->current_module;

        if ((tok.type == TOK_KEY_FN) || (tok.type == TOK_KEY_EXTERN))
        {
            AstFunc *func = parse_func(&parser);
            mod->funcs.add(func);
        }
        else if (tok.type == TOK_KEY_STRUCT)
        {
            AstStruct *struct_ = parse_struct(&parser);
            mod->structs.add(struct_);
        }
        else if (tok.type == TOK_KEY_LET)
        {
            eat(&parser);

            // TODO: this is mostly copy-pasted from parse_stmt(). Factor out
            // into parse_decl() or something?
            // TODO: multiple decls, patterns, etc.
            Token ident = expect(&parser, TOK_IDENT);
            AstExpr *bind = make_ident(ident);

            AstType *type = parse_type(&parser);

            AstStmtDecl *decl = ast_alloc(AstStmtDecl);
            decl->bind = bind;
            decl->type = type;

            if (eat_optional(&parser, TOK_EQ))
            {
                AstExpr *rhs = parse_expr(&parser);
                decl->desugared_rhs = rhs;
            }

            mod->vars.add(decl);

            expect(&parser, TOK_SEMI);
        }
        else if (tok.type == TOK_KEY_IMPORT)
        {
            // Imports are handled in main.cpp, so ignore them here.
            eat(&parser);
            eat(&parser);
            eat(&parser);
        }
        else if (tok.type == TOK_KEY_MODULE)
        {
            eat(&parser);

            // TODO: nested module name parsing
            tok = expect(&parser, TOK_IDENT);

            // TODO: Better allocation?
            char *name = (char *)malloc(tok.len + 1);
            string_copy(tok.str, name, tok.len);
            name[tok.len] = '\0';

            // TODO: move module creation into type checking phase?
            mod = make_module(root, name, mod);
            root->current_module = mod;

            expect(&parser, TOK_OPEN_BRACE);
        }
        else if (tok.type == TOK_CLOSE_BRACE)
        {
            eat(&parser);

            assert(mod);
            mod = mod->parent;
        }
        else
        {
            report_error("Invalid top-level token of type \"%s\". Only structs and functions are allowed.",
                         tok,
                         token_type_names[tok.type]);
        }
    }
}
