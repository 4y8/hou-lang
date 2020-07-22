#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "hou.h"

#define NKEYWORD 6
#define NPUNCT   12

unsigned int linum;
unsigned int cpos;
unsigned int nvar = 0;
KeywordToken keywords[NKEYWORD] = {
        {LET, "let"}, {IN, "in"}, {IF, "if"}, {ELIF, "elif"}, {ELSE, "else"},
        {EXTERN, "extern"}
};
PuncToken punctuation[NPUNCT] = {
        {DOT, '.'}, {COL, ','}, {DIVISE, '/'}, {SEMICOL, ';'}, {PLUS, '+'},
        {TIMES, '*'}, {LPARENT, '('}, {RPARENT, ')'}, {EQUAL, '='}, {LOW, '<'},
        {GREAT, '>'}, {BACKS, '\\'}
};

MemoryTable *mem = NULL;

void *
safe_malloc(size_t size)
{
        void *p;
        MemoryTable *nmem;

        p = malloc(size);
        if (!p) error("Can't allocate memory.", 0, 0, SYNTAX_ERROR);
        nmem = malloc(sizeof(MemoryTable));
        if (!nmem) error("Can't allocate memory.", 0, 0, SYNTAX_ERROR);
        *nmem = (MemoryTable){.next = mem, .p = p};
        mem = nmem;
        return p;
}

void
free_all()
{
        MemoryTable *prev;

        while (mem) {
                prev = mem->next;
                free(mem->p);
                free(mem);
                mem = prev;
        }

}

int
keyword_to_token(char *s)
{

        for (unsigned i = 0; i < NKEYWORD; ++i)
                if (!strcmp(keywords[i].s, s)) return keywords[i].t;
        return -1;
}

int
punct_to_token(char c)
{

        for (unsigned i = 0; i < NPUNCT; ++i)
                if (punctuation[i].c == c) return punctuation[i].t;
        return -1;
}


void
error(char *msg, int linum, int cpos, Error err_code)
{
        char *err_header;
        int  header_size;

        switch(err_code) {
        case UNEXPECTED_CHAR:
                header_size = 21;
                err_header = safe_malloc(header_size);
                strncpy(err_header, "UNEXPECTED CHARACTER ", header_size);
                break;
        case TYPE_ERROR:
                header_size = 11;
                err_header = safe_malloc(header_size);
                strncpy(err_header, "TYPE ERROR ", header_size);
                break;
        case SYNTAX_ERROR:
                header_size = 13;
                err_header = safe_malloc(header_size);
                strncpy(err_header, "SYNTAX ERROR ", header_size);
                break;
        } printf("\033[35m\033[1m-- ");
        printf("%s", err_header);
        for (int i = 0; i < 77 - header_size; i++) printf("-");
        printf("\n\n\033[39m\033[1m%d:%d: %s\n", linum, cpos, msg);
        free_all();
        exit(1);
}

Token
token(unsigned int type)
{

        return (Token){.type = type, .linum = linum, .cpos = cpos};
}

Token
token_str(char *str)
{
        Token t;

        t     = token(IDE);
        t.str = str;
        return t;
}

Token
token_num(int num)
{
        Token t;

        t     = token(NUM);
        t.num = num;
        return t;

}

char *s;

char *
lex_while(int (*fun)(int))
{
        char *str;
        int i;

        i = 1;
        while (fun(*(++s))) {
                ++i;
                ++cpos;
        } str = safe_malloc((i + 1) * sizeof(char));
        strncpy(str, s - i, i);
        str[i] = 0;
        --cpos;
        return str;
}

int
iside(int c)
{

        return isalnum(c) || c == '_';
}

Token
lexer()
{
        Token tok;
        int    tpos;

        tpos   = -1;
        if (*s == 0) return token(END);
        if (isalpha(*s)) {
                char *str = lex_while(iside);
                int i = keyword_to_token(str);
                if (i == -1) tok = token_str(str);
                else tok = token(i);
        } else if (isdigit(*s)) {
                tok = token_num(atoi(lex_while(isdigit)));
        } else {
                int i = punct_to_token(*s);
                if (i != -1) tok = token(i);
                else {
                        switch (*s) {
                        case '\n': ++linum; cpos = -1;
                        case ' ': case '\t': ++cpos; ++s; tok = lexer(); --s;
                                break;
                        case '-':
                                if (*(++s) == '>') {
                                        tok = token(ARR);
                                        ++cpos;
                                } else if (*s == '-') {
                                        while(*(++s) != '\n' && *s != 0) ++cpos;
                                        return lexer();
                                } else {
                                        --s;
                                        tok = token(MINUS);
                                } break;
                        default : error("Unexecpected charachter", linum, cpos,
                                        UNEXPECTED_CHAR);
                        }
                } ++cpos;
                ++s;
        } return tok;
}

Token act_tok;
Token unused_tok;

Token
next_token()
{
        if (unused_tok.type == END) return act_tok = lexer();
        else {
                Token t = unused_tok;
                unused_tok = token(END);
                return t;
        }
}

Token
act_token()
{
        if (unused_tok.type == END) return act_tok;
        else return unused_tok;
}

void
assert(unsigned int type)
{

        if (next_token().type != type)
                error("Unexpected token", act_token().linum, act_token().cpos,
                      SYNTAX_ERROR);
}

int
peek(unsigned int type)
{
        Token t;

        t = next_token();
        if (t.type != type) {
                unused_tok = t;
                return 0;
        } return 1;
}

Expr *
parse_expr()
{
        Expr *expr;
        Token tok;

        tok   = next_token();
        expr  = safe_malloc(sizeof(Expr));
        *expr = (Expr){.linum = tok.linum, .cpos = tok.cpos,
                       .abspos = tok.abspos};
        switch (tok.type) {
        case IDE:
                *expr = (Expr){.type = VAR, .var = tok.str};
                break;
        case NUM:
                *expr = (Expr){.type = INT, .num = tok.num};
                break;
        case LET: {
                struct decllist l;
                struct decllist *p;
                p = &l;
                while (!peek(IN)) {
                        p->next = safe_malloc(sizeof(struct decllist));
                        p->next->decl = parse_top_level();
                        p = p->next;
                } p->next = NULL;
                *expr = (Expr){.type = LETIN, .letin.decl = l.next,
                               .letin.expr = parse_body()};
                break;
        }
        case IF: {
                Expr *cond;
                assert(LPARENT);
                cond = parse_rel();
                assert(RPARENT);
                *expr = (Expr){.type = IF_CLAUSE,
                               .if_clause.condition = cond,
                               .if_clause.if_expr = parse_body(),
                               .if_clause.else_expr = parse_else()};
                break;
        }
        case LPARENT:
                expr = parse_expr();
                assert(RPARENT);
                break;
        case BACKS:
                *expr = (Expr){.type = LAM, .lam = safe_malloc(sizeof(Decl))};
                *expr->lam = (Decl){.type = FUN_DECL,
                                    .name = "_",
                                    .fun_decl.args = parse_arg(ARR),
                                    .fun_decl.body = parse_body()};
                break;

        default:
                error("Unexpected token.", tok.linum, tok.cpos,
                      SYNTAX_ERROR);
                break;
        } return expr;
}

struct elist *
parse_body()
{
        struct elist body;
        struct elist *p;
        unsigned int loop;

        loop = 1;
        p = &body;
        for(;;) {
                p->next = safe_malloc(sizeof(struct elist));
                p->next->expr = *parse_rel();
                p = p->next;
                if (act_token().type == SEMICOL) next_token();
                else break;
        } p->next = NULL;
        return body.next;
}

struct elist *
parse_else()
{

        if (peek(ELSE)) {
                struct elist *body;
                body = parse_body();
                assert(DOT);
                return body;
        } else if (peek(ELIF)) {
                struct elist *body = safe_malloc(sizeof(struct elist));
                body->next = NULL;
                assert(LPARENT);
                Expr *cond = parse_rel();
                assert(RPARENT);
                body->expr = (Expr){.type = IF_CLAUSE,
                                    .if_clause.condition = cond,
                                    .if_clause.if_expr = parse_body(),
                                    .if_clause.else_expr = parse_else()
                }; return body;
        } else if (peek(DOT)) return NULL;
        else error("Unexpected token.", act_token().linum, act_token().cpos,
                   SYNTAX_ERROR);
        return NULL;
}

Expr *
parse_fun()
{
        Expr *e;

        e = parse_expr();
        while(peek(LPARENT)) {
                Expr *expr = malloc(sizeof(Expr));
                struct elist args;
                struct elist *p;
                expr->fun_call.fun = e;
                p = &args;
                while (!peek(RPARENT)) {
                        p->next = safe_malloc(sizeof(struct elist));
                        p->next->expr = *parse_rel();
                        p = p->next;
                        if (!peek(RPARENT)) assert(COL);
                        else unused_tok = token(RPARENT);
                } p->next = NULL;
                expr->type = FUN_CALL;
                expr->fun_call.args = args.next;
                e = expr;
        } return e;
}

Expr *
binop(Expr *left, Expr *right, unsigned int op)
{
        Expr *e;

        e = safe_malloc(sizeof(struct expr));
        *e = (Expr){.type = BINOP, .binop.op = op, .binop.left = left,
                    .binop.right = right};
        return e;
}

Expr *
parse_op(Expr * (*fun)(), unsigned int op0,
         unsigned int op1, unsigned int op2, unsigned int op3)
{
        Expr *expr;
        Expr *e;

        expr = fun();
        e = safe_malloc(sizeof(struct expr));
        e = expr;
        for (;;) {
                if (peek(op0)) e = binop(e, fun(), op1);
                else if (peek(op2)) e = binop(e, fun(), op3);
                else return e;
        }
}

Expr *
parse_mul()
{
        return parse_op(parse_fun, TIMES, OP_TIMES, DIVISE, OP_DIVISE);
}

Expr *
parse_add()
{
        return parse_op(parse_mul, PLUS, OP_PLUS, MINUS, OP_MINUS);
}

Expr *
parse_rel()
{
        Expr *expr;
        Expr *e;

        expr = parse_add();
        e = safe_malloc(sizeof(struct expr));
        e = expr;
        for (;;) {
                if (peek(GREAT))
                        e = peek(EQUAL) ?
                                binop(e, parse_add(), OP_GREATE) :
                                binop(e, parse_add(), OP_GREAT);
                else if (peek(LOW))
                        e = peek(EQUAL) ?
                                binop(e, parse_add(), OP_LOWE) :
                                binop(e, parse_add(), OP_LOW);
                else if (peek(EQUAL)) e = binop(e, parse_add(), OP_EQUAL);
                else return e;
        }
}

Context *init_ctx = NULL;

Type *
parse_type()
{
        Token tok;
        Type *t;

        t = NULL;
        while (!peek(DOT)) {
                tok = next_token();
                switch(tok.type) {
                case IDE:
                        t = tlit(tok.str);
                        break;
                case NUM:
                        t = tvar(tok.num);
                        break;
                case ARR:
                        return tfun(t, parse_type());
                default:
                        error("Unexpected token.", tok.linum, tok.cpos,
                              SYNTAX_ERROR);
                }
        } return t;
}

struct elist *
parse_arg(unsigned int sep)
{
        struct elist args;
        struct elist *p;

        p = &args;
        args.next = NULL;
        if (peek(sep)) return NULL;
        while (act_token().type != sep) {
                p->next = safe_malloc(sizeof(struct elist));
                p->next->expr = *parse_expr();
                if (p->next->expr.type != VAR)
                        error("Unexpected token.", act_token().linum,
                              act_token().cpos, SYNTAX_ERROR);
                p = p->next;
                if (!peek(sep))
                        assert(COL);
        } return args.next;
}

Decl
parse_top_level()
{
        Decl decl;
        Token tok;

        tok = next_token();
        if (tok.type == IDE) {
                char *name = tok.str;
                tok = next_token();
                if (tok.type == LPARENT) {
                        struct elist *args = parse_arg(RPARENT);
                        assert(ARR);
                        decl = (Decl){.type = FUN_DECL,
                                      .fun_decl.args = args,
                                      .fun_decl.body = parse_body(),
                                      .name = name};
                } else if (tok.type == EQUAL) {
                        decl = (Decl){.type = VAR_DECL, .name = name,
                                      .var_decl = parse_body()};
                } else error("Unexpected token.", act_token().linum,
                             act_token().cpos, SYNTAX_ERROR);
        } else if (tok.type == EXTERN) {
                tok = next_token();
                init_ctx = add_ctx(init_ctx, tok.str, gen(parse_type()));
                return parse_top_level();
        } else error("Unexpected token.", act_token().linum,
                     act_token().cpos, SYNTAX_ERROR);
        return decl;
}

struct decllist *
parse_program()
{
        struct decllist *p;
        struct decllist decls;

        decls.next = NULL;
        p = &decls;
        unused_tok = token(END);
        do {
                p->next = safe_malloc(sizeof(struct decllist));
                p->next->decl = parse_top_level();
                p = p->next;
        } while (!peek(END));
        p->next = NULL;
        return decls.next;
}

struct ilist*
ftv(Type *t)
{
        struct ilist *l;

        switch (t->type) {
        case TFUN:
                l = ftv(t->fun.left);
                struct ilist *p = l;
                while (p) p=p->next;
                p = ftv(t->fun.right);
                break;
        case TLIT: l = NULL; break;
        case TVAR:
                l       = safe_malloc(sizeof(struct ilist));
                l->next = NULL;
                l->i    = t->var;
                break;
        } return l;
}

int
occurs(struct ilist *l, int i)
{

        while(l) {
                if (l->i == i) return 1;
                l = l->next;
        } return 0;
}

struct ilist *
ftv_sch(Scheme sch)
{
        struct ilist *l;
        struct ilist *p;

        p = sch.bind;
        while (p) {
                if (occurs(sch.bind, p->i)) {
                        p->i = p->next->i;
                        p->next = p->next->next;
                        continue;
                } p = p->next;
        } return l;
}

Subst *
bind(unsigned int var, Type *t)
{
        Subst *s;

        if (t->type == TVAR && t->var == var) s = NULL;
        else if (occurs(ftv(t), var)) error("Occurs error happend", 0, 0,
                                             TYPE_ERROR);
        else {
                s = safe_malloc(sizeof(Subst));
                *s = (Subst){.t = t, .next = NULL, .nvar = var};
        } return s;
}

Type *
tfun(Type *left, Type *right)
{
        Type *t;

        t = safe_malloc(sizeof(Type));
        *t = (Type){.type = TFUN, .fun.right = right, .fun.left = left};
        return t;
}

Type *
tvar(unsigned int var)
{
        Type *t;

        t = safe_malloc(sizeof(Type));
        *t = (Type){.type = TVAR, .var = var};
        return t;
}

Type *
tlit(char *name)
{
        Type *t;

        t = safe_malloc(sizeof(Type));
        *t = (Type){.lit = name, .type = TLIT};
        return t;
}

Type *
app_subst(Type *t, Subst *s)
{

        if (t->type == TLIT) return t;
        else if (t->type == TFUN)
                return tfun(app_subst(t->fun.left, s), app_subst(t->fun.right, s));
        else
                while (s) {
                        if (s->nvar == t->var) return s->t;
                        s = s->next;
                }
        return t;
}

Scheme
app_subst_sch(Scheme sch, Subst *s)
{
        Subst *p;

        p = s;
        while (p) {
                if (occurs(sch.bind, p->nvar)) {
                        if (p->next) {
                                p->nvar = p->next->nvar;
                                p->t = p->next->t;
                                p->nvar = p->next->nvar;
                        } else s->nvar = -1;
                        continue;
                } p = p->next;
        } sch.type = app_subst(sch.type, s);
        return sch;
}

Subst *
compose_subst(Subst *s1, Subst *s2)
{
        Subst *p;

        if (!s1) return s2;
        if (!s2) return s1;
        p = s2;
        while (p) {
                p->t = app_subst(p->t, s1);
                if (p->next == NULL) {
                        p->next = s1;
                        return s2;
                } p = p->next;
        } return s2;
}

Subst *
unify(Type *t1, Type *t2)
{
        Subst *s;
        if (t1->type == TLIT && t2->type == TLIT && !strcmp(t2->lit, t1->lit))
                s = NULL;
        else if (t1->type == TVAR) s = bind(t1->var, t2);
        else if (t2->type == TVAR) s = bind(t2->var, t1);
        else if (t1->type == TFUN && t2->type == TFUN) {
                Subst *s1 = unify(t1->fun.left, t2->fun.left);
                Subst *s2 = unify(app_subst(t1->fun.right, s1),
                                  app_subst(t2->fun.right, s1));
                s = compose_subst(s1,s2);
        }
        else error("Can't unfiy types", 0, 0, TYPE_ERROR);

        return s;
}

Scheme
find_ctx(char *name, Context *ctx)
{

        while (ctx) {
                if (!strcmp(name, ctx->name))
                        return ctx->sch;
                ctx = ctx->next;
        } error("Unknown variable", 0, 0, TYPE_ERROR);
        exit(1);
}

void
app_subst_ctx(Subst *s, Context *ctx)
{

        while (ctx) {
                ctx->sch = app_subst_sch(ctx->sch, s);
                ctx = ctx->next;
        }
}

Type *
inst(Scheme sch)
{
        Type *t;
        struct ilist *p;

        p = sch.bind;
        t = safe_malloc(sizeof(Type));
        *t = *sch.type;
        while (p) {
                Subst s;
                s.next = NULL;
                s.nvar = p->i;
                s.t = tvar(++nvar);
                p = p->next;
                t = app_subst(t, &s);
        } return t;
}

Scheme
gen(Type *t)
{

        return (Scheme){.type = t, .bind = ftv(t)};
}

Type *
add_tfun(Type *t1, Type *t2)
{
        Type *p;

        p = t1;
        if (p->type == TFUN) {
                while (p->fun.right->type == TFUN) p = p->fun.right;
                p->fun.right = tfun(p->fun.right, t2);
                return t1;
        } else return tfun(t1, t2);
}

TypeReturn
infer(Expr expr, Context *ctx)
{
        TypeReturn tp;

        tp.subst = NULL;
        switch (expr.type) {
        case INT:
                tp.type = tlit("int");
                break;
        case VAR:
                tp.type = inst(find_ctx(expr.var, ctx));
                break;
        case BINOP: {
                TypeReturn r = infer(*expr.binop.right, ctx);
                app_subst_ctx(r.subst, ctx);
                TypeReturn l = infer(*expr.binop.left, ctx);
                tp.subst = compose_subst(r.subst, l.subst);
                tp.subst = compose_subst(tp.subst, unify(l.type, tlit("int")));
                tp.subst = compose_subst(tp.subst, unify(r.type, tlit("int")));
                switch (expr.binop.op) {
                case OP_PLUS:
                case OP_MINUS:
                case OP_TIMES:
                case OP_DIVISE: tp.type = tlit("int"); break;
                case OP_GREATE:
                case OP_LOWE:
                case OP_EQUAL:
                case OP_GREAT:
                case OP_LOW: tp.type = tlit("bool"); break;
                }
                break;
        }
        case FUN_CALL: {
                TypeReturn ft = infer(*expr.fun_call.fun, ctx);
                app_subst_ctx(ft.subst, ctx);
                TypeReturn at = infer_args(expr.fun_call.args, ctx);
                Type *s_type = tvar(++nvar);
                if (!expr.fun_call.args)
                        at.type = tfun(tlit("unit"), s_type);
                else
                        at.type = add_tfun(at.type, s_type);
                tp.subst = unify(at.type, ft.type);
                tp.subst = compose_subst(ft.subst, tp.subst);
                tp.type = app_subst(s_type, tp.subst);
                break;
        }
        case LETIN: {
                tp = infer_body(expr.letin.expr,
                                infer_decls(expr.letin.decl, ctx));
                break;
        }
        case IF_CLAUSE: {
                TypeReturn tcond = infer(*expr.if_clause.condition, ctx);
                tp.subst = unify(tcond.type, tlit("bool"));
                tp.subst = compose_subst(tp.subst, tcond.subst);
                app_subst_ctx(tp.subst, ctx);
                TypeReturn tif = infer_body(expr.if_clause.if_expr, ctx);
                tp.subst = compose_subst(tp.subst, tif.subst);
                app_subst_ctx(tp.subst, ctx);
                if (expr.if_clause.else_expr) {
                        TypeReturn telse =
                                infer_body(expr.if_clause.else_expr, ctx);
                        tp.subst = compose_subst(tp.subst, telse.subst);
                        tp.subst = compose_subst(tp.subst, unify(tif.type,
                                                                 telse.type));
                        tp.type = app_subst(telse.type, tp.subst);
                } else {
                        tp.subst = compose_subst(tp.subst,
                                                 unify(tif.type, tlit("unit")));
                        tp.type = tlit("unit");
                }
                break;
        }
        case LAM:
                tp = infer_decl(*expr.lam, ctx);
                break;
        }
        return tp;
}

TypeReturn
infer_args(struct elist *args, Context *ctx)
{
        TypeReturn tp;

        tp.type = NULL;
        if (args) {
                TypeReturn arg  = infer(args->expr, ctx);
                TypeReturn rest = infer_args(args->next, ctx);
                if (rest.type) {
                        tp.type  = tfun(arg.type, rest.type);
                        tp.subst = compose_subst(arg.subst, rest.subst);
                } else tp = arg;
        } return tp;

}

Scheme
scheme(struct ilist *bind, Type *type)
{

        return (Scheme){.bind = bind, .type = type};
}

Context *
add_ctx(Context *ctx, char *name, Scheme sch)
{
        Context *nctx;

        nctx = safe_malloc(sizeof(Context));
        nctx->name = safe_malloc(strlen(name) + 1);
        strcpy(nctx->name, name);
        nctx->sch = sch;
        nctx->next = ctx;
        return nctx;
}

TypeReturn
infer_decl(Decl decl, Context *ctx)
{
        TypeReturn tp;

        tp.subst = NULL;
        switch (decl.type) {
        case VAR_DECL:
                tp = infer_body(decl.var_decl, ctx);
                break;
        case FUN_DECL: {
                struct elist *p = decl.fun_decl.args;
                Type *decl_type = tvar(++nvar);
                ctx = add_ctx(ctx, decl.name, scheme(NULL, decl_type));
                while (p) {
                        ctx = add_ctx(ctx, p->expr.var,
                                      scheme(NULL, tvar(++nvar)));
                        p = p->next;
                } TypeReturn bt = infer_body(decl.fun_decl.body, ctx);
                app_subst_ctx(bt.subst, ctx);
                tp.subst = bt.subst;
                p = decl.fun_decl.args;
                TypeReturn at = infer_args(decl.fun_decl.args, ctx);
                if (!p) at.type = tfun(tlit("unit"), bt.type);
                else    at.type = add_tfun(at.type, bt.type);
                tp.subst = compose_subst(unify(decl_type, at.type), tp.subst);
                tp.type = app_subst(at.type, tp.subst);
                break;
        }
        }
        return tp;
}

TypeReturn
infer_body(struct elist *body, Context *ctx)
{
        TypeReturn tp;

        tp.subst = NULL;
        while (body) {
                TypeReturn inf = infer(body->expr, ctx);
                tp.subst = compose_subst(tp.subst, inf.subst);
                app_subst_ctx(tp.subst, ctx);
                tp.type = inf.type;
                body = body->next;
        } return tp;
}

Context *
infer_decls(struct decllist *decls, Context *ctx)
{

        while (decls) {
                TypeReturn dt = infer_decl(decls->decl, ctx);
                ctx = add_ctx(ctx, decls->decl.name, gen(dt.type));
                app_subst_ctx(dt.subst, ctx);
                decls = decls->next;
        } return ctx;
}

void
print_tab(int tab)
{

        for (int i = 0; i < tab; ++i) printf(" ");
}

void
print_elist(struct elist elist, int tab)
{
        struct elist *p;

        p = &elist;
        while (p) {
                print_expr(p->expr, tab);
                p = p->next;
        }
}

void
print_decllist(struct decllist *decllist, int tab)
{

        while (decllist) {
                print_decl(decllist->decl, tab);
                decllist = decllist->next;
        }
}

char *
op_to_char(unsigned int op)
{

        switch (op) {
        case OP_PLUS:   return "+";
        case OP_MINUS:  return "-";
        case OP_TIMES:  return "*";
        case OP_DIVISE: return "/";
        case OP_LOWE:   return "<=";
        case OP_LOW:    return "<";
        case OP_GREATE: return ">=";
        case OP_GREAT:  return ">";
        case OP_EQUAL:  return "=";
        }
        return "";
}

void
print_token(Token t)
{
        switch(t.type) {
        case IN:      printf("in");                    break;
        case IF:      printf("if");                    break;
        case LET:     printf("let");                   break;
        case ELSE:    printf("else");                  break;
        case ELIF:    printf("elif");                  break;
        case EXTERN:  printf("extern");                break;
        case NUM:     printf("number: %d", t.num);     break;
        case IDE:     printf("identifier: %s", t.str); break;
        case STR:     printf("string: %s", t.str);     break;
        case COL:     printf(",");                     break;
        case DOT:     printf(".");                     break;
        case ARR:     printf("->");                    break;
        case LOW:     printf("<");                     break;
        case PLUS:    printf("+");                     break;
        case GREAT:   printf(">");                     break;
        case EQUAL:   printf("=");                     break;
        case MINUS:   printf("-");                     break;
        case TIMES:   printf("*");                     break;
        case BACKS:   printf("\\");                    break;
        case LPARENT: printf("(");                     break;
        case DIVISE:  printf("/");                     break;
        case RPARENT: printf(")");                     break;
        case SEMICOL: printf(";");                     break;
        case END:     printf("END");                   break;
    }
}

void
print_expr(struct expr expr, int tab)
{

        print_tab(tab);
        switch (expr.type) {
        case INT:
                printf("number: %d\n", expr.num);
                break;
        case FUN_CALL:
                printf("function call:\n");
                print_expr(*expr.fun_call.fun, tab + 2);
                print_tab(tab);
                printf("args:\n");
                print_elist(*expr.fun_call.args, tab + 2);
                break;
        case VAR:
                printf("variable: %s\n", expr.var);
                break;
        case LETIN:
                printf("let\n");
                print_decllist(expr.letin.decl, tab + 2);
                print_tab(tab);
                printf("in\n");
                print_elist(*expr.letin.expr, tab + 2);
                break;
        case BINOP:
                printf("binop: %s\n", op_to_char(expr.binop.op));
                print_expr(*expr.binop.left, tab + 2);
                print_expr(*expr.binop.right, tab + 2);
                break;
        case IF_CLAUSE:
                printf("if: \n");
                print_expr(*expr.if_clause.condition, tab + 2);
                print_tab(tab);
                printf("then: \n");
                print_elist(*expr.if_clause.if_expr, tab + 2);
                if (expr.if_clause.else_expr) {
                        print_tab(tab);
                        printf("else: \n");
                        print_elist(*expr.if_clause.else_expr, tab + 2);
                }
        case LAM:
                printf("lambda:\n");
                print_decl(*expr.lam, tab + 2);
                break;
        }
}

void
print_decl(struct decl decl, int tab)
{

        print_tab(tab);
        switch (decl.type) {
        case FUN_DECL:
                printf("function: %s\n", decl.name);
                print_elist(*decl.fun_decl.args, tab + 2);
                print_elist(*decl.fun_decl.body, tab + 2);
                break;
        case VAR_DECL:
                printf("variable: %s\n", decl.name);
                print_elist(*decl.var_decl, tab + 2);
                break;
        }
}

void
print_type(Type t)
{

        switch (t.type) {
        case TFUN:
                printf("(");
                print_type(*t.fun.left);
                printf(" -> ");
                print_type(*t.fun.right);
                printf(")");
                break;
        case TLIT:
                printf("%s", t.lit);
                break;
        case TVAR:
                printf("%d", t.var);
                break;
        }
}

#define NREG 8

int used_registers[NREG] = {
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
};

char *registers[NREG] = {
        "rcx",
        "rdx",
        "rdi",
        "rsi",
        "r8",
        "r9",
        "r10",
        "r11"
};

BSSTable *bss_table;
BSSTable *bss_table;
FreeBSSTable *free_bss_table;
int ndecl = -1;

void
add_bss(char *name, int size)
{
        BSSTable *nbss_table;
        BSSTable *p;

        /* Check if the name is already used  */
        p = bss_table;
        while (p) {
                if (!strcmp(name, p->name)) return;
                p = p->next;
        } nbss_table = safe_malloc(sizeof(BSSTable));
        nbss_table->name = safe_malloc(64);
        strncpy(nbss_table->name, name, 64);
        nbss_table->size = size;
        nbss_table->next = bss_table;
        bss_table = nbss_table;
}

void
free_bss(char *name, int size)
{
        FreeBSSTable *nf_table;

        nf_table = safe_malloc(sizeof(FreeBSSTable));
        *nf_table = (FreeBSSTable){.size = size, .name = name,
                                   .next = free_bss_table};
        free_bss_table = nf_table;
}

char *
alloc_bss(int size)
{
        char *name;

        if (free_bss_table && free_bss_table->size == size) {
                name = free_bss_table->name;
                free_bss_table = free_bss_table->next;
                return name;
        } name = safe_malloc(256);
        sprintf(name, "_bss_%d", ++ndecl);
        add_bss(name, size);
        sprintf(name, "__bss_%d", ++ndecl);
        return name;
}

int
alloc_reg()
{

        for (int i = 0; i < NREG; ++i)
                if (!used_registers[i]) {
                        used_registers[i] = 1;
                        return i;
                }
        exit(1);
}

void
free_reg(char *reg)
{
        for (int i = 0; i < NREG; ++i)
                if (!strcmp(registers[i], reg)) {
                        used_registers[i] = 0;
                        return;
                }
}

SContext *
add_sctx(SContext *ctx, char *name, int num)
{
        SContext *nctx;

        nctx = safe_malloc(sizeof(SContext));
        *nctx = (SContext){.next = ctx, .name = name, .num = num};
        return nctx;
}

int
is_power_of2(int n)
{
        int i;

        if (n == 0) return -1;
        i = 0;
        while (n != 1) {
                if (n%2 != 0) return -1;
                n >>= 1;
                ++i;
        } return i;
}

void
cmp_e(char *l, char *r, char *op)
{

        ++ndecl;
        printf("cmp %s, %s\n"
               "mov %s, 1\n"
               "j%s .__label%d\n"
               "mov %s, 0\n"
               ".__label%d:\n", l, r, l, op, ndecl, l, ndecl);
}

char *var_accessor = "rsp";

char *
compile_expr(Expr e, SContext *ctx, char *reg)
{

        switch (e.type) {
        case INT: {
                char *ret_reg = reg ? reg : registers[alloc_reg()];
                if (e.num) printf("mov %s, %d\n", ret_reg, e.num);
                else printf("xor %s, %s\n", ret_reg,
                            ret_reg);
                return ret_reg;
        }
        case BINOP: {
                if (e.binop.right->type == INT) {
                        if (e.binop.right->num == 0)
                                switch(e.binop.op) {
                                case OP_PLUS: /* FALLTHROUGH */
                                case OP_MINUS:
                                        return compile_expr(*e.binop.left, ctx, reg);
                                case OP_TIMES:
                                        return compile_expr((Expr){.type = INT,
                                                                .num = 0}, ctx, reg);
                                case OP_DIVISE:
                                        error("Division by zero.", e.linum, e.cpos,
                                              SYNTAX_ERROR);
                                default: break;
                                }
                        int n = is_power_of2(e.binop.right->num);
                        if (n != -1) {
                                if (e.binop.op == OP_TIMES) {
                                        char *ret_reg = compile_expr(*e.binop.left,
                                                                     ctx, reg);
                                        printf("shl %s, %d\n", ret_reg, n);
                                        return ret_reg;
                                } if (e.binop.op == OP_DIVISE) {
                                        char *ret_reg = compile_expr(*e.binop.left,
                                                                 ctx, reg);
                                        printf("shr %s, %d\n", ret_reg, n);
                                        return ret_reg;
                                }
                        }
                } if (e.binop.left->type == INT) {
                        if (e.binop.left->num == 0)
                                switch(e.binop.op) {
                                case OP_PLUS:
                                        return compile_expr(*e.binop.right, ctx, reg);
                                case OP_TIMES:
                                        return compile_expr((Expr){.type = INT,
                                                                .num = 0}, ctx, reg);
                                default: break;
                                }
                        int n = is_power_of2(e.binop.left->num);
                        if (n != -1 && e.binop.op == OP_TIMES) {
                                char *ret_reg = compile_expr(*e.binop.right,
                                                         ctx, reg);
                                printf("shl %s, %d\n", ret_reg, n);
                                return ret_reg;
                        }
                }
                char *regl = compile_expr(*e.binop.left, ctx, reg);
                char *regr = compile_expr(*e.binop.right, ctx, NULL);
                switch (e.binop.op) {
                case OP_PLUS:
                        printf("add %s, %s\n", regl, regr);
                        break;
                case OP_MINUS:
                        printf("sub %s, %s\n", regl, regr);
                        break;
                case OP_TIMES:
                        printf("imul %s, %s\n", regl, regr);
                        break;
                case OP_DIVISE:{
                        int reg = -1;
                        if (!strcmp("rdx", regr)) {
                                reg = alloc_reg();
                                printf("mov %s, rdx\n", registers[reg]);
                                regr = safe_malloc(4);
                                strcpy(regr, registers[reg]);
                        } printf("push rdx\n"
                                 "mov rax, %s\n"
                                 "xor rdx, rdx\n"
                                 "idiv %s\n"
                                 "mov %s, rax\n"
                                 "pop rdx\n", regl, regr, regl);
                        if (reg != -1) used_registers[reg] = 0;
                        break;
                }
                case OP_LOW:    cmp_e(regl, regr, "l");   break;
                case OP_LOWE:   cmp_e(regl, regr, "le");  break;
                case OP_EQUAL:  cmp_e(regl, regr, "e");   break;
                case OP_GREAT:  cmp_e(regl, regr, "g");   break;
                case OP_GREATE: cmp_e(regl, regr, "ge");  break;
                } free_reg(regr);
                return regl;
        }
        case VAR: {
                char *ret_reg = reg ? reg : registers[alloc_reg()];
                while (ctx) {
                        if (!strcmp(e.var, ctx->name)) {
                                printf("mov %s, [%s + %d]\n",
                                       ret_reg, var_accessor,
                                       (nvar - ctx->num) * 8);
                                return ret_reg;
                        } ctx = ctx->next;
                } printf("mov %s, [_%s]\n", ret_reg, e.var);
                return ret_reg;
        }
        case LETIN: {
                struct decllist *p = e.letin.decl;
                FreeBSSTable *f_table = NULL;
                int length = 0;
                while (p) {
                        char *s = alloc_bss(8);
                        FreeBSSTable *nf_table = safe_malloc(sizeof(FreeBSSTable));
                        *nf_table = (FreeBSSTable){.name = s, .size = 8,
                                                   .next = f_table};
                        f_table = nf_table;

                        compile_decl(p->decl, ctx, s);
                        ctx = add_sctx(ctx, p->decl.name, ++nvar);

                        printf("push QWORD [_%s]\n", s);
                        p = p->next;
                        ++length;
                }
                char *ret_reg = compile_body(e.letin.expr, ctx, reg);
                printf("add rsp, %d\n", length << 3);
                nvar -= length;
                while (f_table) {
                        free_bss(f_table->name, f_table->size);
                        f_table = f_table->next;
                } return ret_reg;
        }
        case FUN_CALL: {
                int length = 0;
                /* Saves registers. */
                int local_used[NREG];
                printf("push rax\n");
                ++nvar;
                memcpy(local_used, used_registers, NREG * sizeof(int));
                for (int i = 0; i < NREG; ++i)
                        if (local_used[i]) {
                                printf("push %s\n", registers[i]);
                                ++nvar;
                        }
                compile_expr(*e.fun_call.fun, ctx, "rax");
                struct elist *p = e.fun_call.args;
                while (p) {
                        char *arg = compile_expr(p->expr, ctx, NULL);
                        printf("push %s\n", arg);
                        ++nvar;
                        free_reg(arg);
                        p = p->next;
                        ++length;
                } printf("call rax\n");
                printf("add rsp, %d\n", length << 3);
                nvar -= length;
                char *ret_reg = reg ? reg : registers[alloc_reg()];
                printf("mov %s, rax\n", ret_reg);
                for (int i = NREG - 1; i >= 0; --i)
                        if (local_used[i]) {
                                printf("pop %s\n", registers[i]);
                                --nvar;
                        }
                printf("pop rax\n");
                --nvar;
                return ret_reg;
        }
        case IF_CLAUSE: {
                char *scratch_reg = compile_expr(*e.if_clause.condition, ctx, NULL);
                char label_if[128], label_else[128], label_end[128];
                char *ret_reg = reg ? reg : registers[alloc_reg()];
                sprintf(label_if, "__if%d", ++ndecl);
                sprintf(label_end, "__end%d", ndecl);
                sprintf(label_else, "__else%d", ndecl);
                printf("cmp %s, 1\n"
                       "je %s\n"
                       "jmp %s\n"
                       "%s:\n", scratch_reg, label_if, label_else, label_if);
                compile_body(e.if_clause.if_expr, ctx, ret_reg);
                printf("jmp %s\n", label_end);
                printf("%s:\n", label_else);
                if (e.if_clause.else_expr)
                        compile_body(e.if_clause.else_expr, ctx, ret_reg);
                printf("%s:\n", label_end);
                return ret_reg;
        }
        case LAM: {
                /* FIXME */
                char *ret_reg = reg ? reg : registers[alloc_reg()];
                char *reg  = alloc_bss(8);
                compile_decl(*e.lam, ctx, reg);
                printf("mov %s, [_%s]\n", ret_reg, reg);
                return ret_reg;
        }
        }
        return "";
}

char *
compile_body(struct elist *body, SContext *ctx, char *reg)
{
        char *s;

        while (body) {
                s = compile_expr(body->expr, ctx, reg);
                if (body->next) free_reg(s);
                body = body->next;
        } return s;
}

void
compile_decl(Decl decl, SContext *ctx, char *name)
{

        add_bss(name, 8);
        if (decl.type == VAR_DECL) {
                char *reg = compile_body(decl.var_decl, ctx, NULL);
                printf("mov [_%s], %s\n", name, reg);
                free_reg(reg);
        } else {
                char label[64];
                int length = 1;
                int n = ++ndecl;
                struct elist *p = decl.fun_decl.args;
                sprintf(label, "__decl%d", ++ndecl);
                printf("jmp %s\n"
                       "__%s%d:\n", label, name, n);
                while (p) {
                        ctx = add_sctx(ctx, p->expr.var, ++nvar);
                        ++length;
                        p = p->next;
                }  ++nvar;
                char *reg = compile_body(decl.fun_decl.body, ctx, NULL);
                printf("mov rax, %s\n"
                       "ret\n"
                       "%s:\n", reg, label);
                printf("lea %s, [__%s%d]\n"
                       "mov [_%s], %s\n", reg, name, n, name, reg);
                free_reg(reg);
                nvar -= length;
        }
}

void
compile_bss()
{

        printf("section .bss\n");
        while (bss_table) {
                printf("_%s: resb %d\n", bss_table->name, bss_table->size);
                bss_table = bss_table->next;
        }
}

void
compile_decls(struct decllist *decls)
{

        while (decls) {
                nvar = -1;
                for (int i = 0; i < NREG; ++i) used_registers[i] = 0;
                compile_decl(decls->decl, NULL, decls->decl.name);
                decls = decls->next;
        }
}

char *prolog =
        "DEFAULT REL\n"
        "global main\n"
        "section .text\n"
        "main:\n";

char *epilog =
        "mov rax, 60\n"
        "syscall\n";

void
program(char *prog)
{
        struct decllist *decl;

        s = prog;
        linum = 1;
        cpos  = 0;
        decl  = parse_program();
        infer_decls(decl, init_ctx);
        printf("%s", prolog);
        compile_decls(decl);
        printf("mov rdi, [_main]\n");
        printf("%s", epilog);
        compile_bss();
        free_all();
}

int
main(int argc, char **argv)
{

        program(argv[1]);
        return 0;
}
