#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "hou.h"

#define TOKEN_SIZE 100
#define NKEYWORD   2

unsigned int linum;
unsigned int cpos;
unsigned int nvar = 0;
char_to_tok keywords[] = {{LET, "let"}, {IN, "in"}};

int
char_to_token(char *s)
{

        for (unsigned i = 0; i < NKEYWORD; ++i)
                if (!strcmp(keywords[i].s, s))
                        return keywords[i].t;
        return -1;
}

void
error(char *msg, int linum, int cpos, Error err_code)
{
        char *err_header;
        int  header_size;

        switch(err_code) {
        case UNEXPECTED_CHAR:
                header_size = 22;
                err_header = malloc(header_size * sizeof(char));
                strncpy(err_header, "UNEXPECTED CHARACTER ", header_size);
                break;
        case TYPE_ERROR:
                header_size = 11;
                err_header = malloc(header_size * sizeof(char));
                strncpy(err_header, "TYPE ERROR ", header_size);
                break;
        case SYNTAX_ERROR:
                header_size = 13;
                err_header = malloc(header_size * sizeof(char));
                strncpy(err_header, "SYNTAX ERROR ", header_size);
                break;
        }
        printf("\033[35m\033[1m-- ");
        printf("%s", err_header);
        for (int i = 0; i < 77 - header_size; i++) printf("-");
        free(err_header);
        exit(1);
}

Token
make_token(unsigned int type)
{
        Token t;

        t.type  = type;
        t.linum = linum;
        t.cpos  = cpos;
        return t;
}

Token
make_token_str(char *str)
{
        Token t;

        t     = make_token(IDE);
        t.str = str;
        return t;
}

Token
make_token_num(int num)
{
        Token t;

        t     = make_token(NUM);
        t.num = num;
        return t;

}

Token*
lexer(char *s)
{
        Token *tokens;
        int    tpos;

        cpos   = 0;
        linum  = 1;
        tpos   = -1;
        tokens = malloc(TOKEN_SIZE * sizeof(Token));
        do {
                if (isalpha(*s)) {
                        int   i = 1;
                        char *str;

                        while (isalpha(*(++s))) {
                                ++i;
                                ++cpos;
                        }
                        str  = malloc((i + 1) * sizeof(char));
                        strncpy(str, s - i, i);
                        i = char_to_token(str);
                        if (i == -1)
                                *(tokens + (++tpos)) = make_token_str(str);
                        else *(tokens + (++tpos)) = make_token(i);
                        --s;
                        --cpos;
                } else if (isdigit(*s)) {
                        int   i = 1;
                        char *str;

                        while (isdigit(*(++s))) {
                                ++i;
                                ++cpos;
                        }
                        str  = malloc((i + 1) * sizeof(char));
                        strncpy(str, s - i, i);
                        *(tokens + (++tpos)) = make_token_num(atoi(str));
                        --s;
                        --cpos;
                } else {
                        switch (*s) {
                        case ',': *(tokens + (++tpos)) = make_token(COL);     break;
                        case '+': *(tokens + (++tpos)) = make_token(PLUS);    break;
                        case '*': *(tokens + (++tpos)) = make_token(TIMES);   break;
                        case '=': *(tokens + (++tpos)) = make_token(EQUAL);   break;
                        case '/': *(tokens + (++tpos)) = make_token(DIVISE);  break;
                        case '(': *(tokens + (++tpos)) = make_token(LPARENT); break;
                        case ')': *(tokens + (++tpos)) = make_token(RPARENT); break;
                        case ';': *(tokens + (++tpos)) = make_token(SEMICOL); break;
                        case ' ': case '\t': break;
                        case '\n': ++linum; cpos = 0; break;
                        case '-':
                                if (*(++s) == '>') {
                                        *(tokens + (++tpos)) = make_token(ARR);
                                        ++cpos;
                                } else {
                                        *(tokens + (++tpos)) = make_token(MINUS);
                                } break;
                        default : error("Unxecpected charachter", linum, cpos,
                                        UNEXPECTED_CHAR);
                        }
                        ++cpos;
                }

        } while (*(++s) != '\0');
        *(tokens + tpos + 1) = make_token(END);
        return tokens;
}

Token *toks;

void
assert(Token token)
{

        if (toks->type != token.type)
                error("Unexpected token", toks->linum, toks->cpos,
                      SYNTAX_ERROR);
        ++toks;
}                   

Expr *
parse_expr()
{
        Expr *expr;

        expr = malloc(sizeof(struct expr));
        expr->linum = toks->linum;
        expr->cpos  = toks->cpos;
        expr->abspos  = toks->abspos;
        switch (toks->type) {
        case IDE:
                switch ((++toks)->type) {
                case LPARENT: {
                        struct elist args;
                        struct elist *pt;
                        expr->fun_call.fun = malloc(sizeof(Expr));
                        expr->fun_call.fun->type = VAR;
                        expr->fun_call.fun->var = (toks - 1)->str;
                        Token *tp;
                        pt = &args;
                        ++toks;
                        while (toks->type != RPARENT) {
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = *parse_expr();
                                pt = pt->next;
                                if (toks->type != RPARENT) assert(make_token(COL));
                        } pt->next = NULL;
                        expr->type = FUN_CALL;
                        ++toks;
                        expr->fun_call.args = args.next;
                        break;
                }
                default:
                        expr->type = VAR;
                        expr->var = (toks - 1)->str;
                        break;
                }
                break;
        case NUM:
                expr->type = INT;
                expr->num = toks->num;
                ++toks;
                break;
        case LET: {
                struct decllist l;
                struct decllist *lp;
                lp = &l;
                ++toks;
                while (toks->type != IN) {
                        lp->next = malloc(sizeof(struct decllist));
                        lp->next->decl = parse_top_level(toks);
                        lp = lp->next;
                } ++toks;
                lp->next = NULL;
                expr->letin.decl = l.next;
                expr->letin.expr = parse_body(toks);
                expr->type = LETIN;
                break;
        }
        default:
                error("Unexpected token.", toks->linum, toks->cpos,
                      SYNTAX_ERROR);
                break;
        }
        return expr;
}

struct elist *
parse_body()
{
        struct elist body;
        struct elist *bp;
        unsigned int loop;

        loop = 1;
        bp = &body;
        while (loop) {
                bp->next = malloc(sizeof(struct elist));
                bp->next->expr = *parse_add();
                bp = bp->next;
                if (toks->type == SEMICOL) ++toks;
                else loop = 0;
        } bp->next = NULL;
        return body.next;
}

struct expr *
binop(struct expr *left, struct expr *right, unsigned int op)
{
        struct expr *e;

        e = malloc(sizeof(struct expr));
        e->type = BINOP;
        e->binop.op = op;
        e->binop.left = left;
        e->binop.right = right;
        return e;
}

Expr *
parse_op(Expr * (*fun)(), unsigned int op0,
          unsigned int op1, unsigned int op2, unsigned int op3)
{
        Expr *expr;
        Expr *e;

        expr = fun(toks);
        e = malloc(sizeof(struct expr));
        e = expr;
        for (;;) {
                if (toks->type == op0) {
                        expr = fun(++toks);
                        e = binop(e, expr, op1);
                } else if (toks->type == op2) {
                        expr = fun(toks);
                        e = binop(e, expr, op3);
                } else {
                        return e;
                }
        }

}

Expr *
parse_mul()
{
        return parse_op(parse_expr, TIMES, OP_TIMES, DIVISE, OP_DIVISE);
}

Expr *
parse_add()
{
        return parse_op(parse_mul, PLUS, OP_PLUS, MINUS, OP_MINUS);
}

Decl
parse_top_level()
{
        Decl decl;

        if (toks->type == IDE) {
                if ((++toks)->type == LPARENT) {
                        struct elist args;
                        struct elist *pt;
                        decl.fun_decl.name = (toks - 1)->str;
                        pt = &args;
                        ++toks;
                        while (toks->type != RPARENT) {
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = *parse_expr();
                                if (pt->next->expr.type != VAR)
                                        error("Unexpected token", (toks - 1)->linum, (toks - 1)->cpos, SYNTAX_ERROR);
                                pt = pt->next;
                                if (toks->type != RPARENT)
                                        assert(make_token(COL));
                        } ++toks;
                        assert(make_token(ARR));
                        decl.type = FUN_DECL;
                        decl.fun_decl.args = args.next;
                        decl.fun_decl.body = parse_body();
                } else if ((toks)->type == EQUAL) {
                        decl.var_decl.name = (toks - 1)->str;
                        ++toks;
                        decl.type = VAR_DECL;
                        decl.var_decl.body = parse_body();
                } else error("Unexpected token.", toks->linum, toks->cpos,
                             SYNTAX_ERROR);
        } else error("Unexpected token.", toks->linum, toks->cpos, SYNTAX_ERROR);
        return decl;
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
        case TINT:
        case TLIT: l = NULL; break;
        case TVAR:
                l       = malloc(sizeof(struct ilist));
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
                s = malloc(sizeof(Subst));
                s->t = t;
                s->next = NULL;
                s->nvar = var;
        }
        return s;
}

Type *
tfun(Type *left, Type *right)
{
        Type *t;

        t = malloc(sizeof(Type));
        t->type = TFUN;
        t->fun.right = right;
        t->fun.left = left;
        return t;
}

Type *
tint()
{
        Type *t;

        t = malloc(sizeof(Type));
        t->type = TINT;
        return t;
}

Type *
tvar (unsigned int var)
{
        Type *t;

        t = malloc(sizeof(Type));
        t->var = var;
        t->type = TVAR;
        return t;
}

Type *
app_subst(Type *t, Subst *s)
{

        if (t->type == TLIT || t->type == TINT) return t;
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
                if (occurs(sch.bind, s->nvar)) {
                        if (s->next) {
                                s->nvar = s->next->nvar;
                                s->t = s->next->t;
                                s->nvar = s->next->nvar;
                        }
                        else s->nvar = -1;
                        continue;
                }
                p = p->next;
        }
        sch.type = app_subst(sch.type, s);
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
                }
                p = p->next;

        }
        return s2;
}

Subst *
unify(Type *t1, Type *t2)
{
        Subst *s;
        if (t1->type == TLIT && t2->type == TLIT && !strcmp(t2->lit, t1->lit))
                s = NULL;
        else if (t1->type == TINT && t2->type == TINT) s = NULL;
        else if (t1->type == TVAR) s = bind(t1->var, t2);
        else if (t2->type == TVAR) s = bind(t2->var, t1);
        else if (t1->type == TFUN && t2->type == TFUN)
                s = compose_subst(unify(t1->fun.right, t2->fun.right),
                              unify(t1->fun.left, t2->fun.left));
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
        t = malloc(sizeof(Type));
        *t = *sch.type;
        while (p) {
                Subst s;
                s.next = NULL;
                s.nvar = p->i;
                s.t = tvar(++nvar);
                p = p->next;
                t = app_subst(t, &s);
        }
        return t;
}

Scheme
gen(Type *t)
{
        Scheme sch;

        sch.type = t;
        sch.bind = ftv(t);
        return sch;
}

Context *
add_dummy_var(Type *t, struct elist *args, Context *ctx)
{
        Context *nctx;

        while (args->next) args = args->next;
        args->next = malloc(sizeof(struct elist));
        args->next->expr.var = "@";
        args->next->expr.type = VAR;
        nctx = malloc(sizeof(Context));
        nctx->sch.type = t;
        nctx->sch.bind = NULL;
        nctx->name = "@";
        nctx->next = ctx;
        return nctx;
}

char *
decl_name(Decl decl)
{

        if (decl.type == VAR_DECL) return decl.var_decl.name;
        return decl.fun_decl.name;
}

TypeReturn
infer(struct expr expr, Context *ctx)
{
        TypeReturn tp;

        tp.subst = NULL;
        switch (expr.type) {
        case INT:
                tp.type = tint();
                break;
        case VAR:
                tp.type = inst(find_ctx(expr.var, ctx));
                break;
        case BINOP: {
                TypeReturn r = infer(*expr.binop.right, ctx);
                app_subst_ctx(r.subst, ctx);
                TypeReturn l = infer(*expr.binop.left, ctx);
                Subst *s = compose_subst(r.subst, l.subst);
                s = compose_subst(s, unify(l.type, tint()));
                s = compose_subst(s, unify(r.type, tint()));
                tp.subst = s;
                tp.type = tint();
                break;
        }
        case FUN_CALL: {
                TypeReturn ft = infer(*expr.fun_call.fun, ctx);
                app_subst_ctx(ft.subst, ctx);
                TypeReturn args = infer_args(expr.fun_call.args, ctx);
                int save_nvar = nvar + 1;
                Context *nctx = add_dummy_var(tvar(++nvar), expr.fun_call.args, ctx);
                TypeReturn at = infer_args(expr.fun_call.args, nctx);
                tp.type = app_subst(tvar(save_nvar), unify(at.type, ft.type));
                tp.subst = compose_subst(at.subst, ft.subst);
                break;
        }
        case LETIN: {
                struct decllist *p = expr.letin.decl;
                while (p) {
                        Context *nctx = malloc(sizeof(struct decllist));
                        TypeReturn dt = infer_decl(p->decl, ctx);
                        nctx->name = decl_name(p->decl);
                        nctx->sch = gen(dt.type);
                        nctx->next = ctx;
                        ctx = nctx;
                        p = p->next;
                }
                tp = infer_body(expr.letin.expr, ctx);
        }
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
                } else {
                        tp.type  = arg.type;
                        tp.subst = arg.subst;
                }
        }
        return tp;

}

TypeReturn
infer_decl(struct decl decl, Context *ctx)
{
        TypeReturn tp;

        tp.subst = NULL;
        switch (decl.type) {
        case VAR_DECL:
                tp = infer_body(decl.var_decl.body, ctx);
                break;
        case FUN_DECL: {
                struct elist *p = decl.fun_decl.args;
                while (p) {
                        Context *nctx = malloc(sizeof(Context));
                        nctx->name = p->expr.var;
                        nctx->sch.bind = NULL;
                        nctx->sch.type = tvar(++nvar);
                        nctx->next = ctx;
                        ctx = nctx;
                        p = p->next;
                } TypeReturn bt = infer_body(decl.fun_decl.body, ctx);
                app_subst_ctx(bt.subst, ctx);
                tp.subst = bt.subst;
                p = decl.fun_decl.args;
                Context *nctx = add_dummy_var(bt.type, decl.fun_decl.args, ctx);
                TypeReturn at = infer_args(decl.fun_decl.args, nctx);
                tp.type = at.type;
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
        }
        return tp;
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
print_slist(struct slist *slist, int tab)
{

        while (slist) {
                print_tab(tab);
                printf("%s\n", slist->str);
                slist = slist->next;
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

char
op_to_char(unsigned int op)
{

        switch (op) {
        case OP_PLUS:   return '+';
        case OP_MINUS:  return '-';
        case OP_TIMES:  return '*';
        case OP_DIVISE: return '/';
        }
        return ' ';
}

void
print_token(Token t)
{
        switch(t.type) {
        case IN:      printf("in");                    break;
        case LET:     printf("let");                   break;
        case NUM:     printf("number: %d", t.num);     break;
        case IDE:     printf("identifier: %s", t.str); break;
        case STR:     printf("string: %s", t.str);     break;
        case COL:     printf(",");                     break;
        case ARR:     printf("->");                    break;
        case PLUS:    printf("+");                     break;
        case EQUAL:   printf("=");                     break;
        case MINUS:   printf("-");                     break;
        case TIMES:   printf("*");                     break;
        case LPARENT: printf("(");                     break;
        case DIVISE:  printf("/");                     break;
        case RPARENT: printf(")");                     break;
        case SEMICOL: printf(";");                     break;
        case END:                                      break;
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
                printf("binop: %c\n", op_to_char(expr.binop.op));
                print_expr(*expr.binop.left, tab + 2);
                print_expr(*expr.binop.right, tab + 2);
                break;
        }
}

void
print_decl(struct decl decl, int tab)
{

        print_tab(tab);
        switch (decl.type) {
        case FUN_DECL:
                printf("function: %s\n", decl.fun_decl.name);
                print_elist(*decl.fun_decl.args, tab + 2);
                print_elist(*decl.fun_decl.body, tab + 2);
                break;
        case VAR_DECL:
                printf("variable: %s\n", decl.var_decl.name);
                print_elist(*decl.var_decl.body, tab + 2);
                break;
        }
}

void
print_type(Type t)
{

        switch (t.type) {
        case TINT:
                printf("int");
                break;
        case TFUN:
                print_type(*t.fun.left);
                printf(" -> ");
                print_type(*t.fun.right);
                break;
        case TLIT:
                printf("%s", t.lit);
                break;
        case TVAR:
                printf("%d", t.var);
                break;
        }
}

#define NREG 7

int used_registers[NREG] = {
        0,
        0,
        0,
        0,
        0,
        0,
        0
};

char *registers[] = {
        "rcx",
        "rdx",
        "rsi",
        "r8",
        "r9",
        "r10",
        "r11"
};

BSSTable *bss_table;

void
add_bss(char *name, int size)
{
        BSSTable *nbss_table;

        nbss_table = malloc(sizeof(BSSTable));
        nbss_table->name = malloc(64);
        strncpy(nbss_table->name, name, 64);
        nbss_table->size = size;
        nbss_table->next = bss_table;
        bss_table = nbss_table;
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

int ndecl = -1;
struct decllist *decls = NULL;

SContext *
add_sctx(SContext *ctx, char *name, int num)
{
        SContext *nctx;

        nctx = malloc(sizeof(SContext));
        nctx->next = ctx;
        nctx->name = name;
        nctx->num = num;
        return nctx;
}

char *
compile_expr(Expr e, SContext *ctx)
{

        switch (e.type) {
        case INT: {
                int reg = alloc_reg();
                printf("mov %s, %d\n", registers[reg], e.num);
                return registers[reg];
                break;
        }
        case BINOP: {
                char *regl = compile_expr(*e.binop.left, ctx);
                char *regr = compile_expr(*e.binop.right, ctx);
                switch (e.binop.op) {
                case OP_PLUS:
                        printf("add %s, %s\n", regr, regl);
                        break;
                case OP_MINUS:
                        printf("sub %s, %s\n", regr, regl);
                        break;
                case OP_TIMES:
                        printf("imul %s, %s\n", regr, regl);
                        break;
                case OP_DIVISE:
                        printf("idiv %s, %s\n", regr, regl);
                        break;
                } free_reg(regl);
                return regr;
                break;
        }
        case VAR:
                while (ctx) {
                        if (!strcmp(e.var, ctx->name)) {
                                int reg = alloc_reg();
                                printf("mov %s, [rsp + %d]\n",
                                       registers[reg], (nvar - ctx->num) * 8);
                                return registers[reg];
                        } ctx = ctx->next;
                } int reg = alloc_reg();
                printf("mov %s, [%s]\n", registers[reg], e.var);
                return registers[reg];
                break;
        case LETIN: {
                struct decllist *p = e.letin.decl;
                int length = 0;
                while (p) {
                        char s[64];
                        sprintf(s, "__decl%d", ++ndecl);
                        compile_decl(p->decl, ctx, s);
                        ctx = add_sctx(ctx, decl_name(p->decl), ++nvar);
                        printf("push QWORD [%s]\n", s);
                        p = p->next;
                        ++length;
                } char *reg = compile_body(e.letin.expr, ctx);
                printf("sub rsp, %d\n", length << 3);
                return reg;
        }
        case FUN_CALL: {
                char *fun = compile_expr(*e.fun_call.fun, ctx);
                int length = 0;
                struct elist *p = e.fun_call.args;
                while (p) {
                        printf("push %s\n", compile_expr(p->expr, ctx));
                        p = p->next;
                        ++length;
                } printf("call %s\n", fun);
                printf("sub rsp, %d\n", length << 3);
                return "rax";
        }
        }
        return "";
}

char *
compile_body(struct elist *body, SContext *ctx)
{
        char *s;

        while (body) {
                s = compile_expr(body->expr, ctx);
                body = body->next;
        } return s;
}

void
compile_decl(Decl decl, SContext *ctx, char *name)
{

        add_bss(name, 8);
        if (decl.type == VAR_DECL) {
                char *reg = compile_body(decl.var_decl.body, ctx);
                printf("mov [%s], %s\n", name, reg);
                free_reg(reg);
        } else {
                char label[64];
                struct elist *p = decl.fun_decl.args;
                sprintf(label, "__decl%d", ++ndecl);
                printf("jmp %s\n"
                       "_%s:\n", label, name);
                while (p) {
                        ctx = add_sctx(ctx, p->expr.var, ++nvar);
                        p = p->next;
                }  ++nvar;
                char *reg = compile_body(decl.fun_decl.body, ctx);
                printf("mov rax, %s\n"
                       "ret\n"
                       "%s:\n", reg, label);
                printf("mov QWORD [%s], QWORD _%s\n", name, name);
                free_reg(reg);
                --nvar;
        }
}

void
compile_bss()
{

        printf("section .bss\n");
        while (bss_table) {
                printf("%s: resb %d\n", bss_table->name, bss_table->size);
                bss_table = bss_table->next;
        }
}

char *prelude =
        "global _start\n"
        "section .text\n"
        "_start:\n";

char *conclusion =
        "mov rax, 1\n"
        "int 80h\n";

int
main(int argc, char **argv)
{

        printf("%s", prelude);
        toks = lexer("let id(a)->a in id(3)");
        printf("mov rbx, %s\n", compile_expr(*parse_add(), NULL));
        printf("%s", conclusion);
        compile_bss();
        return 0;
}
