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

Parser
parse_expr()
{
        Parser p;

        p.expr = malloc(sizeof(struct expr));
        p.expr->linum = toks->linum;
        p.expr->cpos  = toks->cpos;
        p.expr->abspos  = toks->abspos;
        switch (toks->type) {
        case IDE:
                switch ((++toks)->type) {
                case LPARENT: {
                        struct elist args;
                        struct elist *pt;
                        p.expr->fun_call.name = (toks - 1)->str;
                        Token *tp;
                        pt = &args;
                        ++toks;
                        while (toks->type != RPARENT) {
                                Parser b = parse_expr(tp);
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = *b.expr;
                                pt = pt->next;
                                if (toks->type != RPARENT) assert(make_token(COL));
                        } pt->next = NULL;
                        p.expr->type = FUN_CALL;
                        ++toks;
                        p.expr->fun_call.args = args.next;
                        break;
                }
                default:
                        p.expr->type = VAR;
                        p.expr->var = (toks - 1)->str;
                        break;
                }
                break;
        case NUM:
                p.expr->type = INT;
                p.expr->num = toks->num;
                ++toks;
                break;
        case LET: {
                struct decllist l;
                struct decllist *lp;
                BodyParser b;
                lp = &l;
                ++toks;
                while (toks->type != IN) {
                        TopParser b = parse_top_level(toks);
                        lp->next = malloc(sizeof(struct decllist));
                        lp->next->decl = b.decl;
                        lp = lp->next;
                } ++toks;
                lp->next = NULL;
                b = parse_body(toks);
                p.expr->letin.decl = l.next;
                p.expr->letin.expr = b.body;
                p.expr->type = LETIN;
                break;
        }
        default:
                error("Unexpected token.", toks->linum, toks->cpos,
                      SYNTAX_ERROR);
                break;
        }
        return p;
}

BodyParser
parse_body()
{
        struct elist body;
        struct elist *bp;
        unsigned int loop;
        BodyParser p;

        loop = 1;
        bp = &body;
        while (loop) {
                Parser p = parse_add(toks);
                bp->next = malloc(sizeof(struct elist));
                bp->next->expr = *p.expr;
                bp = bp->next;
                if (toks->type == SEMICOL) ++toks;
                else loop = 0;
        } bp->next = NULL;
        p.body = body.next;
        return p;
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

Parser
parse_op(Parser (*fun)(Token *), unsigned int op0,
          unsigned int op1, unsigned int op2, unsigned int op3)
{
        Parser p;
        struct expr *e;

        p = fun(toks);
        e = malloc(sizeof(struct expr));
        e = p.expr;
        for (;;) {
                if (toks->type == op0) {
                        p = fun(++toks);
                        e = binop(e, p.expr, op1);
                } else if (toks->type == op2) {
                        p = fun(toks);
                        e = binop(e, p.expr, op3);
                } else {
                        p.expr = e;
                        return p;
                }
        }

}

Parser
parse_mul()
{
        return parse_op(parse_expr, TIMES, OP_TIMES, DIVISE, OP_DIVISE);
}

Parser
parse_add()
{
        return parse_op(parse_mul, PLUS, OP_PLUS, MINUS, OP_MINUS);
}


TopParser
parse_top_level()
{
        TopParser p;

        if (toks->type == IDE) {
                if ((++toks)->type == LPARENT) {
                        struct elist args;
                        struct elist *pt;
                        BodyParser bp;
                        p.decl.fun_decl.name = (toks - 1)->str;
                        pt = &args;
                        ++toks;
                        while (toks->type != RPARENT) {
                                Parser ap = parse_expr();
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = *ap.expr;
                                pt = pt->next;
                                if (toks->type != RPARENT)
                                        assert(make_token(COL));
                        } ++toks;
                        assert(make_token(ARR));
                        bp = parse_body();
                        p.decl.type = FUN_DECL;
                        p.decl.fun_decl.args = args.next;
                        p.decl.fun_decl.body = bp.body;
                } else if ((toks)->type == EQUAL) {
                        p.decl.var_decl.name = (toks - 1)->str;
                        BodyParser bp;
                        ++toks;
                        bp = parse_body();
                        p.decl.type = VAR_DECL;
                        p.decl.var_decl.body = bp.body;
                } else error("Unexpected token.", toks->linum, toks->cpos,
                             SYNTAX_ERROR);
        } else error("Unexpected token.", toks->linum, toks->cpos, SYNTAX_ERROR);
        return p;
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
                if (p->next == NULL)
                        p->next = s1;
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
        else if (t1->type == TVAR) {
                s = bind(t1->var, t2);
        }
        else if (t2->type == TVAR) s = bind(t2->var, t1);
        else if (t1->type == TFUN && t2->type == TFUN)
                compose_subst(unify(t1->fun.right, t2->fun.right),
                              unify(t1->fun.left, t2->fun.left));

        return s;
}

Scheme
find_ctx(char *name, Context *ctx)
{

        while(ctx) {
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
                Type *t = inst(find_ctx(expr.fun_call.name, ctx));
                TypeReturn args = infer_args(expr.fun_call.args, ctx);
                struct elist *p = expr.fun_call.args;
                break;
        }
        case LETIN: {
                struct decllist *p = expr.letin.decl;
                while (p) {
                        Context *nctx = malloc(sizeof(struct decllist));
                        TypeReturn dt = infer_decl(p->decl, ctx);
                        if (p->decl.type == VAR_DECL)
                                nctx->name = p->decl.var_decl.name;
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
                }
                TypeReturn bt = infer_body(decl.fun_decl.body, ctx);
                app_subst_ctx(bt.subst, ctx);
                tp.subst = bt.subst;
                p = decl.fun_decl.args;
                while(p->next) p = p->next;
                p->next = malloc(sizeof(struct elist));
                p->next->expr.var = "@";
                p->next->expr.type = VAR;
                Context *nctx = malloc(sizeof(Context));
                nctx->sch.type = bt.type;
                nctx->sch.bind = NULL;
                nctx->name = "@";
                nctx->next = ctx;
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
                printf("function call: %s\n", expr.fun_call.name);
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
        default: break;
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

int
main(int argc, char **argv)
{

        toks = lexer("id(a)->a;3");
        print_type(*infer_decl(parse_top_level().decl, NULL).type);
        puts("");
        toks = lexer("let a=2 b=3 in a + b");
        print_type(*infer(*parse_mul().expr, NULL).type);
        puts("");
        return 0;
}
