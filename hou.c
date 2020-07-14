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

void
assert(Token *tokens, Token token)
{

        if (tokens->type != token.type)
                error("Unexpected token", tokens->linum, tokens->cpos,
                      SYNTAX_ERROR);
}                   

Parser
parse_expr(Token *tokens)
{
        Parser p;

        p.expr = malloc(sizeof(struct expr));
        p.expr->linum = tokens->linum;
        p.expr->cpos  = tokens->cpos;
        p.expr->abspos  = tokens->abspos;
        switch (tokens->type) {
        case IDE:
                switch ((tokens + 1)->type) {
                case LPARENT: {
                        struct elist args;
                        struct elist *pt;
                        Token *tp;
                        pt = &args;
                        tp = tokens + 2;
                        while (tp->type != RPARENT) {
                                Parser b = parse_expr(tp);
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = *b.expr;
                                pt = pt->next;
                                tp = b.tokens;
                                if (tp->type != RPARENT) {
                                        assert(tp, make_token(COL));
                                        ++tp;
                                }
                        } pt->next = NULL;
                        p.expr->type = FUN_CALL;
                        p.tokens = tp + 1;
                        p.expr->fun_call.name = tokens->str;
                        p.expr->fun_call.args = args.next;
                        break;
                }
                default:
                        p.tokens = tokens + 1;
                        p.expr->type = VAR;
                        p.expr->var = tokens->str;
                        break;
                }
                break;
        case NUM:
                p.tokens = tokens + 1;
                p.expr->type = INT;
                p.expr->num = tokens->num;
                break;
        case LET: {
                struct decllist l;
                struct decllist *lp;
                BodyParser b;
                lp = &l;
                ++tokens;
                while (tokens->type != IN) {
                        TopParser b = parse_top_level(tokens);
                        lp->next = malloc(sizeof(struct decllist));
                        lp->next->decl = b.decl;
                        lp = lp->next;
                        tokens = b.tokens;
                } ++tokens;
                lp->next = NULL;
                b = parse_body(tokens);
                tokens = b.tokens;
                p.expr->letin.decl = l.next;
                p.expr->letin.expr = b.body;
                p.expr->type = LETIN;
                p.tokens = tokens;
                break;
        }
        default:
                error("Unexpected token.", tokens->linum, tokens->cpos,
                      SYNTAX_ERROR);
                break;
        }
        return p;
}

BodyParser
parse_body(Token *tokens)
{
        struct elist body;
        struct elist *bp;
        unsigned int loop;
        BodyParser p;

        loop = 1;
        bp = &body;
        while (loop) {
                Parser p = parse_add(tokens);
                bp->next = malloc(sizeof(struct elist));
                bp->next->expr = *p.expr;
                tokens = p.tokens;
                bp = bp->next;
                if (tokens->type == SEMICOL) ++tokens;
                else loop = 0;
        } bp->next = NULL;
        p.tokens = tokens;
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
parse_op(Token *tokens, Parser (*fun)(Token *), unsigned int op0,
          unsigned int op1, unsigned int op2, unsigned int op3)
{
        Parser p;
        struct expr *e;

        p = fun(tokens);
        tokens = p.tokens;
        e = malloc(sizeof(struct expr));
        e = p.expr;
        for (;;) {
                if (tokens->type == op0) {
                        p = fun(++tokens);
                        tokens = p.tokens;
                        e = binop(e, p.expr, op1);
                } else if (tokens->type == op2) {
                        p = fun(tokens);
                        tokens = p.tokens;
                        e = binop(e, p.expr, op3);
                } else {
                        p.expr = e;
                        return p;
                }
        }

}

Parser
parse_mul(Token *tokens)
{
        return parse_op(tokens, parse_expr, TIMES, OP_TIMES, DIVISE, OP_DIVISE);
}

Parser
parse_add(Token *tokens)
{
        return parse_op(tokens, parse_mul, PLUS, OP_PLUS, MINUS, OP_MINUS);
}


TopParser
parse_top_level(Token *tokens)
{
        TopParser p;

        if (tokens->type == IDE) {
                if ((tokens + 1)->type == LPARENT) {
                        struct slist args;
                        struct slist *pt;
                        BodyParser bp;
                        Token *tp;
                        pt = &args;
                        tp = tokens + 2;
                        while (tp->type != RPARENT) {
                                pt->next = malloc(sizeof(struct slist));
                                assert(tp, make_token(IDE));
                                pt->next->str = tp->str;
                                pt = pt->next;
                                if ((++tp)->type != RPARENT) {
                                        assert(tp, make_token(COL));
                                        ++tp;
                                }
                        } assert(++tp, make_token(ARR));
                        bp = parse_body(++tp);
                        p.decl.type = FUN_DECL;
                        p.tokens = bp.tokens;
                        p.decl.fun_decl.name = tokens->str;
                        p.decl.fun_decl.args = args.next;
                        p.decl.fun_decl.body = bp.body;
                } else if ((tokens + 1)->type == EQUAL) {
                        BodyParser bp;
                        bp = parse_body(tokens + 2);
                        p.decl.type = VAR_DECL;
                        p.decl.var_decl.name = tokens->str;
                        p.decl.var_decl.body = bp.body;
                        p.tokens = bp.tokens;
                } else error("Unexpected token.", tokens->linum, tokens->cpos,
                             SYNTAX_ERROR);
        } else error("Unexpected token.", tokens->linum, tokens->cpos, SYNTAX_ERROR);
        return p;
}

struct ilist*
ftv(Type t)
{
        struct ilist *l;

        switch (t.type) {
        case TFUN:
                l = ftv(*t.fun.left);
                struct ilist *p = l;
                while (p) p=p->next;
                p = ftv(*t.fun.right);
                break;
        case TLIT: return NULL;
        case TVAR:
                l       = malloc(sizeof(struct ilist));
                l->next = NULL;
                l->i    = t.var;
                break;
        } return l;
}

struct ilist*
ftv_sch(Scheme sch)
{
        struct ilist *l;
        unsigned int *i;
        unsigned int *ip;
        struct ilist *p;
        unsigned int len;

        p = sch.bind;
        while (p) {
                p = p->next;
                ++len;
        }
        p = sch.bind;
        ip = i = malloc(len * sizeof(unsigned int));
        --i;
        while (p) {
                *(++i) = p->i;
                p = p->next;
        } p = l = ftv(sch.type);
        while (p) {
                p = p->next;
        }
        free(i);
        return l;
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
                print_slist(decl.fun_decl.args, tab + 2);
                print_elist(*decl.fun_decl.body, tab + 2);
                break;
        case VAR_DECL:
                printf("variable: %s\n", decl.var_decl.name);
                print_elist(*decl.var_decl.body, tab + 2);
                break;
        }
}

int
main(int argc, char **argv)
{

        print_decl(parse_top_level(lexer("add(a, b)->let fib(a) -> 2 in a + b")).decl, 0);
        return 0;
}
