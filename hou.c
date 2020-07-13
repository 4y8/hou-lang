#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "hou.h"

#define TOKEN_SIZE 10

int linum;
int cpos;

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
                        *(tokens + (++tpos)) = make_token_str(str);
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
                        case '=': *(tokens + (++tpos)) = make_token(EQUAL);   break;
                        case '(': *(tokens + (++tpos)) = make_token(LPARENT); break;
                        case ')': *(tokens + (++tpos)) = make_token(RPARENT); break;
                        case ';': *(tokens + (++tpos)) = make_token(SEMICOL); break;
                        case ' ': case '\t': break;
                        case '\n': ++linum; cpos = 0; break;
                        case '-':
                                if (*(++s) == '>') {
                                        *(tokens + (++tpos)) = make_token(ARR);
                                        ++cpos;
                                        break;
                                } else /* FALLTHROUGH */;
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
print_token(Token t)
{
        switch(t.type) {
        case NUM:     printf("number: %d", t.num);     break;
        case IDE:     printf("identifier: %s", t.str); break;
        case STR:     printf("string: %s", t.str);     break;
        case COL:     printf(",");                     break;
        case ARR:     printf("->");                    break;
        case EQUAL:   printf("=");                     break;
        case LPARENT: printf("(");                     break;
        case RPARENT: printf(")");                     break;
        case SEMICOL: printf(";");                     break;
        case END:                                      break;
    }
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

        p.expr.linum = tokens->linum;
        p.expr.cpos  = tokens->cpos;
        p.expr.abspos  = tokens->abspos;
        switch (tokens->type) {
        case IDE:
                switch ((tokens + 1)->type) {
                case EQUAL: {
                        Parser b;
                        b = parse_expr(tokens + 2);
                        p.tokens = b.tokens;
                        p.expr.type = DECL;
                        p.expr.decl.name = tokens->str;
                        p.expr.decl.body = &b.expr;
                        break;
                }
                case LPARENT: {
                        struct elist args;
                        struct elist *pt;
                        Token *tp;
                        pt = &args;
                        tp = tokens + 2;
                        while (tp->type != RPARENT) {
                                Parser b = parse_expr(tp);
                                pt->next = malloc(sizeof(struct elist));
                                pt->next->expr = b.expr;
                                pt = pt->next;
                                tp = b.tokens;
                                if (tp->type != RPARENT) {
                                        assert(tp, make_token(COL));
                                        ++tp;
                                }
                        } p.expr.type = FUN_CALL;
                        p.tokens = tp;
                        p.expr.fun_call.name = tokens->str;
                        p.expr.fun_call.args = args.next;
                        break;
                }
                default:
                        p.tokens = tokens + 1;
                        p.expr.type = VAR;
                        p.expr.var = tokens->str;
                        break;
                }
                break;
        case NUM:
                p.tokens = tokens + 1;
                p.expr.type = INT;
                p.expr.num = tokens->num;
                break;
        default: break;
        }
        return p;
}

TopParser
parse_top_level(Token *tokens)
{
        TopParser p;

        if (tokens->type == IDE) {
                if ((tokens + 1)->type == LPARENT) {
                        struct slist args;
                        struct slist *pt;
                        struct slist *expt;
                        struct elist body;
                        struct elist *bp;
                        struct elist *exbp;
                        unsigned int loop;
                        Token *tp;
                        pt = &args;
                        tp = tokens + 2;
                        while (tp->type != RPARENT) {
                                pt->next = malloc(sizeof(struct slist));
                                assert(tp, make_token(IDE));
                                pt->next->str = tp->str;
                                pt = pt->next;
                                if ((++tp)->type != RPARENT)
                                        assert(tp, make_token(COL));
                        } loop = 1;
                        bp = &body;
                        assert(++tp, make_token(ARR));
                        ++tp;
                        while (loop) {
                                Parser p = parse_expr(tp);
                                bp->next = malloc(sizeof(struct elist));
                                bp->next->expr = p.expr;
                                tp = p.tokens;
                                bp = bp->next;
                                puts("ee");
                                if (tp->type == SEMICOL) ++tp;
                                else loop = 0;
                        } p.decl.type = FUN_DECL;
                        p.tokens = tp;
                        p.decl.fun_decl.name = tokens->str;
                        p.decl.fun_decl.args = args.next;
                        p.decl.fun_decl.body = body.next;
                }
        } else error("Unexpected token.", tokens->linum, tokens->cpos, SYNTAX_ERROR);
        return p;
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
print_expr(struct expr expr, int tab)
{

        print_tab(tab);
        switch (expr.type) {
        case NUM:
                printf("number: %d\n", expr.num);
                break;
        case FUN_CALL:
                printf("function call: %s\n", expr.fun_call.name);
                print_elist(*expr.fun_call.args, tab + 2);
                break;
        case VAR:
                printf("variable: %s\n", expr.var);
                break;
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
        default: break;
        }
}

Parser p;
int
main(int argc, char **argv)
{

        p = parse_expr(lexer("fib(a, 2)"));
        print_decl(parse_top_level(lexer("fib(a)->2;2")).decl, 0);
        print_expr(p.expr, 0);
        return 0;
}
