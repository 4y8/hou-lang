#ifndef __HOU_H_
#define __HOU_H_

typedef struct {
        enum { LPARENT, RPARENT, IDE, NUM, STR, EQUAL, SEMICOL, COL, END, ARR } type;
        union {
                int   num;
                char *str;
        };
        int cpos;
        int abspos;
        int linum;
} Token;


struct expr {
        union {
                struct {
                        char *name;
                        struct expr *body;
                } decl;
                struct {
                        char *name;
                        struct elist *args;
                } fun_call;
                char *var;
                int num;
        };
        enum { IF, DECL, FUN_CALL, INT, VAR } type;
        int cpos;
        int abspos;
        int linum;
};

struct elist {
        struct expr expr;
        struct elist *next;
};

struct slist {
        char *str;
        struct slist *next;
};

typedef struct {
        struct expr expr;
        Token *tokens;
} Parser;

struct decl {
        union {
                struct {
                        char *name;
                        struct slist *args;
                        struct elist *body;
                } fun_decl;
        };
        enum { FUN_DECL, VAR_DECL } type;
};

typedef struct {
        struct decl decl;
        Token *tokens;
} TopParser;

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

Token make_token(unsigned int);
Token make_token_str(char *);
Token make_token_num(int);
Token *lexer(char *);
void print_token(Token);
Parser parse_expr(Token *);
void assert(Token *, Token);
void print_expr(struct expr, int);
void print_elist(struct elist, int);
void error(char *, int, int, Error);


#endif // __HOU_H_
