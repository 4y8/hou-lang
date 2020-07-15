#ifndef __HOU_H_
#define __HOU_H_

typedef struct {
        enum { LPARENT, RPARENT, IDE, NUM, STR, EQUAL, SEMICOL, COL,
               END, ARR, LET, IN, PLUS, MINUS, TIMES, DIVISE } type;
        union {
                int   num;
                char *str;
        };
        int cpos;
        int abspos;
        int linum;
} Token;

typedef struct {
        unsigned int t;
        char *s;
} char_to_tok;

struct expr {
        union {
                struct {
                        struct decllist *decl;
                        struct elist *expr;
                } letin;
                struct {
                        char *name;
                        struct elist *args;
                } fun_call;
                struct {
                        struct expr *left;
                        struct expr *right;
                        enum { OP_PLUS, OP_MINUS, OP_TIMES, OP_DIVISE } op;
                } binop;
                union {
                        char *var;
                        int deb_var;
                };
                int num;
        };
        enum { FUN_CALL, INT, VAR, LETIN, BINOP } type;
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
        struct expr *expr;
        Token *tokens;
} Parser;

typedef struct {
        struct elist *body;
        Token *tokens;
} BodyParser;

struct decl {
        union {
                struct {
                        char *name;
                        struct elist *args;
                        struct elist *body;
                } fun_decl;
                struct {
                        char *name;
                        struct elist *body;
                } var_decl;
        };
        enum { FUN_DECL, VAR_DECL } type;
};

struct decllist {
        struct decl decl;
        struct decllist *next;
};

typedef struct {
        struct decl decl;
        Token *tokens;
} TopParser;

struct type {
        union {
                char *lit;
                unsigned int var;
                struct {
                        struct type *left;
                        struct type *right;
                } fun;
        };
        enum { TLIT, TVAR, TFUN, TINT } type;
};

typedef struct type Type;

typedef struct subst {
        unsigned int nvar;
        Type *t;
        struct subst *next;
} Subst;

struct ilist {
        unsigned int i;
        struct ilist *next;
};

typedef struct {
        struct ilist *bind;
        Type *type;
} Scheme;

typedef struct {
        Type *type;
        Subst *subst;
} TypeReturn;

typedef struct ctx_elem {
        char *name;
        Scheme sch;
        struct ctx_elem *next;
} Context;

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

Token make_token(unsigned int);
Token make_token_str(char *);
Token make_token_num(int);
Token *lexer(char *);

Parser parse_expr(Token *);
Parser parse_add(Token *);
Parser parse_mul(Token *);
TopParser parse_top_level(Token *);
BodyParser parse_body(Token *);

Type *tfun(Type *, Type *);
TypeReturn infer(struct expr, Context *);
TypeReturn infer_args(struct elist *, Context *);
TypeReturn infer_decl(struct decl, Context *);
TypeReturn infer_body(struct elist *, Context *);

Subst *unify(Type *, Type *);
Subst *bind(unsigned int, Type *);

void print_token(Token);
void assert(Token *, Token);
void print_expr(struct expr, int);
void print_elist(struct elist, int);
void error(char *, int, int, Error);
void print_decl(struct decl, int);
void print_decllist(struct decllist *, int);
void print_type(Type);

#endif // __HOU_H_
