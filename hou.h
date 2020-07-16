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
                        struct expr *fun;
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

typedef struct expr Expr;

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

typedef struct decl Decl;

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

typedef struct sctx_elem {
        char *name;
        int num;
        struct sctx_elem *next;
} SContext;

typedef struct bss_elem {
        char *name;
        int size;
        struct bss_elem *next;
} BSSTable;

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

Token make_token(unsigned int);
Token make_token_str(char *);
Token make_token_num(int);
Token *lexer(char *);

Expr *parse_expr();
Expr *parse_add();
Expr *parse_mul();
Decl parse_top_level();
struct elist *parse_body();
struct decllist *parse_program();

Type *tfun(Type *, Type *);
TypeReturn infer(struct expr, Context *);
TypeReturn infer_args(struct elist *, Context *);
TypeReturn infer_decl(struct decl, Context *);
TypeReturn infer_body(struct elist *, Context *);

Subst *unify(Type *, Type *);
Subst *bind(unsigned int, Type *);

char *compile_expr(Expr, SContext *);
char *compile_body(struct elist *, SContext *);
void compile_decl(Decl, SContext *, char *);

void print_token(Token);
void assert(Token);
void print_expr(struct expr, int);
void print_elist(struct elist, int);
void error(char *, int, int, Error);
void print_decl(struct decl, int);
void print_decllist(struct decllist *, int);
void print_type(Type);

#endif // __HOU_H_
