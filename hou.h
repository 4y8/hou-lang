#ifndef __HOU_H_
#define __HOU_H_

typedef struct {
        enum { LPARENT, RPARENT, IDE, NUM, STR, EQUAL, SEMICOL, COL,
               END, ARR, LET, IN, PLUS, MINUS, TIMES, DIVISE, DOT, IF,
               ELSE, ELIF, GREAT, LOW, EXTERN, BACKS, EXCLAM, OR, TYPE,
               MOD, INFIXL, INFIXR } type;
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
} KeywordToken;

typedef struct {
        unsigned int t;
        char c;
} PuncToken;

typedef struct op_table {
        unsigned int tok;
        unsigned int op;
} OpTable;

typedef struct expr {
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
                        enum { OP_PLUS, OP_MINUS, OP_TIMES, OP_DIVISE, OP_LOW,
                               OP_GREAT, OP_LOWE, OP_GREATE, OP_EQUAL, OP_NEQUAL,
                               OP_MOD} op;
                } binop;
                struct {
                        struct expr *condition;
                        struct elist *if_expr;
                        struct elist *else_expr;
                } if_clause;
                struct decl *lam;
                char *var;
                int num;
        };
        enum { FUN_CALL, INT, VAR, LETIN, BINOP, IF_CLAUSE, LAM } type;
        int cpos;
        int abspos;
        int linum;
} Expr;

typedef struct elist {
        struct expr expr;
        struct elist *next;
} EList;

typedef struct decl {
        union {
                struct {
                        EList *args;
                        EList *body;
                } fun_decl;
                EList *var_decl;
        };
        char *name;
        enum { FUN_DECL, VAR_DECL } type;
} Decl;

typedef struct decllist {
        Decl decl;
        struct decllist *next;
} DeclList;

typedef struct typedecl {
        char *name;
        EList *args;
} TypeDecl;

typedef struct typedecllist {
        char *s;
        struct slist *next;
} TDeclList;

typedef struct type {
        union {
                char *lit;
                unsigned int var;
                struct {
                        struct type *left;
                        struct type *right;
                } fun;
        };
        enum { TLIT, TVAR, TFUN, TPAR } type;
} Type;

typedef struct slist {
        char *s;
        struct slist *next;
} SList;

typedef struct subst {
        unsigned int nvar;
        Type *t;
        struct subst *next;
} Subst;

typedef struct ilist {
        unsigned int i;
        struct ilist *next;
} Ilist;

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

typedef struct free_bss_elem {
        char *name;
        int size;
        struct free_bss_elem *next;
} FreeBSSTable;

typedef struct mem_elem {
        void *p;
        struct mem_elem *next;
} MemoryTable;

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

int punct_to_token(char);
int keyword_to_token(char *);

Token token(unsigned int);
Token token_str(char *);
Token token_num(int);
Token lexer();
Token next_token();
Token act_token();

int peek(unsigned int);

Expr *parse_expr();
Expr *parse_op(Expr *(*)(), OpTable *, int);
Expr *parse_add();
Expr *parse_mul();
Expr *parse_rel();
DeclList *parse_top_level();
EList *parse_else();
EList *parse_arg(unsigned int);
EList *parse_body();
DeclList *parse_program();

Ilist* ftv(Type *);
int occurs(Ilist *, int);
Ilist *ftv_sch(Scheme);

Context *add_ctx(Context *, char *, Scheme);
Scheme gen(Type *);

Type *tfun(Type *, Type *);
Type *tint();
Type *tvar(unsigned int);
Type *tlit();

Type *app_subst(Type *, Subst *);
Scheme app_subst_sch(Scheme, Subst *);

Subst *compose_subst(Subst *, Subst *);
Scheme find_ctx(char *, Context *);

TypeReturn infer(Expr, Context *);
TypeReturn infer_args(EList *, Context *);
TypeReturn infer_decl(Decl, Context *);
TypeReturn infer_body(EList *, Context *);
Context *infer_decls(DeclList *, Context *);

Subst *unify(Type *, Type *);
Subst *bind(unsigned int, Type *);

char *compile_expr(Expr, SContext *, char *);
char *compile_body(EList *, SContext *, char *);
void compile_decl(Decl, SContext *, char *);

void assert(unsigned int);
void error(char *, int, int, Error);

void print_token(Token);
void print_expr(Expr, int);
void print_elist(EList *, int);
void print_decl(Decl, int);
void print_decllist(DeclList *, int);
void print_type(Type);

#endif // __HOU_H_
