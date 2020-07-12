#ifndef __HOU_H_
#define __HOU_H_

typedef struct {
    enum { LPARENT, RPARENT, IDE, NUM, STR, EQUAL, SEMICOL, COL, END } type;
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

typedef struct {
    struct expr expr;
    Token *tokens;
} Parser;

struct decl {
    union {

    };
};

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

Parser parse_expr(Token *);
Parser parse_til(Token *, Token);

#endif // __HOU_H_
