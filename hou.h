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
            struct expr *args;
        } fun_call;
    };
    enum { IF, DECL, FUN_CALL } type;
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

#endif // __HOU_H_
