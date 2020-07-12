#ifndef __HOU_H_
#define __HOU_H_

typedef struct {
    enum { LPARENT, RPARENT, IDE, NUM, STR, EQUAL, SEMICOL } type;
    union {
        int   num;
        char *str;
    };
    int cpos;
    int abspos;
    int linum;
} Token;

typedef enum { TYPE_ERROR, UNEXPECTED_CHAR, SYNTAX_ERROR } Error;

#endif // __HOU_H_
