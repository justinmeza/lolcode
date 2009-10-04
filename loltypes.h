/*
 * loltypes.h
 *
 * Implementation of types for the LOLCODE language.
 *
 * MAINTAINER
 *
 *      Justin J. Meza < justin dot meza at gmail dot com >
 *
 * TODO
 *
 *    - Create value_delete_* for each data type and implement them in
 *      lolcode.c.  Wait... why?
 *
 * LICENSE
 *
 *      Copyright (c) 2007 Justin J. Meza
 *
 *      Permission is hereby granted, free of charge, to any person obtaining a
 *      copy of this software and associated documentation files (the
 *      "Software"), to deal in the Software without restriction, including
 *      without limitation the rights to use, copy, modify, merge, publish,
 *      distribute, sublicense, and/or sell copies of the Software, and to
 *      permit persons to whom the Software is furnished to do so, subject to
 *      the following conditions:
 *
 *      The above copyright notice and this permission notice shall be included
 *      in all copies or substantial portions of the Software.
 *
 *      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *      OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *      IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *      CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *      TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *      SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef __LOLTYPES__
#define __LOLTYPES__

#include <assert.h>

/* Structures and functions required for internal implementation. These need
 * not be dealt with directly. */

enum type {
    NOOB,
    TROOF,
    NUMBR,
    NUMBAR,
    YARN,
};

enum troof {
    FAIL,
    WIN,
};

typedef int numbr;
typedef float numbar;
typedef char *yarn;
typedef enum troof troof;

struct value {
    void *data;
    enum type type;
};

/* Functions for retrieving values */

    troof
value_get_troof(struct value *VALUE)
{
    assert(VALUE);
    assert(VALUE->type == TROOF);
    return *((troof *)(VALUE->data));
}

    numbr
value_get_numbr(struct value *VALUE)
{
    assert(VALUE);
    assert(VALUE->type == NUMBR);
    return *((numbr *)(VALUE->data));
}

    numbar
value_get_numbar(struct value *VALUE)
{
    assert(VALUE);
    assert(VALUE->type == NUMBAR);
    return *((numbar *)(VALUE->data));
}

    yarn
value_get_yarn(struct value *VALUE)
{
    assert(VALUE);
    assert(VALUE->type == YARN);
    return ((yarn)(VALUE->data));
}

/* Functions for creating values */

    struct value *
value_create_noob(void)
{
    struct value *value = malloc(sizeof(struct value));
    value->type = NOOB;
    value->data = NULL;
    return value;
}

    struct value *
value_create_troof(troof DATA)
{
    struct value *value = malloc(sizeof(struct value));
    value->type = TROOF;
    value->data = malloc(sizeof(troof));
    *((troof *)(value->data)) = DATA;
    return value;
}

    struct value *
value_create_numbr(numbr DATA)
{
    struct value *value = malloc(sizeof(struct value));
    value->type = NUMBR;
    value->data = malloc(sizeof(numbr));
    *((numbr *)(value->data)) = DATA;
    return value;
}

    struct value *
value_create_numbar(numbar DATA)
{
    struct value *value = malloc(sizeof(struct value));
    value->type = NUMBAR;
    value->data = malloc(sizeof(numbar));
    *((numbar *)(value->data)) = DATA;
    return value;
}

    struct value *
value_create_yarn(yarn DATA)
{
    struct value *value = malloc(sizeof(struct value));
    value->type = YARN;
    value->data = malloc(sizeof(char) * (strlen(DATA) + 1));
    strcpy(value->data, DATA);
    return value;
}

/* Functions for casting values */

    struct value *
value_cast_noob(struct value *VALUE)
{
    return value_create_noob();
}

    struct value *
value_cast_troof(struct value *VALUE)
{
    struct value *value = NULL;
    troof data;
    assert(VALUE);
    /* NOOB to TROOF */
    /* TROOF to TROOF */
    if (VALUE->type == TROOF)
        data = value_get_troof(VALUE);
    /* NUMBR to TROOF */
    else if (VALUE->type == NUMBR) {
        if (value_get_numbr(VALUE) == 0) data = FAIL;
        else data = WIN;
    }
    /* NUMBAR to TROOF */
    if (VALUE->type == NUMBAR) {
        if (value_get_numbar(VALUE) == 0.0) data = FAIL;
        else data = WIN;
    }
    /* YARN to TROOF */
    else if (VALUE->type == YARN) {
        if (!strcmp(value_get_yarn(VALUE), "")) data = FAIL;
        else data = WIN;
    }
    /* Invalid type */
    else assert(0);
    value = malloc(sizeof(struct value));
    value->type = TROOF;
    value->data = malloc(sizeof(enum troof));
    *((troof *)(value->data)) = data;
    return value;
}

    struct value *
value_cast_numbr(struct value *VALUE)
{
    struct value *value = NULL;
    numbr data;
    assert(VALUE);
    /* NOOB to NUMBR */
    /* TROOF to NUMBR */
    if (VALUE->type == TROOF)
        data = (value_get_troof(VALUE) == WIN ? 1 : 0);
    /* NUMBR to NUMBR */
    else if (VALUE->type == NUMBR)  
        data = value_get_numbr(VALUE);
    /* NUMBAR to NUMBR */
    else if (VALUE->type == NUMBAR)
        data = (numbr)value_get_numbar(VALUE);
    /* YARN to NUMBR */
    else if (VALUE->type == YARN) {
        int pos, neg = 0;
        data = 0;
        for (pos = 0; value_get_yarn(VALUE)[pos] != 0; pos++) {
            if (pos == 0 && value_get_yarn(VALUE)[pos] == '-') {
                neg = 1;
                pos++;
            }
            if (isdigit(value_get_yarn(VALUE)[pos])) {
                data *= 10;
                data += (numbr)(value_get_yarn(VALUE)[pos] - '0');
            }
            else return NULL;
        }
        if (neg) data *= -1;
    }
    /* Invalid type */
    else assert(0);
    value = malloc(sizeof(struct value));
    value->type = NUMBR;
    value->data = malloc(sizeof(numbr));
    *((numbr *)(value->data)) = data;
    return value;
}

    struct value *
value_cast_numbar(struct value *VALUE)
{
    struct value *value = NULL;
    numbar data;
    assert(VALUE);
    /* NOOB to NUMBAR */
    /* TROOF to NUMBAR */
    if (VALUE->type == TROOF)
        data = (numbar)(value_get_troof(VALUE) == WIN ? 1.0 : 0.0);
    /* NUMBR to NUMBAR */
    else if (VALUE->type == NUMBR)
        data = (numbar)value_get_numbr(VALUE);
    /* NUMBAR to NUMBAR */
    else if (VALUE->type == NUMBAR)
        data = value_get_numbar(VALUE);
    /* YARN to NUMBAR */
    else if (VALUE->type == YARN) {
        int pos, neg = 0, dec = 0;
        data = 0.0;
        for (pos = 0; value_get_yarn(VALUE)[pos] != 0; pos++) {
            if (pos == 0 && value_get_yarn(VALUE)[pos] == '-') {
                neg = 1;
                pos++;
            }
            if (isdigit(value_get_yarn(VALUE)[pos])) {
                if (dec) {
                    int n;
                    numbar frac = (numbar)(value_get_yarn(VALUE)[pos] - '0');
                    for (n = 0; n < dec; n++)
                        frac /= 10;
                    data += frac;
                    dec++;
                }
                else {
                    data *= 10.0;
                    data += (numbar)(value_get_yarn(VALUE)[pos] - '0');
                }
            }
            else if (value_get_yarn(VALUE)[pos] == '.' && !dec) dec = 1;
            else return NULL;
        }
        if (neg) data *= -1.0;
    }
    /* Invalid type */
    else assert(0);
    value = malloc(sizeof(struct value));
    value->type = NUMBAR;
    value->data = malloc(sizeof(numbar));
    *((numbar *)(value->data)) = data;
    return value;
}

    struct value *
value_cast_yarn(struct value *VALUE)
{
    struct value *value = NULL;
    yarn data;
    assert(VALUE);
    /* NOOB to YARN */
    if (VALUE->type == NOOB) {
        data = malloc(sizeof(char));
        *data = '\0';
    }
    /* TROOF to YARN */
    /* NUMBR to YARN */
    else if (VALUE->type == NUMBR) {
        numbr val = value_get_numbr(VALUE);
        char *buf = malloc(sizeof(char) * 64);
        int pos = 0, whole, neg = 0;
        if (val < 0) {
            buf[pos++] = '-';
            val *= -1;
            neg = 1;
        }
        whole = val;
        while (whole /= 10) pos++;
        buf[pos + 1] = 0;
        /* Whole part */
        while (val >= 0 && pos >= 0 + neg) {
            buf[pos--] = '0' + (val % 10);
            val /= 10;
        }
        data = malloc(sizeof(char) * (strlen(buf) + 1));
        strcpy(data, buf);
        free(buf);
    }
    /* NUMBAR to YARN */
    else if (VALUE->type == NUMBAR) {
        numbar val = value_get_numbar(VALUE);
        /* TODO: What is appropriate for this value? */
        numbar eps = (numbar)0.001;
        char *buf = malloc(sizeof(char) * 64);
        int pos = 0, end, whole, neg = 0, precision = 2;
        if (val < 0) {
            buf[pos++] = '-';
            val *= -1.0;
            neg = 1;
        }
        whole = (int)val;
        while (whole /= 10) pos++;
        end = pos + 2 + precision;
        buf[pos + 1] = '.';
        whole = (int)val;
        /* Whole part */
        while (whole >= 0 && pos >= 0 + neg) {
            buf[pos--] = '0' + (whole % 10);
            whole /= 10;
        }
        pos = end - precision;
        /* Fraction part */
        while (val > 0 && pos < end) {
            val *= 10;
            /* HACK: to solve 0.1 * 10 being treated as 0.999... */
            buf[pos++] = '0' + ((int)(val + eps) % 10);
            val -= (int)val;
        }
        /* Pad with 0's */
        while (pos < end)
            buf[pos++] = '0';
        buf[pos] = 0;
        data = malloc(sizeof(char) * (strlen(buf) + 1));
        strcpy(data, buf);
        free(buf);
    }
    /* YARN to YARN */
    else if (VALUE->type == YARN) {
        data = malloc(sizeof(char) * (strlen(value_get_yarn(VALUE)) + 1));
        strcpy(data, value_get_yarn(VALUE));
    }
    /* Invalid type */
    else assert(0);
    value = malloc(sizeof(struct value));
    value->type = YARN;
    value->data = data;
    return value;
}

/* General value manipulation functions */

    int
value_cmp(struct value *LEFT, struct value *RIGHT)
    /* Compares the contents of two similarly-typed value structures */
{
    assert(LEFT);
    assert(RIGHT);
    assert(LEFT->type);
    assert(RIGHT->type);
    switch (LEFT->type) {
        case NOOB: return 1;
        case TROOF:
            return value_get_troof(LEFT) == value_get_troof(RIGHT);
        case NUMBR:
            return value_get_numbr(LEFT) == value_get_numbr(RIGHT);
        case NUMBAR:
            return value_get_numbar(LEFT) == value_get_numbar(RIGHT);
        case YARN:
            return !strcmp(value_get_yarn(LEFT), value_get_yarn(RIGHT));
        default: return 0;
    }
    return 0;
}

    struct value *
value_copy(struct value *VALUE)
    /* Returns a copy of the contents of VALUE */
{
    assert(VALUE);
    assert(VALUE->data);
    switch (VALUE->type) {
        case NOOB:
            return value_create_noob();
        case TROOF:
            return value_create_troof(value_get_troof(VALUE));
        case NUMBR:
            return value_create_numbr(value_get_numbr(VALUE));
        case NUMBAR:
            return value_create_numbar(value_get_numbar(VALUE));
        case YARN:
            return value_create_yarn(value_get_yarn(VALUE));
    }
    return NULL;
}

    void
value_delete(struct value *VALUE)
{
    assert(VALUE);
    if (VALUE->data) free(VALUE->data);
    free(VALUE);
}

    unsigned int
value_size_yarn(yarn DATA)
{
    return sizeof(char) * (strlen(DATA) + 1);
}

#endif /* __LOLTYPES__ */
