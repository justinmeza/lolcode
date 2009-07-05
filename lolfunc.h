/*
 * lolfunc.h
 *
 * A function evaluation library for LOLCODE standard functions.
 *
 * MAINTAINER
 *
 *      Justin J. Meza < justin dot meza at gmail dot com >
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

#ifndef __LOLFUNC__
#define __LOLFUNC__

#include "loltypes.h"

/* LOLCODE standard functions */

/* Each of these functions accept and return pointers to value structures. Type
 * checking is done within each function. NULL is returned if argument types
 * are not compatible with a particular function. Use the value_get_* functions
 * within loltypes.h to retrieve argument data. One common way to return the
 * new value is by using the value_create_* functions. */

    struct value *
func_sumof(struct value *LEFT, struct value *RIGHT)
{
    /* Sanity check */
    if (!LEFT || !RIGHT) return NULL;
    /* Make sure types are correct */
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    /* Perform the operation */
    if (LEFT->type == NUMBR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbr(value_get_numbr(LEFT) + value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbr(LEFT) + value_get_numbar(RIGHT));
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbar(value_get_numbar(LEFT) + value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbar(LEFT) + value_get_numbar(RIGHT));
        else return NULL;
    }
    /* Return NULL otherwise */
    return NULL;
}

    struct value *
func_diffof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbr(value_get_numbr(LEFT) - value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbr(LEFT) - value_get_numbar(RIGHT));
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbar(value_get_numbar(LEFT) - value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbar(LEFT) - value_get_numbar(RIGHT));
        else return NULL;
    }
    return NULL;
}

    struct value *
func_produktof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbr(value_get_numbr(LEFT) * value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbr(LEFT) * value_get_numbar(RIGHT));
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        if (RIGHT->type == NUMBR)
            return value_create_numbar(value_get_numbar(LEFT) * value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR)
            return value_create_numbar(value_get_numbar(LEFT) * value_get_numbar(RIGHT));
        else return NULL;
    }
    return NULL;
}

    struct value *
func_quoshuntof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        if (RIGHT->type == NUMBR && value_get_numbr(RIGHT) != 0)
            return value_create_numbr(value_get_numbr(LEFT) / value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR && value_get_numbar(RIGHT) != 0.0)
            return value_create_numbar(value_get_numbr(LEFT) / value_get_numbar(RIGHT));
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        if (RIGHT->type == NUMBR && value_get_numbr(RIGHT) != 0)
            return value_create_numbar(value_get_numbar(LEFT) / value_get_numbr(RIGHT));
        else if (RIGHT->type == NUMBAR && value_get_numbar(RIGHT) != 0.0)
            return value_create_numbar(value_get_numbar(LEFT) / value_get_numbar(RIGHT));
        else return NULL;
    }
    return NULL;
}

    struct value *
func_modof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR) return NULL;
    if (RIGHT->type != NUMBR) return NULL;
    return value_create_numbr(value_get_numbr(LEFT) % value_get_numbr(RIGHT));
}

    struct value *
func_biggrof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        numbr left = value_get_numbr(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_numbr(left > right ? left : right);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_numbar(left > right ? left : right);
        }
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        numbar left = value_get_numbar(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_numbar(left > right ? left : right);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_numbar(left > right ? left : right);
        }
        else return NULL;
    }
    return NULL;
}

    struct value *
func_smallrof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        numbr left = value_get_numbr(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_numbr(left < right ? left : right);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_numbar(left < right ? left : right);
        }
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        numbar left = value_get_numbar(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_numbar(left < right ? left : right);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_numbar(left < right ? left : right);
        }
        else return NULL;
    }
    return NULL;
}

    struct value *
func_bothof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != TROOF) return NULL;
    if (RIGHT->type != TROOF) return NULL;
    return value_create_troof(value_get_troof(LEFT)
            && value_get_troof(RIGHT) ? WIN : FAIL);
}

    struct value *
func_eitherof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != TROOF) return NULL;
    if (RIGHT->type != TROOF) return NULL;
    return value_create_troof(value_get_troof(LEFT)
            || value_get_troof(RIGHT) ? WIN : FAIL);
}

    struct value *
func_wonof(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != TROOF) return NULL;
    if (RIGHT->type != TROOF) return NULL;
    return value_create_troof(value_get_troof(LEFT)
            == value_get_troof(RIGHT) ? FAIL : WIN);
}

    struct value *
func_bothsaem(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        numbr left = value_get_numbr(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_troof(left == right ? WIN : FAIL);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_troof(left == right ? WIN : FAIL);
        }
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        numbar left = value_get_numbar(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_troof(left == right ? WIN : FAIL);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_troof(left == right ? WIN : FAIL);
        }
        else return NULL;
    }
    return NULL;
}

    struct value *
func_diffrint(struct value *LEFT, struct value *RIGHT)
{
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != NUMBR && LEFT->type != NUMBAR) return NULL;
    if (RIGHT->type != NUMBR && RIGHT->type != NUMBAR) return NULL;
    if (LEFT->type == NUMBR) {
        numbr left = value_get_numbr(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_troof(left != right ? WIN : FAIL);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_troof(left != right ? WIN : FAIL);
        }
        else return NULL;
    }
    else if (LEFT->type == NUMBAR) {
        numbar left = value_get_numbar(LEFT);
        if (RIGHT->type == NUMBR) {
            numbr right = value_get_numbr(RIGHT);
            return value_create_troof(left != right ? WIN : FAIL);
        }
        else if (RIGHT->type == NUMBAR) {
            numbar right = value_get_numbar(RIGHT);
            return value_create_troof(left != right ? WIN : FAIL);
        }
        else return NULL;
    }
    return NULL;
}

    struct value *
func_smoosh(struct value *LEFT, struct value *RIGHT)
{
    struct value *value = NULL;
    yarn total = NULL;
    if (!LEFT || !RIGHT) return NULL;
    if (LEFT->type != YARN) return NULL;
    if (RIGHT->type != YARN) return NULL;
    total = malloc(sizeof(char) * (strlen(LEFT->data) + strlen(RIGHT->data) + 1));
    strcpy(total, LEFT->data);
    strcpy(total + strlen(LEFT->data), RIGHT->data);
    value = value_create_yarn(total);
    free(total);
    return value;
}

/* Function application functions */

    struct value *
func_foldl(struct list *LIST,
        struct value *(*FUNCTION)(struct value *, struct value *))
    /* Applies FUNCTION to all items in LIST from left to right */
{
    struct value *value = NULL;
    if (!LIST || !FUNCTION) return NULL;
    value = value_copy(list_head(LIST));
    list_pop_front(LIST);
    while (!list_empty(LIST)) {
        struct value *temp = FUNCTION(value, (struct value *)list_head(LIST));
        value_delete(value);
        if (!temp) {
            list_delete(LIST);
            return NULL;
        }
        value = temp;
        list_pop_front(LIST);
    }
    list_delete(LIST);
    return value;
}

    struct value *
func_foldl_short(struct list *LIST,
        struct value *(*FUNCTION)(struct value *, struct value *),
        struct value *VALUE)
    /* Applies FUNCTION to all items in LIST from left to right, and returns
     * when the accumulator is the same as the value stored in VALUE. */
{
    struct value *value = NULL;
    if (!LIST || !FUNCTION) return NULL;
    value = value_copy(list_head(LIST));
    list_pop_front(LIST);
    while (!list_empty(LIST) && !value_cmp(VALUE, value)) {
        struct value *temp = FUNCTION(value, (struct value *)list_head(LIST));
        value_delete(value);
        if (!temp) {
            list_delete(LIST);
            return NULL;
        }
        value = temp;
        list_pop_front(LIST);
    }
    value_delete(VALUE);
    list_delete(LIST);
    return value;
}

    struct value *
func_foldr(struct list *LIST,
        struct value *(*FUNCTION)(struct value *, struct value *))
    /* Applies FUNCTION to all items in LIST' from right to left */
{
    struct value *value = NULL;
    if (!LIST || !FUNCTION) return NULL;
    value = value_copy(list_tail(LIST));
    list_pop_back(LIST);
    while (!list_empty(LIST)) {
        struct value *temp = FUNCTION((struct value *)list_tail(LIST), value);
        value_delete(value);
        if (!temp) {
            list_delete(LIST);
            return NULL;
        }
        value = temp;
        list_pop_back(LIST);
    }
    list_delete(LIST);
    return value;
}

    struct value *
func_foldr_short(struct list *LIST,
        struct value *(*FUNCTION)(struct value *, struct value *),
        struct value *VALUE)
    /* Applies FUNCTION to all items in LIST from right to left, and returns
     * when the accumulator is the same as the value stored in VALUE. */
{
    struct value *value = NULL;
    if (!LIST || !FUNCTION) return NULL;
    value = value_copy(list_tail(LIST));
    list_pop_back(LIST);
    while (!list_empty(LIST) && !value_cmp(VALUE, value)) {
        struct value *temp = FUNCTION((struct value *)list_tail(LIST), value);
        value_delete(value);
        if (!temp) {
            list_delete(LIST);
            return NULL;
        }
        value = temp;
        list_pop_back(LIST);
    }
    list_delete(LIST);
    return value;
}

#endif /* __LOLFUNC__ */
