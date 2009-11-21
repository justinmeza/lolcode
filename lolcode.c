/*
 * lolcode.c
 *
 * This program parses and executes the commands in a LOLCODE file. If a file
 * is not syntactically valid, execution will halt at whatever point precedes
 * the error. Variables are assigned and cast dynamically (at run time).
 *
 * See < http://lolcode.com/specs/1.2 > for more information.
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
 *
 * TODO
 *
 *    - Add scoping to other control structures
 *    - For the love of god please make everything 80 columns wide at most!
 *    - For BIGGR OF and SMALLR OF, what should the return type be? The
 *      bigger (or smaller) of the two values, or NUMBR if both NUMBRs and
 *      NUMBAR if any NUMBARs. Specifically, what if two different types are
 *      the same?  Which value is to be returned in this case of a tie?
 *    - Character hex value interpolation, Unicode?
 *    - Pick a good value for eps to ensure 0.1 * 10 = 0.99999... is
 *      interpreted as 1.
 *    - Are ending AN's evil in the get_args function?
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "hash.h"
#include "list.h"
#include "state.h"
#include "parser.h"
#include "args.h"
#include "loltypes.h"
#include "lolfunc.h"

    void
data_delete_value(void *DATA)
    /* Deletes pointers to values */
{
    value_delete(DATA);
}

    void
data_delete_list(void *DATA)
    /* Deletes pointers to lists */
{
    list_delete(DATA);
}

    void
data_delete_func(void *DATA)
    /* Deletes pointers to funcs */
{
    func_delete(DATA);
}

    int
parser_rules(char *BUF, size_t LEN, unsigned int *START, unsigned int *POS)
    /* The order of these rules is important! */
{
    /* String literals */
    if (BUF[*POS] == '"') {
        do if (BUF[(*POS)++] == '\n') return 1;
        while (*POS < LEN && (BUF[*POS] != '"' || (*POS > 0 && BUF[*POS - 1] == ':')));
    }
    /* Single-line comments */
    if (!strncmp(BUF + *POS, "BTW", 3) && !(*POS > 0 && BUF[*POS - 1] == 'O')) {
        *POS += 3;
        while (*POS < LEN && BUF[*POS] != '\n') *START = ++(*POS);
    }
    return 0;
}

    void
error(struct parser *PARSER, char *MESSAGE)
    /* Prints error messages and sets the state of the ERROR structure */
{
    struct token *token;
    assert(PARSER);
    token = (struct token *)list_head(PARSER->tokens);
    if (token) {
        if (token->null) fprintf(stderr, "%s:%d: %s: at end of line.\n",
                PARSER->name,
                token->line + 1,
                MESSAGE);
        else fprintf(stderr, "%s:%d: %s: at `%s'.\n",
                PARSER->name,
                token->line + 1,
                MESSAGE,
                token->data);
    }
    else fprintf(stderr, "%s: %s: at end of file.\n",
            PARSER->name,
            MESSAGE);
}

    struct value *
token_to_troof(struct token *TOKEN)
    /* Casts TOKEN's value to a TROOF. On success, returns the new value and
     * frees the token, otherwise, returns NULL and the token remains
     * allocated. */
{
    troof data;
    assert(TOKEN);
    assert(TOKEN->data);
    if (!strcmp(TOKEN->data, "WIN"))
        data = WIN;
    else if (!strcmp(TOKEN->data, "FAIL"))
        data = FAIL;
    else {
        return NULL;
    }
    token_delete(TOKEN);
    return value_create_troof(data);;
}

    struct value *
token_to_numbr(struct token *TOKEN)
{
    numbr data = 0;
    int pos, neg = 0;
    assert(TOKEN);
    assert(TOKEN->data);
    for (pos = 0; TOKEN->data[pos] != 0; pos++) {
        if (pos == 0 && TOKEN->data[pos] == '-') {
            neg = 1;
            pos++;
        }
        if (isdigit(TOKEN->data[pos])) {
            data *= 10;
            data += (int)(TOKEN->data[pos] - '0');
        }
        else return NULL;
    }
    if (neg) data *= -1;
    token_delete(TOKEN);
    return value_create_numbr(data);
}

    struct value *
token_to_numbar(struct token *TOKEN)
{
    numbar data = 0.0;
    int pos, neg = 0, dec = 0;
    assert(TOKEN);
    assert(TOKEN->data);
    for (pos = 0; TOKEN->data[pos] != 0; pos++) {
        if (pos == 0 && TOKEN->data[pos] == '-') {
            neg = 1;
            pos++;
        }
        if (isdigit(TOKEN->data[pos])) {
            if (dec) {
                int n;
                numbar frac = (numbar)(TOKEN->data[pos] - '0');
                for (n = 0; n < dec; n++)
                    frac /= 10;
                data += frac;
                dec++;
            }
            else {
                data *= 10.0;
                data += (numbar)(TOKEN->data[pos] - '0');
            }
        }
        else if (TOKEN->data[pos] == '.' && !dec) dec = 1;
        else return NULL;
    }
    if (neg) data *= -1.0;
    token_delete(TOKEN);
    return value_create_numbar(data);
}

    struct value *
token_to_yarn(struct parser *PARSER, struct state *VARS, struct token *TOKEN)
{
    struct value *value = NULL;
    yarn temp;                          /* Constructs the YARN */
    int len;                            /* Token string length */
    int n;                              /* Tracks token position */
    int i;                              /* Tracks value position */
    /* Check for YARN format */
    assert(TOKEN);
    assert(TOKEN->data);
    if (TOKEN->data[0] != '"' || !(len = strlen(TOKEN->data))
            || TOKEN->data[len - 1] != '"')
        return NULL;
    /* Copy characters, removing quotes */
    temp = malloc(sizeof(char) * (len - 1));
    for (n = 1, i = 0; n < len - 1; n++) {
        if (TOKEN->data[n] == '"' && n != 0 && n < len - 1) {
            /* Rogue quote character */
            error(PARSER, "Rogue quote character within YARN");
            return NULL;
        }
        else if (TOKEN->data[n] == ':') {
            /* Escape characters */
            if (TOKEN->data[n + 1] == ')') {    /* Newline */
                temp[i++] = '\n';
                n++;
            }
            else if (TOKEN->data[n + 1] == '>') {   /* Tab */
                temp[i++] = '\t';
                n++;
            }
            else if (TOKEN->data[n + 1] == 'o') {   /* Alarm */
                temp[i++] = '\a';
                n++;
            }
            else if (TOKEN->data[n + 1] == '"') {   /* Double quote */
                temp[i++] = '"';
                n++;
            }
            else if (TOKEN->data[n + 1] == ':') {   /* Colon */
                temp[i++] = ':';
                n++;
            }
            else if (TOKEN->data[n + 1] == '{') {   /* Variable */
                yarn data = NULL;
                struct value *value = NULL;
                int start = n + 2;
                int end = start;
                /* Seek to end brace */
                while (end < len - 1 && TOKEN->data[end] != '}') end++;
                if (end == len - 1) {
                    error(PARSER, "Unclosed variable interpolation block");
                    free(temp);
                    token_delete(TOKEN);
                    return NULL;
                }
                TOKEN->data[end] = '\0';
                /* Retrieve variable value */
                value = state_read(VARS, TOKEN->data + start);
                if (!value) {
                    error(PARSER, "Invalid interpolation expression");
                    free(temp);
                    token_delete(TOKEN);
                    return NULL;
                }
                value = value_cast_yarn(value);
                data = value_get_yarn(value);
                /* Reallocate temp, adding data's size */
                temp = realloc(temp, sizeof(char) * (len + strlen(data) - 2));
                /* Place the contents of data in temp */
                strcpy(temp + i, data);
                i += strlen(data);
                /* Clean up */
                value_delete(value);
                n = end;
                break;
            }
            else {
                error(PARSER, "Rogue escape character within YARN");
                free(temp);
                token_delete(TOKEN);
                return NULL;
            }
        }
        /* Otherwise, just copy over all other characters */
        else temp[i++] = TOKEN->data[n];
    }
    temp[i] = '\0';
    /* Clean up and return */
    value = value_create_yarn(temp);
    free(temp);
    token_delete(TOKEN);
    return value;
}

struct value *evaluate_expr(struct parser *, struct state *, struct hash *,
        struct hash *);

    struct list *
args_get(struct parser *PARSER, struct state *VARS, struct hash *LOOPS,
        struct hash *FUNCS, int NUM)
    /* Removes NUM arguments from parsers token stream, optionally seperated by
     * ANs. NUM < 0 retrieves as many arguments as possible.  Caller code
     * should check to make sure the number of arguments actually returned
     * matches the arity of the function. */
{
    struct list *args = list_create(data_delete_value);
    struct value *arg = NULL;
    int start = NUM;
    while (NUM--) {
        if ((arg = evaluate_expr(PARSER, VARS, LOOPS, FUNCS)) == NULL) break;
        list_push_back(args, arg);
        if (NUM) parser_ignore(PARSER, "AN");
    }
    return args;
}

    struct list *
args_convert(struct list *LIST, int *TYPES, unsigned int SIZE)
    /* Converts a list of mixed-type values into a list of values conforming to
     * a particular typing scheme. The exact scheme is determined using the
     * TYPES array and a simple two step process, described below. SIZE is the
     * size of the TYPES array. */
{
    struct list *list = list_create(data_delete_value);
    /* For each value in LIST, */
    while (!list_empty(LIST)) {
        struct value *value = (struct value *)list_head(LIST);
        unsigned int n;
        /* Check if native type matches one in TYPES */
        for (n = 0; n < SIZE; n++) {
            if (value->type == TYPES[n]) {
                list_push_back(list, value_copy(value));
                break;
            }
        }
        /* If type matches, move to next value */
        if (n < SIZE) {
            list_pop_front(LIST);
            continue;
        }
        /* Otherwise, check if the type can be cast */
        for (n = 0; n < SIZE; n++) {
            struct value *cast = NULL;
            switch (TYPES[n]) {
                case NOOB:
                    cast = value_create_noob();
                    break;
                case TROOF:
                    cast = value_cast_troof(value);
                    break;
                case NUMBR:
                    cast = value_cast_numbr(value);
                    break;
                case NUMBAR:
                    cast = value_cast_numbar(value);
                    break;
                case YARN:
                    cast = value_cast_yarn(value);
                    break;
                default:
                    fprintf(stderr, "Incorrect cast type.\n");
                    return NULL;
                    break;
            }
            if (cast) {
                list_push_back(list, cast);
                list_pop_front(LIST);
                break;
            }
        }
        /* If unable to cast, bail */
        if (n == SIZE) {
            list_delete(list);
            list_delete(LIST);
            return NULL;
        }
        /* Otherwise, move to next value */
        list_pop_front(LIST);
    }
    list_delete(LIST);
    return list;
}

    int
evaluate_parser(struct parser *PARSER, struct state *VARS, struct hash *LOOPS,
        struct hash *FUNCS)
    /* Evaluates all of the remaining tokens in PARSER and returns the result
     * of the last evaluated expression. The arguments which are used for the
     * state of the program are not modified but are visible to the evaluated
     * code. Returns 0 if the entire parser was evaluated, and 1 otherwise. */
{
    assert(PARSER);
    assert(VARS);
    assert(LOOPS);
    assert(FUNCS);

    /* HAI <VERSION> */
    if (!parser_cmp(PARSER, "HAI")) {
        error(PARSER, "Expected HAI token");
        return 1;
    }
    if (!parser_cmp(PARSER, "1.2")) {
        error(PARSER, "Expected version 1.2");
        return 1;
    }
    if (!parser_cmp(PARSER, NULL)) {
        error(PARSER, "Unexpected");
        return 1;
    }

    while (!parser_empty(PARSER)) {
        struct value *value = NULL;
        /* OBTW ... TLDR */
        if (parser_cmp(PARSER, "OBTW"))
                list_delete(parser_seek(PARSER, "TLDR"));
        else {
            /* KTHXBYE */
            if (parser_cmp_peek(PARSER, "KTHXBYE")) return 0;
            /* Evaluate parser expressions */
            else if (!(value = evaluate_expr(PARSER, VARS, LOOPS, FUNCS)))
                return 1;
        }
        /* We should be left with a null token */
        if (!parser_cmp(PARSER, NULL)) {
            error(PARSER, "Unexpected token");
            /* TODO: try returning the unexpected token and deleting it
             * later */
            if (value) value_delete(value);
            return 1;
        }
        /* Update IT */
        if (value) state_write(VARS, "IT", value);
    }
    return 0;
}

    struct value *
evaluate_expr(struct parser *PARSER, struct state *VARS, struct hash *LOOPS,
        struct hash *FUNCS)
    /* Evaluates the next valid expression present in PARSER's token stream */
{
    struct token *token;
    struct value *value;
    assert(PARSER);
    assert(VARS);
    assert(LOOPS);
    assert(FUNCS);

    /* VISIBLE */
    if (parser_cmp(PARSER, "VISIBLE")) {
        /* Retrieve all arguments */
        struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, -1);
        /* Check for at least one argument */
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to VISIBLE");
            list_delete(args);
            return NULL;
        }
        /* Convert and print arguments */
        while (!list_empty(args)) {
            struct value *arg = (struct value *)list_head(args);
            if (arg->type != YARN) {
                struct value *val = NULL;
                val = value_cast_yarn(arg);
                if (!val) {
                    error(PARSER, "Invalid argument to VISIBLE");
                    list_delete(args);
                    value_delete(val);
                    return NULL;
                }
                printf("%s", value_get_yarn(val));
                value_delete(val);
            }
            else printf("%s", value_get_yarn(arg));
            list_pop_front(args);
        }
        list_delete(args);
        /* Append optional newline */
        if (!parser_cmp(PARSER, "!")) printf("\n");
        return value_create_noob();
    }

    /* SUM OF */
    if (parser_cmp(PARSER, "SUM")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `SUM'");
            return NULL;
        }
        /* Retrieve all arguments */
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        /* Check for correct number of arguments */
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to SUM OF");
            list_delete(args);
            return NULL;
        }
        /* Convert arguments to correct types */
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to SUM OF");
            list_delete(values);
            return NULL;
        }
        /* Apply the appropriate operation */
        return func_foldl(values, func_sumof);
    }

    /* DIFF OF */
    if (parser_cmp(PARSER, "DIFF")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `DIFF'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to DIFF OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to DIFF OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_diffof);
    }

    /* PRODUKT OF */
    if (parser_cmp(PARSER, "PRODUKT")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `PRODUKT'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to PRODUKT OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to PRODUKT OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_produktof);
    }

    /* QUOSHUNT OF */
    if (parser_cmp(PARSER, "QUOSHUNT")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `QHUSHUNT'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to QUOSHUNT OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to QUOSHUNT OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_quoshuntof);
    }

    /* MOD OF */
    if (parser_cmp(PARSER, "MOD")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[1] = { NUMBR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `MOD'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to MOD OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to MOD OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_modof);
    }

    /* BIGGR OF */
    if (parser_cmp(PARSER, "BIGGR")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `BIGGR'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to BIGGR OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to BIGGR OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_biggrof);
    }

    /* SMALLR OF */
    if (parser_cmp(PARSER, "SMALLR")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `BIGGR'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to SMALLR OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to SMALLR OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_smallrof);
    }

    /* BOTH OF, BOTH SAEM */
    if (parser_cmp(PARSER, "BOTH")) {
        if (parser_cmp(PARSER, "OF")) {
            struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
            struct list *values = NULL;
            int types[1] = { TROOF };
            if (list_size(args) != 2) {
                error(PARSER, "Wrong number of arguments to BOTH OF");
                list_delete(args);
                return NULL;
            }
            values = args_convert(args, types, 1);
            if (!values) {
                error(PARSER, "Invalid argument to BOTH OF");
                list_delete(values);
                return NULL;
            }
            return func_foldl(values, func_bothof);
        }
        else if (parser_cmp(PARSER, "SAEM")) {
            struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
            struct list *values = NULL;
            int types[2] = { NUMBR, NUMBAR };
            if (list_size(args) != 2) {
                error(PARSER,"Wrong number of arguments to BOTH SAEM");
                list_delete(args);
                return NULL;
            }
            values = args_convert(args, types, 2);
            if (!values) {
                error(PARSER, "Invalid argument to BOTH SAEM");
                list_delete(values);
                return NULL;
            }
            return func_foldl(values, func_bothsaem);
        }
        else {
            error(PARSER, "Expected token after `BOTH'");
            return NULL;
        }
    }

    /* EITHER OF */
    if (parser_cmp(PARSER, "EITHER")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[1] = { TROOF };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `EITHER'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to EITHER OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to EITHER OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_eitherof);
    }

    /* WON OF */
    if (parser_cmp(PARSER, "WON")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[1] = { TROOF };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `WON'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to WON OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to WON OF");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_wonof);
    }

    /* NOT */
    if (parser_cmp(PARSER, "NOT")) {
        /* Retrieve one argument */
        struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, 1);
        struct value *value = NULL;
        struct value *result = NULL;
        /* Check for correct number of arguments */
        if (list_size(args) != 1) {
            error(PARSER, "Wrong number of arguments to NOT");
            list_delete(args);
            return NULL;
        }
        /* Apply the NOT operation */
        value = (struct value *)list_head(args);
        if (value->type != TROOF) {
            error(PARSER, "Invalid argument to NOT");
            list_delete(args);
            return NULL;
        }
        if (value_get_troof(value) == WIN)
            result = value_create_troof(FAIL);
        else
            result = value_create_troof(WIN);
        list_delete(args);
        return result;
    }

    /* ALL OF */
    if (parser_cmp(PARSER, "ALL")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[1] = { TROOF };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `ALL'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, -1);
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to ALL OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to ALL OF");
            list_delete(values);
            return NULL;
        }
        parser_cmp(PARSER, "MKAY");
        /* Short-circuit operation */
        return func_foldl_short(values,
                func_bothof,
                value_create_troof(FAIL));
    }

    /* ANY OF */
    if (parser_cmp(PARSER, "ANY")) {
        struct list *args = NULL;
        struct list *values = NULL;
        int types[1] = { TROOF };
        if (!parser_cmp(PARSER, "OF")) {
            error(PARSER, "Expected `OF' after `ANY'");
            return NULL;
        }
        args = args_get(PARSER, VARS, LOOPS, FUNCS, -1);
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to ANY OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to ANY OF");
            list_delete(values);
            return NULL;
        }
        parser_cmp(PARSER, "MKAY");
        /* Short-circuit operation */
        return func_foldl_short(values,
                func_eitherof,
                value_create_troof(WIN));
    }

    /* DIFFRINT */
    if (parser_cmp(PARSER, "DIFFRINT")) {
        struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, 2);
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to DIFFRINT");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to DIFFRINT");
            list_delete(values);
            return NULL;
        }
        return func_foldl(values, func_diffrint);
    }

    /* SMOOSH */
    if (parser_cmp(PARSER, "SMOOSH")) {
        struct list *args = args_get(PARSER, VARS, LOOPS, FUNCS, -1);
        struct list *values = NULL;
        int types[1] = { YARN };
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to SMOOSH");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to SMOOSH");
            list_delete(values);
            return NULL;
        }
        parser_cmp(PARSER, "MKAY");
        return func_foldl(values, func_smoosh);
    }

    /* I HAS A ... ITZ */
    if (parser_cmp(PARSER, "I")) {
        struct token *token;
        if (!parser_cmp(PARSER, "HAS")) {
            error(PARSER, "Expected `HAS' after `I'");
            return NULL;
        }
        if (!parser_cmp(PARSER, "A")) {
            error(PARSER, "Expected `A' after `I HAS'");
            return NULL;
        }
        /* Make sure next token is non-NULL and unique */
        /* TODO: check for reserved keyword */
        if (parser_cmp_peek(PARSER, NULL)
                || !(token = parser_get(PARSER))) {
            error(PARSER, "Variable name expected");
            return NULL;
        }
        if (state_find(VARS, token->data, 0)) {
            error(PARSER, "Variable previously declared");
            return NULL;
        }
        /* Check for initialization */
        if (parser_cmp(PARSER, "ITZ")) {
            struct value *value = evaluate_expr(PARSER, VARS, LOOPS, FUNCS);
            if (!value) {
                error(PARSER, "Expected expression after `ITZ'");
                value_delete(value);
                token_delete(token);
                return NULL;
            }
            state_insert(VARS, token->data, value);
        }
        /* Otherwise create a NOOB type variable */
        else state_write(VARS, token->data, value_create_noob());
        token_delete(token);
        return value_create_noob();
    }

    /* R */
    if (parser_cmp_at(PARSER, 1, "R")) {
        struct token *token = parser_get(PARSER);
        struct value *value;
        /* Sanity check */
        if (!token) {
            error(PARSER, "Invalid assignment target");
            return NULL;
        }
        /* Remove the R from the token stream */
        parser_cmp(PARSER, "R");
        /* Retrieve the value to assign */
        value = evaluate_expr(PARSER, VARS, LOOPS, FUNCS);
        if (!value) {
            error(PARSER, "Invalid assignment expression");
            token_delete(token);
            return NULL;
        }
        /* Write the variable value */
        state_write(VARS, token->data, value);
        /* Clean up and return a NOOB */
        token_delete(token);
        return value_create_noob();
    }

    /* IS NOW A */
    if (parser_cmp_at(PARSER, 1, "IS")) {
        struct token *token = parser_get(PARSER);
        struct value *value;
        if (!token) {
            error(PARSER, "Invalid casting target");
            return NULL;
        }
        parser_cmp(PARSER, "IS");
        if (!parser_cmp(PARSER, "NOW")) {
            error(PARSER, "Expected `NOW' after `IS'");
            token_delete(token);
            return NULL;
        }
        if (!parser_cmp(PARSER, "A")) {
            error(PARSER, "Expected `A' after `IS NOW'");
            token_delete(token);
            return NULL;
        }
        value = state_read(VARS, token->data);
        if (parser_cmp(PARSER, "TROOF"))
            state_write(VARS, token->data, value_cast_troof(value));
        else if (parser_cmp(PARSER, "YARN"))
            state_write(VARS, token->data, value_cast_yarn(value));
        else if (parser_cmp(PARSER, "NUMBR"))
            state_write(VARS, token->data, value_cast_numbr(value));
        else if (parser_cmp(PARSER, "NUMBAR"))
            state_write(VARS, token->data, value_cast_numbar(value));
        else if (parser_cmp(PARSER, "NOOB"))
            state_write(VARS, token->data, value_cast_noob(value));
        token_delete(token);
        return value_create_noob();
    }

    /* MAEK */
    if (parser_cmp(PARSER, "MAEK")) {
        struct token *token = parser_get(PARSER);
        struct value *value;
        if (!token) {
            error(PARSER, "Invalid casting target");
            return NULL;
        }
        parser_cmp(PARSER, "A");
        value = state_read(VARS, token->data);
        if (parser_cmp(PARSER, "TROOF"))
            value = value_cast_troof(value);
        else if (parser_cmp(PARSER, "YARN"))
            value = value_cast_yarn(value);
        else if (parser_cmp(PARSER, "NUMBR"))
            value = value_cast_numbr(value);
        else if (parser_cmp(PARSER, "NUMBAR"))
            value = value_cast_numbar(value);
        else if (parser_cmp(PARSER, "NOOB"))
            value = value_cast_noob(value);
        token_delete(token);
        return value;
    }

    /* GIMMEH */
    if (parser_cmp(PARSER, "GIMMEH")) {
        struct token *token = parser_get(PARSER);
        struct value *value = NULL;
        yarn line = NULL;
        size_t size = 0;
        if (!token || !(value = state_read(VARS, token->data))) {
            error(PARSER, "Invalid assignment target");
            token_delete(token);
            return NULL;
        }
        /* TODO: Does GIMMEH replace every type?
         * if (value->type != YARN) {
         *     error(PARSER, "Type mismatch: GIMMEH expected YARN");
         *     token_delete(token);
         *     return NULL;
         * } */
        get_line(&line, &size, stdin);
        state_write(VARS, token->data, value_create_yarn(line));
        free(line);
        token_delete(token);
        return value_create_noob();
    }

    /* WTF? */
    if (parser_cmp(PARSER, "WTF?")) {
        struct value *it = NULL;
        struct list *find = NULL;
        if (!VARS || !(it = state_read(VARS, "IT"))) return NULL;
        find = list_create(token_delete);
        list_push_back(find, token_create_str("OMG"));
        list_push_back(find, token_create_str("OMGWTF"));
        list_push_back(find, token_create_str("OIC"));
        state_save(VARS);
        while (!parser_empty(PARSER)) {
            /* OMG */
            list_delete(parser_seek_list(PARSER, find));
            if (parser_cmp(PARSER, "OMG")) {
                struct value *value = evaluate_expr(PARSER, VARS, LOOPS, FUNCS);
                if (!value) {
                    error(PARSER, "Expected expression after `OMG'");
                    list_delete(find);
                    return NULL;
                }
                if (value->type != it->type) {
                    error(PARSER, "Incorrect type for `OMG' value");
                    list_delete(find);
                    return NULL;
                }
                if (value_cmp(value, it)) {
                    list_delete(find);
                    state_restore(VARS);
                    return value_create_noob();
                }
                continue;
            }
            else if (parser_cmp(PARSER, "OMGWTF")
                    || parser_cmp(PARSER, "OIC")) {
                list_delete(find);
                state_restore(VARS);
                return value_create_noob();
            }
        }
    }

    /* OMG */
    /* TODO: Would it be faster to simply remove the next token, or just let it
     * be superficially evaluated as it is now? Basically, should IT be
     * affected by evaluating an `OMG' expression? If so, leave the evaluation
     * in. Otherwise, remove the next token so it is not parsed. */
    if (parser_cmp(PARSER, "OMG"));

    /* GTFO */
    if (parser_cmp(PARSER, "GTFO")) {
        list_delete(parser_seek(PARSER, "OIC"));
        return value_create_noob();
    }

    /* OIC */
    if (parser_cmp(PARSER, "OIC"))
        return value_create_noob();

    /* O RLY? */
    if (parser_cmp(PARSER, "O")) {
        struct value *value = NULL;
        struct list *find = NULL;
        if (!parser_cmp(PARSER, "RLY?")) {
            error(PARSER, "Expected `RLY?' after `O'");
            return NULL;
        }
        if (!VARS || !(value = state_read(VARS, "IT"))) return NULL;
        /* Save current scope state, restore it before returning */
        state_save(VARS);
        if (value_get_troof(value) == WIN) {
            /* YA RLY */
            find = list_create(token_delete);
            list_push_back(find, token_create_str("YA"));
            list_push_back(find, token_create_str("OIC"));
            list_delete(parser_seek_list(PARSER, find));
            list_delete(find);
            if (!parser_cmp(PARSER, "YA"))
                return value_create_noob();
            find = list_create(token_delete);
            list_push_back(find, token_create_str("RLY"));
            list_push_back(find, token_create_str("OIC"));
            list_delete(parser_seek_list(PARSER, find));
            list_delete(find);
            if (parser_cmp(PARSER, "RLY")) {
                state_restore(VARS);
                return value_create_noob();
            }
            else {
                error(PARSER, "Expected `RLY' after `YA'");
                return NULL;
            }
            state_restore(VARS);
        }
        else {
            struct value *expr = NULL;
            find = list_create(token_delete);
            list_push_back(find, token_create_str("MEBBE"));
            list_push_back(find, token_create_str("NO"));
            list_push_back(find, token_create_str("OIC"));
            while (!parser_empty(PARSER)) {
                list_delete(parser_seek_list(PARSER, find));
                /* MEBBE */
                if (parser_cmp(PARSER, "MEBBE")) {
                    if (!(expr = evaluate_expr(PARSER, VARS, LOOPS, FUNCS))) {
                        error(PARSER, "Expected expression after `MEBBE'");
                        list_delete(find);
                        return NULL;
                    }
                    if (value_get_troof(expr) == FAIL) continue;
                    /* TODO: Is this the proper way to handle IT? Modified only
                     * when WIN?  At all? */
                    state_write(VARS, "IT", expr);
                    list_delete(find);
                    state_restore(VARS);
                    return value_create_noob();
                }
                /* NO WAI */
                else if (parser_cmp(PARSER, "NO")) {
                    if (parser_cmp(PARSER, "WAI")) {
                        list_delete(find);
                        state_restore(VARS);
                        return value_create_noob();
                    }
                    else {
                        error(PARSER, "Expected `WAI' after `NO'");
                        list_delete(find);
                        return NULL;
                    }
                }
                else {
                    parser_cmp(PARSER, "OIC");
                    list_delete(find);
                    state_restore(VARS);
                    return value_create_noob();
                }
            }
        }
    }

    /* MEBBE */
    if (parser_cmp(PARSER, "MEBBE")) {
        list_delete(parser_seek(PARSER, "OIC"));
        return value_create_noob();
    }

    /* NO WAI */
    if (parser_cmp(PARSER, "NO")) {
        if (parser_cmp(PARSER, "WAI")) {
            list_delete(parser_seek(PARSER, "OIC"));
            return value_create_noob();
        }
        else {
            error(PARSER, "Expected `WAI' after `NO'");
            return NULL;
        }
    }

    /* IM IN UR */
    if (parser_cmp(PARSER, "IM")) {
        if (parser_cmp(PARSER, "IN")) {
            int line;
            int column;
            struct token *name = NULL;
            struct token *token = NULL;
            struct token *var = NULL;
            struct list *ops = NULL;
            struct list *list = NULL;
            void *head = NULL;
            if (!parser_cmp(PARSER, "YR")) {
                error(PARSER, "Expected `YR' after `IN'");
                return NULL;
            }
            name = parser_get(PARSER);
            token = parser_get(PARSER);
            if (!parser_cmp(PARSER, "YR")) {
                error(PARSER, "Expected `YR' after operation in loop");
                token_delete(name);
                token_delete(token);
                return NULL;
            }
            var = parser_get(PARSER);
            if (!state_read(VARS, var->data)) {
                error(PARSER, "Invalid loop update target");
                token_delete(name);
                token_delete(token);
                token_delete(var);
                return NULL;
            }
            line = name->line;
            column = name->column;
            ops = list_create(token_delete);
            /* If a stopping condition is present, read and format the
             * conditional expression. */
            list_push_back(ops, token_create_null(0));
            if (parser_cmp(PARSER, "TIL")) {
                list = parser_seek(PARSER, NULL);
                list_push_back(ops, token_create("NOT", strlen("NOT"), line, column, 0));
                head = list_head(list);
                do {
                    struct token *item = (struct token *)list_head(list);
                    list_push_back(ops, item);
                    list_shift_down(list);
                }
                while (list_head(list) != head);
            }
            else if (parser_cmp(PARSER, "WILE")) {
                list = parser_seek(PARSER, NULL);
                head = list_head(list);
                do {
                    struct token *item = (struct token *)list_head(list);
                    list_push_back(ops, item);
                    list_shift_down(list);
                }
                while (list_head(list) != head);
            }
            else if (parser_cmp_peek(PARSER, NULL)) {
                list_push_back(ops, token_create("WIN", strlen("WIN"), line, column, 0));
                list_push_back(ops, token_create_null(0));
            }
            else {
                error(PARSER, "Invalid loop condition; expected `TIL' or `WILE'");
                token_delete(name);
                token_delete(token);
                token_delete(var);
                list_delete(ops);
                return NULL;
            }
            /* Consume the trailing null token */
            parser_cmp(PARSER, NULL);
            list_push_back(ops, token_create("O", strlen("O"), line, column, 0));
            list_push_back(ops, token_create("RLY?", strlen("RLY?"), line, column, 0));
            list_push_back(ops, token_create_null(0));
            list_push_back(ops, token_create("YA", strlen("YA"), line, column, 0));
            list_push_back(ops, token_create("RLY", strlen("RLY"), line, column, 0));
            list_push_back(ops, token_create_null(0));
            /* TODO: Is there a more robust way of doing this whole process?
             * If the condition succeeds, update the variable */
            list_push_back(ops, token_create(var->data, strlen(var->data), line, column, 0));
            list_push_back(ops, token_create("R", strlen("R"), line, column, 0));
            if (!strcmp("UPPIN", token->data))
                list_push_back(ops, token_create("SUM", strlen("SUM"), line, column, 0));
            else if (!strcmp("NERFIN", token->data))
                list_push_back(ops, token_create("DIFF", strlen("DIFF"), line, column, 0));
            else {
                error(PARSER, "Invalid loop operation; expected `UPPIN' or `NERFIN'");
                token_delete(name);
                token_delete(token);
                token_delete(var);
                list_delete(ops);
                return NULL;
            }
            list_push_back(ops, token_create("OF", strlen("OF"), line, column, 0));
            list_push_back(ops, token_create(var->data, strlen(var->data), line, column, 0));
            list_push_back(ops, token_create("AN", strlen("AN"), line, column, 0));
            list_push_back(ops, token_create("1", strlen("1"), line, column, 0));
            list_push_back(ops, token_create_null(0));
            /* Seek to the end of the loop */
            list = parser_seek(PARSER, (const char *)name->data);
            head = list_head(list);
            do {
                struct token *item = (struct token *)list_head(list);
                list_push_back(ops, item);
                list_shift_down(list);
            }
            while (list_head(list) != head);
            /* Add closing to conditional */
            list_push_back(ops, token_create_null(0));
            list_push_back(ops, token_create("OIC", strlen("OIC"), line, column, 0));
            /* Store the loop operations */
            hash_insert(LOOPS, name->data, ops);
            /* Start out the loop */
            parser_put_back(PARSER, ops);
            return value_create_noob();
        }
        else if (parser_cmp(PARSER, "OUTTA")) {
            struct token *name = NULL;
            if (!parser_cmp(PARSER, "YR")) {
                error(PARSER, "Expected `YR' after `OUTTA'");
                return NULL;
            }
            name = parser_get(PARSER);
            parser_put_back(PARSER, hash_find(LOOPS, name->data));
            return value_create_noob();
        }
        else {
            error(PARSER, "Expected token after `IM'");
            return NULL;
        }
    }

    /* HOW DUZ I */
    if (parser_cmp(PARSER, "HOW")) {
        if (parser_cmp(PARSER, "DUZ")) {
            struct token *name = NULL;
            struct item *head = NULL;
            struct list *args = list_create(token_delete);
            struct list *body = NULL;
            if (!parser_cmp(PARSER, "I")) {
                error(PARSER, "Expected `I' after `DUZ'");
                return NULL;
            }
            name = parser_get(PARSER);
            while (parser_cmp(PARSER, "YR")) {
                struct token *arg = NULL;
                void *head = NULL;
                /* Make sure next token is non-NULL and unique */
                /* TODO: check for reserved keyword */
                if (!(arg = parser_get(PARSER))) {
                    error(PARSER, "Argument name expected");
                    return NULL;
                }
                if (list_size(args) > 0) {
                    head = list_head(args);
                    do {
                        struct token *token = (struct token *)list_head(args);
                        if (!strcmp(token->data, arg->data)) {
                            error(PARSER, "Duplicate argument name");
                            token_delete(arg);
                            list_delete(args);
                            return NULL;
                        }
                        list_shift_down(args);
                    }
                    while (list_head(args) != head);
                }
                list_push_back(args, arg);
            }
            /* Save function body in FUNCS hash */
            body = parser_seek(PARSER, "IF");
                    head = list_head(body);
                    do {
                        struct token *token = (struct token *)list_head(body);
printf("%s\n", token->data);
                        list_shift_down(body);
                    }
                    while (list_head(body) != head);
        }
    }

    /* We must be left with value */
    token = parser_get(PARSER);
    if (!token) return NULL;

    /* VARIABLE */
    if (VARS && (value = state_read(VARS, token->data))) {
        token_delete(token);
        if (value->type == NOOB) return value_cast_noob(value);
        if (value->type == TROOF) return value_cast_troof(value);
        if (value->type == NUMBR) return value_cast_numbr(value);
        if (value->type == NUMBAR) return value_cast_numbar(value);
        if (value->type == YARN) return value_cast_yarn(value);
    }

    /* PRIMITIVE TYPES */
    if ((value = token_to_yarn(PARSER, VARS, token))
            || (value = token_to_numbr(token))
            || (value = token_to_numbar(token))
            || (value = token_to_troof(token)))
        return value;

    /* UNABLE TO EVALUATE */
    list_push_front(PARSER->tokens, token);
    return NULL;
}

    int
main(int ARGC, char **ARGV)
{
    struct parser *parser = NULL;
    struct list *ignore = NULL;
    struct list *keep = NULL;
    struct state *vars = NULL;
    struct hash *loops = NULL;
    struct hash *funcs = NULL;
    FILE *file = stdin;
    int n, status;

    /* Print help text if appropriate */
    if (get_arg(ARGC, ARGV, 'h', &n)) {
        fprintf(stderr, "USAGE: lolcode [-f FILENAME]\n");
        status = 2;
        goto DONE;
    }

    ignore = list_create(token_delete);
    list_push_back(ignore, token_create_str(" "));
    list_push_back(ignore, token_create_str("\t"));
    list_push_back(ignore, token_create_str("...\n"));

    keep = list_create(token_delete);
    list_push_back(keep, token_create_str("\n"));
    list_push_back(keep, token_create_str(","));
    list_push_back(keep, token_create_str("\r"));

    /* Check for file on command line */
    if (get_arg(ARGC, ARGV, 'f', &n)) file = fopen(ARGV[n], "r");

    parser = parser_create(file,
            file == stdin ? "STDIN" : ARGV[n],
            ignore, keep, 0, parser_rules);
    if (!parser) {
        fprintf(stderr, "Unable to open file.\n");
        goto DONE;
    }

    vars = state_create(data_delete_value, 1);
    loops = hash_create(data_delete_list, 1);
    funcs = hash_create(data_delete_func, 1);

    /* Initialize the default IT variable */
    state_write(vars, "IT", value_create_noob());
    status = evaluate_parser(parser, vars, loops, funcs);

DONE:
    hash_delete(funcs);
    hash_delete(loops);
    state_delete(vars);
    parser_delete(parser);
    list_delete(ignore);
    list_delete(keep);
    return status;
}
