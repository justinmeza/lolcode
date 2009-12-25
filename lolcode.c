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
 *      Copyright (c) 2007-2010 Justin J. Meza
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
 *    - Make sure variables, loops, and functions do not share names
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

/* Symbol access types and symbol definition */

enum access
{
    READONLY,
    READWRITE,
};

struct symbol
{
    struct value *value;
    struct value *scope;
    enum access access;
};

/* Various helper functions for deleting objects */

    void
data_delete_list(void *DATA)
    /* Deletes pointers to lists */
{
    list_delete(DATA);
}

    void
data_delete_null(void *DATA)
    /* Does not delete anything (used with ACCESS list) */
{ }

    void *
data_copy_null(const void *DATA)
    /* Does not copy anything (used with ACCESS list) */
{ }

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
token_to_yarn(struct parser *PARSER, struct value *STATE, struct token *TOKEN)
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
                value = state_read(value_get_bukkit(STATE), TOKEN->data + start);
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

    struct symbol *
token_to_symbol(struct value *STATE, struct list *ACCESS, struct token *TOKEN)
    /* Resolves the value corresponding to TOKEN in STATE, searching through
     * parent scopes if required until the root scope is reached whereupon
     * NULL is returned if the value does not exist.  If the value is found
     * in the current scope (STATE) or a pointer to the value is on the ACCESS
     * list, ACCESS is set to READWRITE, otherwise ACCESS is set to READONLY.
     * 
     *                               +===========================+
     *                               |         symbol->          |
     *         +========+============+=======+=======+===========+
     *         | SCOPE  | IN ACCESS? | value | scope |  access   |
     *         +========+============+=======+=======+===========+
     *         |  NONE  |    N/A     | NULL  |  N/A  |    N/A    |
     *         +========+------------+-------+-------+-----------+
     *         |        |    YES     |   *   |   x   | READWRITE |
     *         | STATE  +------------+-------+-------+-----------+
     *         |        |     NO     |   *   |   x   | READWRITE |
     *         +========+------------+-------+-------+-----------+
     *         |        |    YES     |   *   |   x   | READWRITE |
     *         | PARENT +------------+-------+-------+-----------+
     *         |        |     NO     |   *   |   x   | READONLY  |
     *         +========+------------+-------+-------+-----------+
     *         * = the value which was found
     *         x = the scope in which the value was found
     */
{
    enum access access = READONLY;
    struct value *value = STATE, *state = NULL, *scope = STATE;
    char *start = NULL, *end = NULL;
    struct symbol *symbol = NULL;
    assert(TOKEN);
    assert(ACCESS);
    assert(STATE);
    /* If ``I'', we're done */
    if (strcmp(TOKEN->data, "I")) {
        do {
            value = scope;
            start = TOKEN->data;
            do {
                char *middle = NULL, *key = NULL;
                /* Find the next ``!'' */
                end = strchr(start, '!');
                /* If none found, seek to end */
                if (!end) end = strchr(start, '\0');
                /* Extract the middle portion */
                middle = malloc(sizeof(char) * (end - start + 1));
                strncpy(middle, start, end - start);
                middle[end - start] = '\0';
                /* If at start or ``!'', read from state */
                if (start == TOKEN->data || *(start - 1) == '!') key = middle;
                /* Else, if not at start and ``?'', read from STATE */
                else if (start != TOKEN->data && *(start - 1) == '?') {
                    struct value *read = NULL;
                    struct value *temp = NULL;
                    struct token *token = token_create_str(middle);
                    struct symbol *symbol = token_to_symbol(STATE, ACCESS,
                            token);
                    enum access access;
                    token_delete(token);
                    read = symbol->value;
                    access = symbol->access;
                    free(symbol);
                    if (!read) {
                        value = NULL;
                        break;
                    }
                    if (access != READONLY && access != READWRITE) {
                        value = NULL;
                        break;
                    }
                    temp = value_cast_yarn(read);
                    if (!temp) {
                        value = NULL;
                        break;
                    }
                    free(middle);
                    key = malloc(sizeof(char)
                            * (strlen(value_get_yarn(temp)) + 1));
                    strcpy(key, value_get_yarn(temp));
                    value_delete(temp);
                    start = end + 2;
                }
                else {
                    value = NULL;
                    break;
                }
                start = end + 2;
                /* Resolve the next level */
                value = state_read(value_get_bukkit(state = value), key);
                if (!value) {
                    struct token *token = token_create_str(key);
                    struct value *numbr = token_to_numbr(token);
                    /* Create final NUMBR accesses on-the-fly */
                    if (start != TOKEN->data && !(*end) && numbr) {
                        state_write(value_get_bukkit(state), key,
                                value = value_create_noob());
                        value_delete(numbr);
                    }
                    else {
                        if (numbr) value_delete(numbr);
                        else token_delete(token);
                        value = NULL;
                        break;
                    }
                }
                /* Check for access */
                else if (!list_empty(ACCESS)) {
                    void *head = list_head(ACCESS);
                    do {
                        struct value *item = (struct value *)list_head(ACCESS);
                        if (item == value) access = READWRITE;
                        list_shift_down(ACCESS);
                    }
                    while (list_head(ACCESS) != head);
                }
                free(key);
            }
            while (*end);
            if (value) break;
            scope = scope->parent;
        }
        while (scope);
    }
    /* Apply default access permissions if at local scope */
    if (scope == STATE) access = READWRITE;
    symbol = malloc(sizeof(struct symbol));
    symbol->value = value;
    symbol->scope = state;
    symbol->access = access;
    return symbol;
}

struct value *evaluate_expr(struct parser *, struct value *, struct list *,
        struct list *);

    struct list *
args_get(struct parser *PARSER, struct value *STATE, struct list *BREAKS,
        struct list *ACCESS, int NUM)
    /* Removes NUM arguments from parsers token stream, optionally seperated by
     * ANs. NUM < 0 retrieves as many arguments as possible.  Caller code
     * should check to make sure the number of arguments actually returned
     * matches the arity of the function. */
{
    struct list *args = NULL;
    struct value *arg = NULL;
    int start = NUM;
    assert(PARSER);
    assert(STATE);
    assert(BREAKS);
    args = list_create(data_delete_value, data_copy_value);
    while (NUM--) {
        if ((arg = evaluate_expr(PARSER, STATE, BREAKS, ACCESS)) == NULL) break;
        list_push_back(args, arg);
        if (NUM) parser_cmp(PARSER, "AN");
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
    struct list *list = list_create(data_delete_value, data_copy_value);
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
    }
    list_delete(LIST);
    return list;
}

    int
evaluate_parser(struct parser *PARSER, struct value *STATE, struct list *BREAKS,
        struct list *ACCESS)
    /* Evaluates all of the remaining tokens in PARSER and returns the result
     * of the last evaluated expression. The arguments which are used for the
     * state of the program are not modified but are visible to the evaluated
     * code. Returns 0 if the entire parser was evaluated, and 1 otherwise. */
{
    assert(PARSER);
    assert(STATE);
    assert(BREAKS);
    while (!parser_empty(PARSER)) {
        struct value *value = NULL;
        /* Skip over any null tokens */
        while (parser_cmp(PARSER, NULL));
        /* OBTW ... TLDR */
        /* Note that this needs to be here because it cannot appear wherever an
         * expression appears but rathar at the beginning of a new line */
        if (parser_cmp(PARSER, "OBTW"))
            list_delete(parser_seek(PARSER, "TLDR"));
        /* Evaluate an expression */
        else if (!(value = evaluate_expr(PARSER, STATE, BREAKS, ACCESS)))
            return 1;
        /* Update IT */
        if (value) state_write(value_get_bukkit(STATE), "IT", value);
        /* We should be left with a null token */
        if (!parser_cmp(PARSER, NULL)) {
            error(PARSER, "Unexpected token");
            /* TODO: try returning the unexpected token and deleting it
             * later */
            return 1;
        }
    }
    return 0;
}

    struct value *
evaluate_expr(struct parser *PARSER, struct value *STATE, struct list *BREAKS,
        struct list *ACCESS)
    /* Evaluates the next valid expression present in PARSER's token stream */
{
    struct token *token = NULL;
    struct value *value = NULL;
    struct symbol *symbol = NULL;
    assert(PARSER);
    assert(STATE);
    assert(BREAKS);

    /* HAI */
    if (parser_cmp(PARSER, "HAI")) {
        struct value *scope = NULL;
        struct list *body = NULL;
        struct parser *parser = NULL;
        if (!parser_cmp(PARSER, "1.2") && !parser_cmp(PARSER, "1.3")
                && !parser_cmp_peek(PARSER, NULL)) {
            error(PARSER, "Expected version after `HAI'");
            return NULL;
        }
        scope = value_create_bukkit(STATE);
        body = parser_seek(PARSER, "KTHXBYE");
        list_pop_front(body);      /* <NULL> */
        list_pop_back(body);       /* KTHXBYE */
        parser = parser_create_bind(PARSER->name, body);
        if (evaluate_parser(parser, scope, BREAKS, ACCESS)) {
            parser_delete(parser);
            list_delete(body);
            return NULL;
        }
        parser_delete(parser);
        list_delete(body);
        /* We cannot access scope from anywhere else, so don't name it */
        state_write(value_get_bukkit(STATE), "", scope);
        return value_create_noob();
    }

    /* VISIBLE */
    if (parser_cmp(PARSER, "VISIBLE")) {
        /* Retrieve all arguments */
        struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, -1);
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to DIFF OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to DIFF OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to PRODUKT OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to PRODUKT OF");
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
            error(PARSER, "Expected `OF' after `QUOSHUNT'");
            return NULL;
        }
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to QUOSHUNT OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to QUOSHUNT OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to MOD OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to MOD OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to BIGGR OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to BIGGR OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to SMALLR OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to SMALLR OF");
            return NULL;
        }
        return func_foldl(values, func_smallrof);
    }

    /* BOTH OF, BOTH SAEM */
    if (parser_cmp(PARSER, "BOTH")) {
        if (parser_cmp(PARSER, "OF")) {
            struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
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
                return NULL;
            }
            return func_foldl(values, func_bothof);
        }
        else if (parser_cmp(PARSER, "SAEM")) {
            struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
            struct list *values = NULL;
            void *head = NULL;
            int types[2] = { NUMBR, NUMBAR };
            if (list_size(args) != 2) {
                error(PARSER, "Wrong number of arguments to BOTH SAEM");
                list_delete(args);
                return NULL;
            }
            head = list_head(args);
            do {
                struct value *value = (struct value *)list_head(args);
                list_shift_down(args);
                if (value->type != NUMBR && value->type != NUMBAR) {
                    list_delete(args);
                    return value_create_troof(FAIL);
                }
            }
            while (head != list_head(args));
            values = args_convert(args, types, 2);
            if (!values) {
                error(PARSER, "Invalid argument to BOTH SAEM");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to EITHER OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to EITHER OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to WON OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to WON OF");
            return NULL;
        }
        return func_foldl(values, func_wonof);
    }

    /* NOT */
    if (parser_cmp(PARSER, "NOT")) {
        /* Retrieve one argument */
        struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, 1);
        struct list *values = NULL;
        struct value *value = NULL;
        struct value *result = NULL;
        int types[1] = { TROOF };
        /* Check for correct number of arguments */
        if (list_size(args) != 1) {
            error(PARSER, "Wrong number of arguments to NOT");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to NOT");
            return NULL;
        }
        /* Apply the NOT operation */
        value = (struct value *)list_head(values);
        if (value->type != TROOF) {
            error(PARSER, "Expected TROOF");
            list_delete(values);
            return NULL;
        }
        if (value_get_troof(value) == WIN) result = value_create_troof(FAIL);
        else result = value_create_troof(WIN);
        list_delete(values);
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, -1);
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to ALL OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to ALL OF");
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
        args = args_get(PARSER, STATE, BREAKS, ACCESS, -1);
        if (list_size(args) == 0) {
            error(PARSER, "No arguments supplied to ANY OF");
            list_delete(args);
            return NULL;
        }
        values = args_convert(args, types, 1);
        if (!values) {
            error(PARSER, "Invalid argument to ANY OF");
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
        struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, 2);
        struct list *values = NULL;
        int types[2] = { NUMBR, NUMBAR };
        void *head = NULL;
        if (list_size(args) != 2) {
            error(PARSER, "Wrong number of arguments to DIFFRINT");
            list_delete(args);
            return NULL;
        }
        head = list_head(args);
        do {
            struct value *value = (struct value *)list_head(args);
            list_shift_down(args);
            if (value->type != NUMBR && value->type != NUMBAR) {
                list_delete(args);
                return value_create_troof(FAIL);
            }
        }
        while (head != list_head(args));
        values = args_convert(args, types, 2);
        if (!values) {
            error(PARSER, "Invalid argument to DIFFRINT");
            return NULL;
        }
        return func_foldl(values, func_diffrint);
    }

    /* SMOOSH */
    if (parser_cmp(PARSER, "SMOOSH")) {
        struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS, -1);
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
            return NULL;
        }
        parser_cmp(PARSER, "MKAY");
        return func_foldl(values, func_smoosh);
    }

    /* ... HAS A ... ITZ A */
    if (parser_cmp_at(PARSER, 1, "HAS")) {
        struct token *token = parser_get(PARSER);
        struct value *value = NULL;
        struct symbol *symbol = token_to_symbol(STATE, ACCESS, token);
        struct value *target = symbol->value;
        enum access access = symbol->access;
        token_delete(token);
        free(symbol);
        if (!target) {
            error(PARSER, "Invalid assignment target");
            return NULL;
        }
        if (target->type != BUKKIT) {
            error(PARSER, "Invalid assignment target type");
            return NULL;
        }
        if (access != READWRITE) {
            error(PARSER, "Assignment target is not writable");
            return NULL;
        }
        if (!parser_cmp(PARSER, "HAS")) {
            error(PARSER, "Expected `HAS'");
            return NULL;
        }
        if (!parser_cmp(PARSER, "A")) {
            error(PARSER, "Expected `A' after `HAS'");
            return NULL;
        }
        /* Make sure next token is non-NULL and unique */
        /* TODO: check for reserved keyword */
        if (parser_cmp_peek(PARSER, NULL) || !(token = parser_get(PARSER))) {
            error(PARSER, "Variable name expected");
            return NULL;
        }
        if (state_find(value_get_bukkit(target), token->data, 0)) {
            error(PARSER, "Variable previously declared");
            return NULL;
        }
        /* Check for initialization */
        if (parser_cmp(PARSER, "ITZ")) {
            /* Check for empty initialization */
            if (parser_cmp(PARSER, "A")) {
                if (parser_cmp(PARSER, "NOOB"))
                    value = value_create_noob();
                else if (parser_cmp(PARSER, "TROOF"))
                    value = value_create_troof(FAIL);
                else if (parser_cmp(PARSER, "NUMBR"))
                    value = value_create_numbr(0);
                else if (parser_cmp(PARSER, "NUMBAR"))
                    value = value_create_numbar(0.0);
                else if (parser_cmp(PARSER, "YARN"))
                    value = value_create_yarn("");
                else if (parser_cmp(PARSER, "BUKKIT"))
                    value = value_create_bukkit(target);
                else {
                    error(PARSER, "Type expected");
                    return NULL;
                }
            }
            /* Inheritance */
            else if (parser_cmp(PARSER, "LIEK")) {
                struct token *token = NULL;
                struct value *parent = NULL;
                if (!parser_cmp(PARSER, "A")) {
                    error(PARSER, "Expected `A' after `ITZ LIEK'");
                    return NULL;
                }
                token = parser_get(PARSER);
                symbol = token_to_symbol(STATE, ACCESS, token);
                token_delete(token);
                parent = symbol->value;
                access = symbol->access;
                free(symbol);
                if (!parent) {
                    error(PARSER, "Invalid parent object");
                    return NULL;
                }
                if (access != READONLY && access != READWRITE) {
                    error(PARSER, "Parent object is not readable");
                    return NULL;
                }
                /* Keep track of parent parent */
                value = value_copy(parent);
                value->parent = parent;
            }
            else value = evaluate_expr(PARSER, STATE, BREAKS, ACCESS);
            if (!value) {
                error(PARSER, "Expected expression after `ITZ'");
                return NULL;
            }
        }
        /* Otherwise create a NOOB type variable */
        else value = value_create_noob();
        state_write(value_get_bukkit(target), token->data, value);
        token_delete(token);
        return value_create_noob();
    }

    /* R */
    if (parser_cmp_at(PARSER, 1, "R")) {
        struct value *target = NULL, *value = NULL;
        struct token *token = parser_get(PARSER);
        struct symbol *symbol = token_to_symbol(STATE, ACCESS, token);
        enum access access;
        token_delete(token);
        target = symbol->value;
        access = symbol->access;
        free(symbol);
        /* Sanity check */
        if (!target) {
            error(PARSER, "Invalid assignment target");
            return NULL;
        }
        if (access != READWRITE) {
            error(PARSER, "Assignment target is not writable");
            return NULL;
        }
        /* Remove the R from the token stream */
        parser_cmp(PARSER, "R");
        /* Retrieve the value to assign */
        value = evaluate_expr(PARSER, STATE, BREAKS, ACCESS);
        if (!value) {
            error(PARSER, "Invalid assignment expression");
            return NULL;
        }
        /* Write the variable value */
        value_replace(target, value);
        return value_create_noob();
    }

    /* ... IS NOW A */
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
        value = state_read(value_get_bukkit(STATE), token->data);
        if (parser_cmp(PARSER, "TROOF"))
            state_write(value_get_bukkit(STATE), token->data,
                    value_cast_troof(value));
        else if (parser_cmp(PARSER, "YARN"))
            state_write(value_get_bukkit(STATE), token->data,
                    value_cast_yarn(value));
        else if (parser_cmp(PARSER, "NUMBR"))
            state_write(value_get_bukkit(STATE), token->data,
                    value_cast_numbr(value));
        else if (parser_cmp(PARSER, "NUMBAR"))
            state_write(value_get_bukkit(STATE), token->data,
                    value_cast_numbar(value));
        else if (parser_cmp(PARSER, "NOOB"))
            state_write(value_get_bukkit(STATE), token->data,
                    value_cast_noob(value));
        else {
            error(PARSER, "Expected type");
            return NULL;
        }
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
        value = state_read(value_get_bukkit(STATE), token->data);
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
        else {
            error(PARSER, "Expected type");
            return NULL;
        }
        token_delete(token);
        return value;
    }

    /* GIMMEH */
    if (parser_cmp(PARSER, "GIMMEH")) {
        struct token *token = parser_get(PARSER);
        struct value *value = NULL;
        yarn line = NULL;
        size_t size = 0;
        if (!token || !(value =
                    state_read(value_get_bukkit(STATE), token->data))) {
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
        /* TODO: Do we want to save the trailing newline? */
        if (line[strlen(line) - 1] == '\n') {
            if (line[strlen(line) - 1] == '\r') {
                line[strlen(line) - 2] = '\0';
            }
            else line[strlen(line) - 1] = '\0';
        }
        state_write(value_get_bukkit(STATE),
                token->data, value_create_yarn(line));
        free(line);
        token_delete(token);
        return value_create_noob();
    }

    /* WTF? */
    if (parser_cmp(PARSER, "WTF?")) {
        struct value *it = NULL;
        struct list *find = NULL;
        if (!STATE || !(it =
                state_read(value_get_bukkit(STATE), "IT"))) return NULL;
        find = list_create(data_delete_token, data_copy_token);
        list_push_back(find, token_create_str("OMG"));
        list_push_back(find, token_create_str("OMGWTF"));
        list_push_back(find, token_create_str("OIC"));
        state_save(value_get_bukkit(STATE));
        list_push_front(BREAKS, token_create_str("OIC"));
        while (!parser_empty(PARSER)) {
            /* OMG */
            list_delete(parser_seek_list(PARSER, find));
            if (parser_cmp(PARSER, "OMG")) {
                struct value *value = evaluate_expr(PARSER, STATE, BREAKS, ACCESS);
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
                    state_restore(value_get_bukkit(STATE));
                    return value_create_noob();
                }
                continue;
            }
            else if (parser_cmp(PARSER, "OMGWTF")
                    || parser_cmp(PARSER, "OIC")) {
                list_delete(find);
                state_restore(value_get_bukkit(STATE));
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
        struct token *token = list_head(BREAKS);
        /* Disambiguate OICs by matching openings and closings */
        if (!strcmp(token->data, "OIC")) {
            int count = 0;
            while (!parser_empty(PARSER)) {
                if (parser_cmp(PARSER, "WTF?") || (parser_cmp(PARSER, "O")
                        && parser_cmp(PARSER, "RLY?")))
                    count++;
                else if (parser_cmp(PARSER, "OIC")) count--;
                else token_delete(parser_get(PARSER));
                if (count < 0) break;
            }
        }
        else list_delete(parser_seek(PARSER, token->data));
        list_pop_front(BREAKS);
        /* If we read to the end, keep the null token */
        if (parser_empty(PARSER)) parser_unget(PARSER);
        return value_create_noob();
    }

    /* OIC */
    if (parser_cmp(PARSER, "OIC")) {
        if (!strcmp(((struct token *)list_head(BREAKS))->data, "OIC"))
            list_pop_front(BREAKS);
        return value_create_noob();
    }

    /* O RLY?, O HAI */
    if (parser_cmp(PARSER, "O")) {
        if (parser_cmp(PARSER, "RLY?")) {
            struct value *value = NULL;
            struct list *find = NULL;
            if (!(value = state_read(value_get_bukkit(STATE), "IT"))) return NULL;
            /* Save current scope state, restore it before returning */
            state_save(value_get_bukkit(STATE));
            if (value_get_troof(value) == WIN) {
                /* YA RLY */
                find = list_create(data_delete_token, data_copy_token);
                list_push_back(find, token_create_str("YA"));
                list_push_back(find, token_create_str("OIC"));
                list_delete(parser_seek_list(PARSER, find));
                list_delete(find);
                /* Found OIC */
                if (!parser_cmp(PARSER, "YA")) return value_create_noob();
                /* Found YA */
                if (!parser_cmp(PARSER, "RLY")) {
                    error(PARSER, "Expected `RLY' after `YA'");
                    return NULL;
                }
                state_restore(value_get_bukkit(STATE));
                return value_create_noob();
            }
            else {
                struct value *expr = NULL;
                find = list_create(data_delete_token, data_copy_token);
                list_push_back(find, token_create_str("MEBBE"));
                list_push_back(find, token_create_str("NO"));
                list_push_back(find, token_create_str("OIC"));
                while (!parser_empty(PARSER)) {
                    list_delete(parser_seek_list(PARSER, find));
                    /* MEBBE */
                    if (parser_cmp(PARSER, "MEBBE")) {
                        if (!(expr = evaluate_expr(PARSER, STATE, BREAKS, ACCESS))) {
                            error(PARSER, "Expected expression after `MEBBE'");
                            list_delete(find);
                            return NULL;
                        }
                        if (value_get_troof(expr) == FAIL) continue;
                        /* TODO: Is this the proper way to handle IT? Modified only
                         * when WIN?  At all? */
                        state_write(value_get_bukkit(STATE), "IT", expr);
                        list_delete(find);
                        state_restore(value_get_bukkit(STATE));
                        return value_create_noob();
                    }
                    /* NO WAI */
                    else if (parser_cmp(PARSER, "NO")) {
                        if (parser_cmp(PARSER, "WAI")) {
                            list_delete(find);
                            state_restore(value_get_bukkit(STATE));
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
                        state_restore(value_get_bukkit(STATE));
                        return value_create_noob();
                    }
                }
            }
        }
        else if (parser_cmp(PARSER, "HAI")) {
            struct token *name = NULL;
            struct value *state = NULL;
            struct list *body = NULL, *breaks = NULL, *grants = NULL;
            struct parser *parser = NULL;
            if (!parser_cmp(PARSER, "IM")) {
                error(PARSER, "Expected `IM' after `O HAI'");
                return NULL;
            }
            name = parser_get(PARSER);
            if (parser_cmp(PARSER, "IM")) {
                struct token *token = NULL;
                struct symbol *symbol = NULL;
                struct value *value = NULL;
                enum access access;
                if (!parser_cmp(PARSER, "LIEK")) {
                    error(PARSER, "Expected `LIEK' after `IM'");
                    return NULL;
                }
                token = parser_get(PARSER);
                symbol = token_to_symbol(STATE, ACCESS, token);
                token_delete(token);
                value = symbol->value;
                access = symbol->access;
                free(symbol);
                if (!value) {
                    error(PARSER, "Parent value does not exist");
                    return NULL;
                }
                if (value->type != BUKKIT) {
                    error(PARSER, "Invalid parent type");
                    return NULL;
                }
                if (access != READONLY && access != READWRITE) {
                    error(PARSER, "Parent is not readable");
                    return NULL;
                }
                /* Keep track of parent */
                state = value_copy(value);
                state->parent = value;
            }
            else state = value_create_bukkit(STATE);
            /* Get the body of the O HAI block */
            body = parser_seek(PARSER, "KTHX");
            list_pop_front(body);      /* <NULL> */
            list_pop_back(body);       /* KTHX */
            /* Set up the initial state of the block (empty) */
            breaks = list_create(data_delete_token, data_copy_token);
            grants = list_create(data_delete_null, data_copy_null);
            /* Evaluate the block under the state generated above */
            parser = parser_create_bind(PARSER->name, body);
            assert(!evaluate_parser(parser, state, breaks, grants));
            /* Save the object and clean up */
            state_write(value_get_bukkit(STATE), name->data, state);
            list_delete(breaks);
            list_delete(grants);
            parser_delete(parser);
            list_delete(body);
            return value_create_noob();
        }
        else {
            error(PARSER, "Expected token after `O'");
            return NULL;
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

    /* IM IN YR ..., IM OUTTA YR ... */
    if (parser_cmp(PARSER, "IM")) {
        struct token *name = NULL;
        struct token *op = NULL;
        struct token *var = NULL;
        struct list *breaks = NULL;
        struct list *update = NULL;
        struct list *body = NULL;
        struct list *guard = NULL;
        struct parser *parser = NULL;
        struct value *result = NULL;
        if (!parser_cmp(PARSER, "IN")) {
            error(PARSER, "Expected `IN' after `IM'");
            return NULL;
        }
        if (!parser_cmp(PARSER, "YR")) {
            error(PARSER, "Expected `YR' after `IM IN'");
            return NULL;
        }
        name = parser_get(PARSER);
        if (!parser_cmp_peek(PARSER, NULL)) {
            op = parser_get(PARSER);
            if (!parser_cmp(PARSER, "YR")) {
                error(PARSER, "Expected `YR' after loop operation");
                token_delete(name);
                token_delete(op);
                return NULL;
            }
            var = parser_get(PARSER);
            if (!state_read(value_get_bukkit(STATE), var->data)) {
                error(PARSER, "Loop variable not found");
                token_delete(name);
                token_delete(op);
                token_delete(var);
                return NULL;
            }
            /* Generate update */
            update = list_create(data_delete_token, data_copy_token);
            list_push_back(update, token_create_str(var->data));
            list_push_back(update, token_create_str("R"));
            if (!strcmp(op->data, "UPPIN"))
                list_push_back(update, token_create_str("SUM"));
            else if (!strcmp(op->data, "NERFIN"))
                list_push_back(update, token_create_str("DIFF"));
            else {
                error(PARSER, "Expected `UPPIN' or `NERFIN'");
                token_delete(name);
                token_delete(op);
                token_delete(var);
                list_delete(update);
                return NULL;
            }
            list_push_back(update, token_create_str("OF"));
            list_push_back(update, token_create_str(var->data));
            list_push_back(update, token_create_str("AN"));
            list_push_back(update, token_create_str("1"));
            list_push_back(update, token_create_null(0));
            /* Generate guard */
            if (parser_cmp(PARSER, "TIL")) {
                guard = parser_seek(PARSER, NULL);
                list_push_front(guard, token_create_str("NOT"));
            }
            else if (parser_cmp(PARSER, "WILE"))
                guard = parser_seek(PARSER, NULL);
            else if (parser_cmp(PARSER, NULL)) {
                guard = list_create(data_delete_token, data_copy_token);
                list_push_back(guard, token_create_str("WIN"));
                list_push_back(guard, token_create_null(0));
            }
            else {
                error(PARSER, "Invalid loop condition; expected `TIL' or `WILE'");
                token_delete(name);
                token_delete(op);
                token_delete(var);
                list_delete(guard);
                return NULL;
            }
        }
        /* Read in body */
        body = parser_seek(PARSER, (const char *)name->data);
        list_pop_back(body);  /* loop name */
        list_pop_back(body);  /* YR */
        list_pop_back(body);  /* OUTTA */
        list_pop_back(body);  /* IM */
        /* Evaluate loop */
        while (1) {
            troof proceed = WIN;
            state_save(value_get_bukkit(STATE));
            if (guard) {
                /* Perform guard check */
                parser = parser_create_bind(PARSER->name, guard);
                result = evaluate_expr(parser, STATE, BREAKS, ACCESS);
                parser_delete(parser);
                if (!result) {
                    error(PARSER, "Invalid expression");
                    return NULL;
                }
                if (result->type != TROOF) {
                    error(PARSER, "Expected guard to return TROOF");
                    value_delete(result);
                    return NULL;
                }
                proceed = value_get_troof(result);
                value_delete(result);
            }
            if (proceed == WIN) {
                /* Evaluate loop body */
                breaks = list_create(data_delete_token, data_copy_token);
                list_push_front(breaks, token_create_str(""));
                parser = parser_create_bind(PARSER->name, body);
                if (evaluate_parser(parser, STATE, breaks, ACCESS)) {
                    error(PARSER, "Unable to evaluate");
                    list_delete(breaks);
                    value_delete(result);
                    parser_delete(parser);
                    return NULL;
                }
                parser_delete(parser);
                /* Check if we broke out of loop */
                if (list_empty(breaks)) {
                    list_delete(breaks);
                    break;
                }
                list_delete(breaks);
                if (update) {
                    /* Update loop variable */
                    parser = parser_create_bind(PARSER->name, update);
                    assert(!evaluate_parser(parser, STATE, BREAKS, ACCESS));
                    parser_delete(parser);
                }
            }
            else break;
            state_restore(value_get_bukkit(STATE));
        }
        state_restore(value_get_bukkit(STATE));
        if (guard) list_delete(guard);
        if (update) {
            list_delete(update);
            token_delete(var);
            token_delete(op);
        }
        list_delete(body);
        token_delete(name);
        return value_create_noob();
    }

    /* HOW DUZ ..., IF U SAY SO */
    if (parser_cmp(PARSER, "HOW")) {
        struct token *token = NULL;
        struct item *head = NULL;
        struct list *args = NULL, *body = NULL;
        struct symbol *symbol = NULL;
        struct value *object = NULL;
        enum access access;
        if (!parser_cmp(PARSER, "DUZ")) {
            error(PARSER, "Expected `DUZ' after `HOW'");
            return NULL;
        }
        token = parser_get(PARSER);
        symbol = token_to_symbol(STATE, ACCESS, token);
        token_delete(token);
        object = symbol->value;
        access = symbol->access;
        free(symbol);
        if (!object) {
            error(PARSER, "Expected variable after `HOW DUZ'");
            return NULL;
        }
        if (object->type != BUKKIT) {
            error(PARSER, "Expected object after `HOW DUZ'");
            return NULL;
        }
        if (access != READWRITE) {
            error(PARSER, "Object is not writable");
            return NULL;
        }
        args = list_create(data_delete_token, data_copy_token);
        token = parser_get(PARSER);
        while (parser_cmp(PARSER, "YR")) {
            struct token *arg = NULL;
            void *head = NULL;
            /* Make sure next token is non-NULL and unique */
            /* TODO: check for reserved keywords */
            if (!(arg = parser_get(PARSER))) {
                error(PARSER, "Argument name expected");
                list_delete(args);
                return NULL;
            }
            /* Check for duplicate argument names */
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
            if (!parser_cmp(PARSER, "AN")) break;
            else if (!parser_cmp_peek(PARSER, "YR")) {
                error(PARSER, "Expected `YR' after `AN'");
                list_delete(args);
                return NULL;
            }
        }
        /* Save function in object */
        body = parser_seek(PARSER, "IF");
        list_pop_front(body);
        list_pop_back(body);
        if (!parser_cmp(PARSER, "U")) {
            error(PARSER, "Expected `U' after `IF'");
            list_delete(args);
            return NULL;
        }
        if (!parser_cmp(PARSER, "SAY")) {
            error(PARSER, "Expected `SAY' after `IF U'");
            list_delete(args);
            return NULL;
        }
        if (!parser_cmp(PARSER, "SO")) {
            error(PARSER, "Expected `SO' after `IF U SAY'");
            list_delete(args);
            return NULL;
        }
        state_write(value_get_bukkit(object), token->data,
                value_create_funkshun(args, body));
        token_delete(token);
        return value_create_noob();
    }

    /* FOUND YR */
    if (parser_cmp(PARSER, "FOUND")) {
        struct value *value = NULL;
        struct token *token = NULL;
        if (!parser_cmp(PARSER, "YR")) {
            error(PARSER, "Expected `YR' after `FOUND'");
            return NULL;
        }
        if (!(value = evaluate_expr(PARSER, STATE, BREAKS, ACCESS))) {
            error(PARSER, "Expected expression after `FOUND YR'");
            return NULL;
        }
        token = list_head(BREAKS);
        list_delete(parser_seek(PARSER, token->data));
        list_pop_front(BREAKS);
        /* If we read to the end, keep the null token */
        if (parser_empty(PARSER)) parser_unget(PARSER);
        return value_copy(value);
    }

    /* MAH */
    if (parser_cmp(PARSER, "MAH")) {
        /* Check for at least one argument */
        if (parser_cmp(PARSER, NULL)) {
            error(PARSER, "No arguments supplied to MAH");
            return NULL;
        }
        /* Keep getting tokens and adding them to ACCESS */
        do {
            struct token *token = parser_get(PARSER);
            struct symbol *symbol = token_to_symbol(STATE, ACCESS, token);
            token_delete(token);
            value = symbol->value;
            free(symbol);
            if (!value) {
                error(PARSER, "Variable does not exist");
                return NULL;
            }
            list_push_front(ACCESS, value);
            parser_cmp(PARSER, "AN");
        }
        while (!parser_cmp_peek(PARSER, NULL));
        return value_create_noob();
    }

    /* We must be left with a value */
    token = parser_get(PARSER);
    if (!token) return NULL;

    /* PRIMITIVE TYPES */
    if ((value = token_to_yarn(PARSER, STATE, token))
            || (value = token_to_numbr(token))
            || (value = token_to_numbar(token))
            || (value = token_to_troof(token))) 
        return value;

    /* STATE */
    symbol = token_to_symbol(STATE, ACCESS, token);
    if (symbol->value) {
        enum access access = symbol->access;
        struct value *value = symbol->value;
        struct value *scope = symbol->scope;
        token_delete(token);
        free(symbol);
        if (access != READONLY && access != READWRITE) {
            error(PARSER, "Value is not readable");
            return NULL;
        }
        if (value->type == NOOB) return value_cast_noob(value);
        if (value->type == TROOF) return value_cast_troof(value);
        if (value->type == NUMBR) return value_cast_numbr(value);
        if (value->type == NUMBAR) return value_cast_numbar(value);
        if (value->type == YARN) return value_cast_yarn(value);
        if (value->type == FUNKSHUN) {
            struct parser *parser = NULL;
            struct list *breaks = NULL;
            struct list *grants = NULL;
            struct list *body = NULL;
            struct funkshun *funkshun = value_get_funkshun(value);
            struct value *result = NULL, *local = NULL;
            void *head = NULL;
            int status = 0;
            /* Retrieve all arguments */
            struct list *args = args_get(PARSER, STATE, BREAKS, ACCESS,
                    list_size(funkshun->args));
            /* Check for correct arity */
            if (list_size(args) != list_size(funkshun->args)) {
                error(PARSER, "Invalid number of arguments");
                list_delete(args);
                return NULL;
            }
            /* Populate local scope with arguments */
            local = value_create_bukkit(scope);
            if (list_size(funkshun->args) > 0) {
                head = list_head(funkshun->args);
                do {
                    struct token *arg = (struct token *)list_head(funkshun->args);
                    state_write(value_get_bukkit(local), arg->data,
                            value_copy((struct value *)list_head(args)));
                    list_pop_front(args);
                    list_shift_down(funkshun->args);
                }
                while (list_head(funkshun->args) != head);
            }
            list_delete(args);
            /* Set up state and breaks */
            state_write(value_get_bukkit(local), "IT", value_create_noob());
            breaks = list_create(data_delete_token, data_copy_token);
            list_push_front(breaks, token_create_str(""));
            grants = list_create(data_delete_null, data_copy_null);
            /* Copy function body, to enable recursion */
            body = list_copy(funkshun->body);
            parser = parser_create_bind(PARSER->name, body);
            if (evaluate_parser(parser, local, breaks, grants)) {
                list_delete(grants);
                parser_delete(parser);
                value_delete(local);
                list_delete(body);
                list_delete(breaks);
                return NULL;
            }
            list_delete(grants);
            parser_delete(parser);
            result = value_copy(state_read(value_get_bukkit(local), "IT"));
            value_delete(local);
            list_delete(body);
            list_delete(breaks);
            return result;
        }
        if (value->type == BUKKIT) return value_copy(value);
    }
    free(symbol);

    /* UNABLE TO EVALUATE */
    parser_unget(PARSER);
    token_delete(token);
    return NULL;
}

    int
main(int ARGC, char **ARGV)
{
    struct parser *parser = NULL;
    struct list *ignore = NULL;
    struct list *keep = NULL;
    struct list *breaks = NULL;
    struct value *state = NULL;
    struct list *access = NULL;
    FILE *file = stdin;
    int n, status;
    /* Print help text if appropriate */
    if (get_arg(ARGC, ARGV, 'h', &n)) {
        fprintf(stderr, "USAGE: lolcode [-f FILENAME]\n");
        status = 2;
        goto DONE;
    }
    ignore = list_create(data_delete_token, data_copy_token);
    list_push_back(ignore, token_create_str(" "));
    list_push_back(ignore, token_create_str("\t"));
    list_push_back(ignore, token_create_str("...\n"));
    keep = list_create(data_delete_token, data_copy_token);
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
    /* Set up initial state */
    state = value_create_bukkit(NULL);
    breaks = list_create(data_delete_token, data_copy_token);
    /* Note that we should never break from main body */
    list_push_front(breaks, token_create_str("KTHXBYE"));
    access = list_create(data_delete_null, data_copy_null);
    /* Initialize the default IT variable */
    state_write(value_get_bukkit(state), "IT", value_create_noob());
    status = evaluate_parser(parser, state, breaks, access);
DONE:
    list_delete(access);
    list_delete(breaks);
    value_delete(state);
    parser_delete(parser);
    list_delete(ignore);
    list_delete(keep);
    return status;
}
