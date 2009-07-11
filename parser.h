/*
 * parser.h
 *
 * A simple text file parser. Given a file name, a list of literals to ignore
 * and split tokens by, and a list of literals to keep and split tokens by, the
 * parser will generate a steady stream of tokens based on the contents of the
 * file. The entire file is not read into the parser at once, rather, new lines
 * of tokens are grabbed as needed. Data for each token (such as line number
 * and column position) are stored with the token as well.
 *
 * MAINTAINER
 *
 *      Justin J. Meza < justin dot meza at gmail dot com >
 *
 * TODO
 *
 *    - Look at the TODO for list_delete... might solve a memory problem
 *    - Add a parser_cmp_list to compare a list of consecutive tokens.
 *    - Find places to change ints to unsigned ints
 *    - Update header info and example
 *    - Note terminology of token stream
 *
 * USAGE
 *
 *      A parser object takes as its arguments a file name and two lists of
 *      tokens which we shall call "ignore" and "keep". The file is then
 *      _intuitively_ parsed as follows (*NOT* _actually_ for obvious lack of
 *      efficiency):
 *
 *      (1) Create tokens of strings of characters separated by any number of
 *      elements in the ignore list.
 *
 *      (2) From the set of newly-created tokens, for every token containing an
 *      element in the keep list, split it into up to three new tokens
 *      containing the portion of the old token *before* the keep token (if
 *      any), the keep token itself (this is guaranteed to be present and
 *      thus always be "kept" as a token), and the portion of the old token
 *      *after* the keep token (if any).
 *
 *      Thus, *NONE* of the tokens present in the ignore token list are kept in
 *      the token stream, and *ALL* tokens present in the keep token list are
 *      kept -- in place -- in the token stream. You may take these "rules" as
 *      invariants for the parser.
 *
 *      (Though this parser was effectively designed as a deterministic finite
 *      state machine, we will refrain from using any automata theory
 *      terminology and keep the discussion in "layman's terms.")
 *
 * EXAMPLE (OVERVIEW)
 *
 *      The road goes ever on an on,
 *          Down from the door where it began.
 *
 *      For example, the file containing "The road goes ever on and on,\n\tDown
 *      from the door where it began.\n\n" can be broken up many ways:
 *
 *      Ignoring "whitespace" (commonly, " ", "\n", and "\t") yields the
 *      individual tokens: "The", "road", "goes", "ever", "on", "and", "on,",
 *      "Down", "from", "the", "door", "where", "it", "began.".
 *
 *      But notice that we might want to actually delimit tokens by punctuation
 *      (splitting by whitespace alone keeps punctuation attached to the ends
 *      of tokens), too. If we add "," and "." to our list of tokens to ignore,
 *      this can be done.
 *
 *      Or, consider the practical application of simply ignoring "\n\n" *only*
 *      and tokenizing on the remaining text -- this will yield stanzas of text
 *      as tokens, for example.
 *
 *      There are many other ways to tokenize text but sometimes we want to do
 *      so and keep the token we split on in the token stream to provide the
 *      program parsing the file some extra context. For example, say we wanted
 *      to leave the period punctuation mark (".") in the token stream to
 *      signal the end of a sentence as its own token; this is one of the many
 *      times when keeping split tokens is required. After ignoring whitespace
 *      and keeping ".", our tokens will look like: "The", "road", "goes",
 *      "ever", "on", "and", "on,", "Down", "from", "the","door", "where",
 *      "it", "began", "." (notice the last token (a period) is all by itself,
 *      signaling the end of a sentence).
 *
 *      Of course there are many, *MANY*, other ways of breaking up text,
 *      uniquely suitable for this parser. There may be equally many ways of
 *      optimizing the process of parsing for certain workloads but for the
 *      sake of generality this exercise is left to the hacker for whom it is
 *      important. We'll provide some tweaks which can be employed in any
 *      scenario, below.
 *
 * TWEAKING
 *
 *      The simplest performance tweak involves placing the tokens you
 *      anticipate to appear most frequently in the ignore and keep lists
 *      toward the head of the list so they are matched earlier on during
 *      comparison.
 *
 *      To help avoid some of the time penalty for disk access, the parser is
 *      able to extract a minimum number of bytes from a file at a single time.
 *      If a particular line of a fine is larger than the minimum number of
 *      bytes, the entire line is read and tokenized, however if the line is
 *      smaller, multiple lines may be read before tokenizing begins. This can
 *      be set by modifying the BUFFER argument passed to parser_create.
 *
 * CAVEATS
 * 
 *      Keeping a portion of a longer token to be ignored is well-defined: the
 *      portion of the token to be kept from the split is placed in the stream
 *      and the rest of the token is discarded (intuitively, the token to be
 *      ignored is _replaced_ by the token to keep). This is *okay* because it
 *      obeys our invariants for tokenizing: no elements of the ignore token
 *      list are present in the token stream and all elements of the split
 *      token list are kept -- in place -- in the token stream.
 *
 *      The result of ignoring tokens present within other tokens to ignore is
 *      undefined.
 */

#ifndef __PARSER__
#define __PARSER__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "getline.h" /* A portable `getline' function, slightly modified */
#include "list.h"

/* Structures and functions required for internal implementation. These need
 * not be dealt with directly. */

struct token {
    char *data;
    unsigned int line;
    unsigned int column;
    unsigned int null;
};

struct parser {
    FILE *fd;
    char *name;
    unsigned int line;                  /* Current line */
    unsigned int column;                /* Current column */
    unsigned int buffer;                /* File buffer size */
    struct list *ignore;
    struct list *keep;
    struct list *tokens;
    int (*rules)(char *, size_t, unsigned int *, unsigned int *);
};

    unsigned int
token_list_cmp(struct list *LIST, char *BUFFER, unsigned int SIZE)
{
    void *head = list_head(LIST);
    do {
        struct token *token = (struct token *)list_head(LIST);
        unsigned int size = strlen(token->data);
        if (size <= SIZE
                && !strncmp(token->data, BUFFER, size))
            return size;
        list_shift_down(LIST);
    }
    while (list_head(LIST) != head);
    return 0;
}

/* Functions for use with tokens */

    struct token *
token_create_str(const char *STRING)
    /* Creates a new token containing STRING. Used for comparison, where line
     * and column are not taken into account. */
{
    struct token *token = malloc(sizeof(struct token));
    token->data = malloc(sizeof(char) * (strlen(STRING) + 1));
    strcpy(token->data, STRING);
    token->line = 0;
    token->column = 0;
    token->null = 0;
    return token;
}

    struct token *
token_create_null(unsigned int LINE)
    /* Creates a new null token. Null tokens act like normal tokens except for
     * the fact that their null value is non-zero. Their textual representation
     * is the string <NULL>. They may be compared just like any other token and
     * provide a convenient way of detecting a token division. */
{
    struct token *token = malloc(sizeof(struct token));
    token->data = malloc(sizeof(char) * 7);
    strcpy(token->data, "<NULL>");
    token->line = LINE;
    token->column = 0;
    token->null = 1;
    return token;
}

    struct token *
token_create(const char *VALUE, unsigned int SIZE, unsigned int LINE,
        unsigned int COLUMN, unsigned int _NULL)
    /* Creates a new token containing the string starting at VALUE and running
     * for SIZE characters, located at LINE and COLUMN and optionally being
     * _NULL. */
{
    struct token *token = malloc(sizeof(struct token));
    token->data = malloc(sizeof(char) * (SIZE + 1));
    strncpy(token->data, VALUE, SIZE);
    token->data[SIZE] = '\0'; /* Must null-terminate ourselves with strncpy */
    token->line = LINE;
    token->column = COLUMN;
    token->null = _NULL;
    return token;
}

    void
token_delete(void *TOKEN)
    /* Properly deletes a token by freeing it as well as the value it holds */
{
    struct token *token = (struct token *)TOKEN;
    assert(TOKEN);
    if (token->data) free(token->data);
    free(TOKEN);
}

/* Functions for use with parsers */

/* Here is some template code for the rules function. It is called before the
 * parser attempts to tokenize according to the split and keep rules. BUF is a
 * constant pointer to the characters stored in the parsers buffer, LEN is the
 * length of the buffer, START is a pointer to the index where the next token
 * to parse begins, and POS is a pointer to the index of the current character
 * under inspection in the buffer. BUF and LEN are not modifiable, START and
 * POS are. Returning 0 resumes normal parsing, returning anything else
 * advances the buffer to the next line to be parsed. If an error in
 * encountered while applying a rule, returning 1 is a suitable action. */

/*
    int
parser_rules(char *BUF, size_t LEN, unsigned int *START, unsigned int *POS)
    // The order of these rules is important!
{
    // String literals
    if (BUF[*POS] == '"') {
        do if (BUF[(*POS)++] == '\n') return 1;
        while (*POS < LEN && (BUF[*POS] != '"' || (*POS > 0 && BUF[*POS - 1] == ':')));
    }
    // Single-line comments
    if (!strncmp(BUF + *POS, "BTW", 3) && !(*POS > 0 && BUF[*POS - 1] == 'O')) {
        *POS += 3;
        while (*POS < LEN && BUF[*POS] != '\n') *START = ++(*POS);
    }
    return 0;
}
*/

    struct parser *
parser_create(FILE *_FILE, const char *NAME, struct list *IGNORE, struct list *KEEP, unsigned int BUFFER,
        int (*RULES)(char *, size_t, unsigned int *, unsigned int *))
    /* Creates a new parser which reads from _FILE, splitting tokens separated
     * by strings in the IGNORE and KEEP lists, but leaving any tokens in KEEP
     * in the token stream. BUFFER is a lower bound on the number of token to
     * read ahead for when requesting more tokens. RULES is a pointer to a
     * function designed to override some of the standard parsing behavior (an
     * example is given above). */
{
    struct parser *parser = malloc(sizeof(struct parser));
    parser->fd = _FILE;
    parser->name = malloc(sizeof(char) * (strlen(NAME) + 1));
    strcpy(parser->name, NAME);
    parser->tokens = list_create(token_delete);
    parser->ignore = IGNORE;
    parser->keep = KEEP;
    parser->line = 0;
    parser->column = 0;
    parser->buffer = BUFFER;
    parser->rules = RULES;
    return parser;
}

    void
parser_delete(struct parser *PARSER)
    /* Properly deletes a parser by closing the file it referenced and cleaning
     * up its list of tokens waiting to be read. */
{
    assert(PARSER);
    if (PARSER->fd != NULL) fclose(PARSER->fd);
    list_delete(PARSER->tokens);
    free(PARSER->name);
    free(PARSER);
}

    int
parser_empty(struct parser *PARSER)
    /* Detects if a parser contains no more tokens. Returns 1 if empty and 0 if
     * not. */
{
    assert(PARSER);
    assert(PARSER->fd);
    if (list_size(PARSER->tokens) == 0
            && (feof(PARSER->fd) || ferror(PARSER->fd))) return 1;
    return 0;
}

    struct token *
parser_get(struct parser *PARSER)
    /* Retrieves the next token from a parser and caches some additional tokens
     * (up to the next line) in the token list for future access.  In general,
     * we follow the procedure:
     *
     * 0 While we do not have any non-null tokens,
     * 0.1 Make sure our file is in an acceptable state
     * 0.2 Get lines until we fill our buffer
     * 0.2.1 If we were able to retrieve data,
     * 0.2.1.1 Apply some general rules which override others
     * 0.2.1.2 Check for any data to ignore but split tokens upon
     * 0.2.1.3 Check for any data to keep and split tokens upon
     * 0.2.1.4 Divide our input buffer into tokens
     * 1 Return the next token in the token stream */
{
    struct item *item = NULL;
    struct token *token = NULL;
    struct token *saved = NULL;
    void *head = NULL;
    assert(PARSER);
    assert(!parser_empty(PARSER));
    /* 0 While we do not have any non-null tokens, */
    while(list_empty(PARSER->tokens) ||
            (list_size(PARSER->tokens) == 1 &&
            ((struct token *)list_tail(PARSER->tokens))->null))  {
        char *buf = NULL;
        size_t len = 0, p = 0;
        unsigned int start = 0, pos, ignore = 0, keep = 0, size, r;
    /* 0.1 Make sure our file is in an acceptable state */
        if (feof(PARSER->fd)) return NULL;
    /* 0.2 Get lines until we fill our buffer */
        do r = getline_at(&buf, &p, &len, PARSER->fd);
        while(r >= 0 && p < PARSER->buffer);
        len = strlen(buf);
    /* 0.2.1 If we were able to retrieve data, */
        if (buf == NULL || len == 0) continue;
        for (pos = 0; pos <= len; pos++) {
    /* 0.2.1.1 Apply some general rules which override others */
            if (PARSER->rules && PARSER->rules(buf, len, &start, &pos)) break;
    /* 0.2.1.2 Check for any data to ignore but split tokens upon */
            if (PARSER->ignore) ignore =
                token_list_cmp(PARSER->ignore, buf + pos, len - pos);
    /* 0.2.1.3 Check for any data to keep and split tokens upon */
            if (PARSER->keep) keep =
                token_list_cmp(PARSER->keep, buf + pos, len - pos);
    /* 0.2.1.4 Divide our input buffer into tokens */
            if (ignore || pos == len) {
                if (pos > start) {
                    token = token_create(buf + start,
                            pos - start,
                            PARSER->line,
                            start,
                            0);
                    list_push_back(PARSER->tokens, token);
                }
                start = pos + ignore;
                ignore = 0;
            }
            else if (keep) {
                if (pos > start) {
                    token = token_create(buf + start,
                            pos - start,
                            PARSER->line,
                            start,
                            0);
                    list_push_back(PARSER->tokens, token);
                    /* Check for multiple null tokens */
                    token = (struct token *)list_tail(PARSER->tokens);
                    if (token && !token->null) list_push_back(PARSER->tokens,
                            token_create_null(PARSER->line));

                }
                else if (pos == start) {
                    token = (struct token *)list_tail(PARSER->tokens);
                    /* Check for no tokens or multiple null tokens */
                    if (!token || !token->null) list_push_back(PARSER->tokens,
                            token_create_null(PARSER->line));
                }
                start = pos + keep;
                keep = 0;
            }
        }
        PARSER->line++;
        PARSER->column = 0;
        free(buf);
    }
    /* 1 Return the next token in the token stream */
    token = (struct token *)list_head(PARSER->tokens);
    saved = token_create(token->data,
            strlen(token->data),
            token->line,
            token->column,
            token->null);
    list_pop_front(PARSER->tokens);
    return saved;
}

    int
parser_cmp(struct parser *PARSER, const char *TOKEN)
    /* Compares TOKEN to the next token in the token stream and if equivalent,
     * returns 1 and removes the token from the stream.  Otherwise, 0 is
     * returned and the stream remains unaltered. */
{
    struct token *token = NULL;
    assert(PARSER);
    token = parser_get(PARSER);
    assert(token);
    /* Check for null tokens and token string equality */
    if ((!TOKEN && !token->null)
            || (TOKEN && strcmp(TOKEN, token->data))) {
        list_push_front(PARSER->tokens, token);
        return 0;
    }
    token_delete(token);
    return 1;
}

    int
parser_cmp_at(struct parser *PARSER, int POS, const char *TOKEN)
    /* Seeks to an arbitrary position POS in the token stream and returns 1 if
     * the token value at that position is the same as STR and 0 if not. */
{
    struct item *item;
    struct token *token;
    unsigned int n;
    assert(POS >= 0);
    assert(PARSER);
    assert(list_size(PARSER->tokens) > POS);
    /* Seek */
    for (item = PARSER->tokens->head, n = 0;
            item != NULL && n < POS;
            n++, item = item->next) ;
    token = (struct token *)item->data;
    /* Check for null tokens and token string equality */
    if ((!TOKEN && !token->null) || (TOKEN && strcmp(TOKEN, token->data)))
        return 0;
    return 1;
}

    int
parser_cmp_peek(struct parser *PARSER, struct token *TOKEN)
    /* Performs the same action as parser_cmp but does not modify the token
     * stream in any case. */
{
    struct token *token = NULL;
    assert(PARSER);
    assert(TOKEN);
    assert(PARSER->tokens);
    assert(PARSER->tokens->head);
    token = (struct token *)list_head(PARSER->tokens);
    if ((TOKEN->null && !token->null) || strcmp(TOKEN->data, token->data))
        return 0;
    return 1;
}

    int
parser_ignore(struct parser *PARSER, const char *TOKEN)
    /* Ignores a particular token if it appears next in the token stream */
{
    struct token *token = NULL;
    assert(PARSER);
    token = parser_get(PARSER);
    assert(token);
    /* Check for null tokens and token string equality */
    if ((!TOKEN && !token->null) || strcmp(TOKEN, token->data)) {
        list_push_front(PARSER->tokens, token);
        return 0;
    }
    token_delete(token);
    return 1;
}

    struct list *
parser_seek(struct parser *PARSER, const char *TOKEN)
    /* Seek up to and including the first occurrence of a particular token,
     * removing any tokens visited along the way. */
{
    struct list *list = NULL;
    struct token *token = NULL;
    assert(PARSER);
    list = list_create(token_delete);
    do {
        token = parser_get(PARSER);
        if (!token) continue;
        /* TODO: Can both of the list_push_backs be moved here? (Maybe similar
         * for other functions, too?) */
        if ((!TOKEN && !token->null) || !strcmp(TOKEN, token->data)) {
            list_push_back(list, token);
            return list;
        }
        list_push_back(list, token);
    }
    while (!parser_empty(PARSER));
    return list;
}

    struct list *
parser_seek_list(struct parser *PARSER, struct list *LIST)
    /* Seek to the first occurrence of one of any of the tokens present in LIST
     * and leaves the token which was found in the token stream to be checked
     * by the caller. */
{
    struct list *list = NULL;
    struct token *token = NULL;
    assert(PARSER);
    assert(LIST);
    list = list_create(token_delete);
    do {
        void *head = NULL;
        token = parser_get(PARSER);
        if (!token) continue;
        head = list_head(LIST);
        do {
            struct item *item = (struct item *)list_head(LIST);
            if (!strcmp(item->data, token->data)) {
                list_push_front(PARSER->tokens, token);
                /* TODO: Does this belong here:
                list_delete(LIST); */
                return list;
            }
            list_shift_down(LIST);
        }
        while (list_head(LIST) != head);
        list_push_back(list, token);
    }
    while (!parser_empty(PARSER));
    list_delete(LIST);
    return list;
}

    void
parser_put_back(struct parser *PARSER, struct list *LIST)
    /* Places the contents of LIST in the beginning of the token stream.  LIST
     * must be a list of strings. The contents of LIST remain unchanged. */
{
    void *tail = NULL;
    assert(PARSER);
    assert(LIST);
    tail = list_tail(LIST);
    do {
        struct token *item = (struct token *)list_tail(LIST);
        struct token *token = token_create(item->data, strlen(item->data),
                item->line, item->column, item->null);
        list_push_front(PARSER->tokens, token);
        list_shift_up(LIST);
    }
    while (list_tail(LIST) != tail);
}

#endif /* __PARSER__ */
