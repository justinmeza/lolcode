/*
 * state.h
 *
 * Saves and restores multiple scopes of state. Reads and writes occur in the
 * scope where their value is stored, except when writes create a new variable
 * -- this is done in the current scope.
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
 *    - Add a state_remove function to un-scope values
 *
 * EXAMPLE
 * 
 *          void
 *      data_delete_int(void *DATA)
 *          // Specifies how to delete pointers to ints
 *      {
 *          free(DATA);
 *      }
 *
 *          int
 *      main(void)
 *          // Creates a state variable, uses it to store scopes of integer
 *          // values, and prints 1230456123.
 *      {
 *          int *my_data_1 = malloc(sizeof(int));
 *          int *my_data_2 = malloc(sizeof(int));
 *          struct state *state = state_create(data_delete_int, 1);
 *          *my_data_1 = 123, *my_data_2 = 456;
 *          state_insert(state, my_data_1, "my_data_1");
 *          printf("%d", state_read(state, "my_data_1"));
 *          state_write(state, "my_data_1", 0);
 *          printf("%d", state_read(state, "my_data_1"));
 *          state_save(state);
 *          state_insert(state, my_data_2, "my_data_1");
 *          printf("%d", state_read(state, "my_data_1"));
 *          state_restore(state);
 *          printf("%d", state_read(state, "my_data_1"));
 *          state_delete(state);
 *          return 0;
 *      }
 */

#ifndef __STATE__
#define __STATE__

#include "hash.h"
#include "list.h"

/* Structures and functions required for internal implementation. These need
 * not be dealt with directly. */

struct state {
    struct list *data;
    unsigned int size;
    void (*delete)(void *);
};

    void
data_delete_hash(void *DATA)
    /* Deletes pointers to hashes */
{
    hash_delete(DATA);
}

/* Functions for use with states */

    struct state *
state_create(void (*DELETE)(void *), unsigned int SIZE)
    /* Creates a state with a hash of SIZE buckets */
{
    struct state *state = NULL;
    struct hash *hash = NULL;
    if (!DELETE) return NULL;
    state = malloc(sizeof(struct state));
    state->data = list_create(data_delete_hash);
    state->size = SIZE;
    state->delete = DELETE;
    hash = hash_create(state->delete, state->size);
    list_push_front(state->data, hash);
    return state;
}

    void
state_delete(struct state *STATE)
    /* Deletes a state and all its data */
{
    if (!STATE) return;
    list_delete(STATE->data);
    free(STATE);
}

    void
state_save(struct state *STATE)
    /* Saves the current state. Restore it later with state_restore. */
{
    struct hash *hash = NULL;
    if (!STATE) return;
    hash = hash_create(STATE->delete, STATE->size);
    list_push_front(STATE->data, hash);
}

    void
state_restore(struct state *STATE)
    /* Restores a state saved with state_save. */
{
    if (!STATE || list_empty(STATE->data)) return;
    list_pop_front(STATE->data);
}

    void *
state_find(struct state *STATE, void *KEY, unsigned int DEPTH)
    /* Searches for data referenced by KEY within STATE at a particular DEPTH
     * of scopes. The current scope has depth 0, the scope before it depth 1,
     * and so on. */
{
    struct list *list = NULL;
    void *head = NULL, *data = NULL;
    unsigned int depth = 0;
    if (!STATE || !KEY) return NULL;
    list = STATE->data;
    if (DEPTH > list->size) return NULL;
    head = list_head(list);
    do {
        struct hash *hash = (struct hash *)list_head(list);
        if (depth == DEPTH) data = hash_find(hash, KEY);
        /* TODO: Optimize this because most hits will be local -- there is no
         * sense shifting the rest of the list around. */
        list_shift_down(list);
        depth++;
    }
    while (list_head(list) != head);
    return data;
}

    void *
state_read(struct state *STATE, void *KEY)
    /* Reads the most local version of data referenced by KEY */
{
    struct list *list = NULL;
    void *head = NULL, *data = NULL;
    if (!STATE || !KEY) return NULL;
    list = STATE->data;
    head = list_head(list);
    do {
        struct hash *hash = (struct hash *)list_head(list);
        if (data == NULL) data = hash_find(hash, KEY);
        /* TODO: Optimize this because most hits will be local -- there is no
         * sense shifting the rest of the list around. */
        list_shift_down(list);
    }
    while (list_head(list) != head);
    return data;
}

    void
state_write(struct state *STATE, void *KEY, void *DATA)
    /* Writes to the most local version of data referenced by KEY. If the data
     * must be created, it is placed in the current scope. */
{
    struct list *list = NULL;
    void *head = NULL;
    int found = 0;
    if (!STATE || !KEY) return;
    list = STATE->data;
    head = list_head(list);
    do {
        struct hash *hash = (struct hash *)list_head(list);
        if (!found && hash_find(hash, KEY)) {
            hash_remove(hash, KEY);
            hash_insert(hash, KEY, DATA);
            found = 1;
        }
        /* TODO: Optimize this because most hits will be local -- there is no
         * sense shifting the rest of the list around. */
        list_shift_down(list);
    }
    while (list_head(list) != head);
    if (!found) hash_insert((struct hash *)head, KEY, DATA);
}

    void
state_insert(struct state *STATE, void *KEY, void *DATA)
    /* Inserts DATA referenced by KEY in the current scope. If KEY already
     * references data, nothing is modified. */
{
    struct list *list = NULL;
    struct hash *hash = NULL;
    int found = 0;
    if (!STATE || !KEY) return;
    list = STATE->data;
    hash = (struct hash *)list_head(list);
    if (!hash_find(hash, KEY)) hash_insert(hash, KEY, DATA);
}

#endif /* __STATE__ */
