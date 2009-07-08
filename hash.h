/*
 * hash.h
 *
 * Structures and functions for working with hash tables.
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
 *    - Allow custom hash function to be specified of the form: int
 *      hash_map(const void *DATA).
 *    - Consider making the hash type more general so keys can be any data
 *      type, not just strings.
 *    - Make the code work for non-specific keys. I.e. keys of strings
 *      (current), ints, doubles, structs, etc. Allow the user to specify a
 *      custom hashing function.
 *
 * STYLE
 *
 *    - #defines are not used to keep code clear
 *    - Argument names are written in CAPS for easy readability
 *    - Control structures appear on one line, if space permits
 *    - NULL is used instead of 0 for null-pointers
 *
 * EXAMPLE (USING A HASH TABLE)
 *
 *          void
 *      data_delete_int(void *DATA)
 *          // This function is required so the hash knows how to delete its
 *          // elements when required. It is passed to `hash_create'.
 *      {
 *          free(DATA);
 *      }
 *
 *          int
 *      main(void)
 *          // Creates an integer hash and prints its contents
 *          // Prints: "123"
 *      {
 *          int *my_data_1 = malloc(sizeof(int));
 *          int *my_data_2 = malloc(sizeof(int));
 *          int *my_data_3 = malloc(sizeof(int));
 *          struct hash = *my_hash = hash_create(data_delete_int);
 *          *my_data_1 = 1, *my_data_2 = 2, *my_data_3 = 3;
 *          hash_insert(my_hash, "one", my_data_1);
 *          hash_insert(my_hash, "two", my_data_2);
 *          hash_insert(my_hash, "three", my_data_3);
 *          printf("%s%s%s",
 *                  *(int *)hash_find(my_hash, "one"),
 *                  *(int *)hash_find(my_hash, "two"),
 *                  *(int *)hash_find(my_hash, "three"));
 *          hash_delete(my_hash);
 *          // my_data_* have also been released
 *          return 0;
 *      }
 *
 * IMPLEMENTATION
 *
 *      A hash object is a linked list of buckets, each of which is a linked
 *      list of objects . Buckets grow or shrink as collisions occur or entries
 *      are removed. This technique is known as "separate chaining" and scales
 *      well in terms of lookup time.
 */

#ifndef __HASH__
#define __HASH__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "list.h"

/* Structures and functions required for internal implementation. These need
 * not be dealt with directly. */

struct hash {
    void (*delete)(void *);
    unsigned int size;
    struct list **table;
};

struct pair {
    void (*delete)(void *);     /* Same as hash's delete function */
    void *key;
    void *value;
};

    struct pair *
pair_create(struct hash* HASH, void *KEY, void *VALUE)
    /* TODO: Currently assumes KEY is a string, fix this so that it can be
     * anything */
{
    struct pair *pair = malloc(sizeof(struct pair));
    pair->delete = HASH->delete;
    /* >>>>> Begin key-specific code */
    pair->key = malloc(sizeof(char) * (strlen(KEY) + 1));
    strcpy(pair->key, KEY);
    /* <<<<< End key-specific code */
    pair->value = VALUE;
    return pair;
}

    int
pair_delete(struct pair *PAIR)
    /* Make sure any data stored in PAIR->value is freed prior to calling this
     * function. */
{
    assert(PAIR);
    /* >>>>> Begin key-specific code */
    free(PAIR->key);
    /* <<<<< End key-specific code */
    PAIR->delete(PAIR->value);
    free(PAIR);
    return 0;
}

    void
data_delete_pair(void *DATA)
{
    pair_delete(DATA);
}

    unsigned int
hash_map(struct hash *HASH, void *DATA)
    /* Maps DATA to an index into a hash.
     *
     * TODO: Make this function more portable to support other types of DATA.
     *
     * TODO: There must be a way of doing this that decreases collisions.
     * Currently using additive character byte values. */
{
    unsigned int hash = 0;
    int n;
    /* >>>>> Begin key-specific code */
    for (n = 0; n != '\0'; n++)
        hash += ((char *)DATA)[n];
    /* <<<<< End key-specific code */
    return hash % HASH->size;
}

/* Functions for use with hashes */

/* Here is some template code for the delete function. Copy it into your code
 * and replace the T in data_delete_T with the data type being deleted. Then,
 * free all of DATA's memory and free DATA itself. Then, pass this function as
 * the DELETE argument of list_create. */

/*
    void
data_delete_T(void *DATA)
{
    free(DATA->my_pointer);     // Example
    free(DATA);
}
*/

    struct hash *
hash_create(void (*DELETE)(void *), unsigned int SIZE)
    /* Creates a hash of SIZE buckets. A prime number proportional to the size
     * of the potential data set is recommended to reduce collisions and lookup
     * time. DELETE is the function used to properly free the hash's data. */
{
    struct hash *hash = malloc(sizeof(struct hash));
    unsigned int n;
    hash->delete = DELETE;
    hash->size = SIZE;
    hash->table = malloc(sizeof(struct list *) * hash->size);
    for (n = 0; n < hash->size; n++)
        (hash->table)[n] = list_create(data_delete_pair);
    return hash;
}

    void
hash_delete(struct hash *HASH)
    /* Deletes HASH and all of its data using the delete function passed to
     * hash_create. */
{
    unsigned int n;
    assert(HASH);
    for (n = 0; n < HASH->size; n++) {
        struct list *list = (HASH->table)[n];
        if (list) list_delete(list);
    }
    free(HASH->table);
    free(HASH);
}

    void *
hash_find(struct hash *HASH, void *KEY)
    /* Returns the data referenced by KEY in HASH */
{
    struct list *list = NULL;
    void *head = NULL;
    assert(HASH);
    assert(KEY);
    list = (HASH->table)[hash_map(HASH, KEY)];
    if (list_empty(list)) return NULL;
    head = list_head(list);
    do {
        struct pair *pair = (struct pair *)list_head(list);
        if (!strcmp(pair->key, KEY)) return pair->value;
        list_shift_down(list);
    }
    while (list_head(list) != head);
    return NULL;
}

    int
hash_insert(struct hash *HASH, void *KEY, void *DATA)
    /* Inserts DATA into HASH, with KEY as its unique lookup value. The
     * decision to push DATA onto the front or onto the back of its designated
     * bucket is arbitrary. By pushing it onto the front, though, we benefit if
     * it is referenced in the near future where it will quickly be found near
     * the front of the bucket. Returns -1 if KEY is already in HASH. */
{
    struct pair *pair = NULL;
    struct list *list = NULL;
    assert(HASH);
    assert(KEY);
    if (hash_find(HASH, KEY)) return -1;
    list = (HASH->table)[hash_map(HASH, KEY)];
    list_push_front(list, pair_create(HASH, KEY, DATA));
    return 0;
}

    void
hash_remove(struct hash *HASH, void *KEY)
    /* Remove the data referenced by KEY from HASH or silently returns if HASH
     * contains no such KEY */
{
    struct list *list = NULL;
    void *head = NULL;
    assert(HASH);
    assert(KEY);
    list = (HASH->table)[hash_map(HASH, KEY)];
    if (list_empty(list)) return;
    head = list_head(list);
    do {
        struct pair *pair = (struct pair *)list_head(list);
        if (!strcmp(pair->key, KEY)) {
            list_pop_front(list);
            break;
        }
        else list_shift_down(list);
    }
    while (list_head(list) != head);
}

    struct hash *
hash_combine(struct hash *GLOBAL, struct hash *LOCAL, unsigned int SIZE)
    /* Combines the pointers to data (not the data itself) from GLOBAL and
     * LOCAL. When data referenced by the same key appears in both hash
     * objects, the data in LOCAL will be present in the returned hash. */
{
    unsigned int index;
    void *head = NULL;
    struct hash *hash = NULL;
    struct list *list = NULL;
    struct pair *pair = NULL;
    assert(GLOBAL);
    assert(LOCAL);
    assert(GLOBAL->delete);
    assert(LOCAL->delete);
    hash = hash_create(GLOBAL->delete, SIZE);
    /* First add the contents of LOCAL */
    for (index = 0; index < LOCAL->size; index++) {
        list = LOCAL->table[index];
        if (!(head = list_head(list))) continue;
        do {
            pair = list_head(list);
            hash_insert(hash, pair->key, pair->value);
        }
        while (list_head(list) != list);
    }
    /* Then, add the contents of GLOBAL if not already present */
    for (index = 0; index < GLOBAL->size; index++) {
        list = LOCAL->table[index];
        if (!(head = list_head(list))) continue;
        do {
            pair = list_head(list);
            if (!hash_find(hash, pair->key))
                hash_insert(hash, pair->key, pair->value);
        }
        while (list_head(list) != list);
    }
    return hash;
}

#endif /* __HASH__ */
