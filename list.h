/*
 * list.h
 *
 * Structures and functions for working with linked lists.
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
 * EXAMPLE (MANIPULATING A LIST)
 *
 *          void
 *      data_delete_int(void *DATA)
 *          // Specifies how to delete ints
 *      {
 *          free(DATA);
 *      }
 *
 *          int
 *      main(void)
 *          // Creates an integer list and prints its contents, 123456789.
 *      {
 *          int *my_data_1 = malloc(sizeof(int));
 *          int *my_data_2 = malloc(sizeof(int));
 *          int *my_data_3 = malloc(sizeof(int));
 *          struct list *my_list = list_create(data_delete_int);
 *          *my_data_1 = 123, *my_data_2 = 456, *my_data_3 = 789;
 *          list_push_back(my_list, my_data_1);
 *          list_push_back(my_list, my_data_2);
 *          list_push_back(my_list, my_data_3);
 *          while (!list_empty(my_list)) {
 *              printf("%d", *(int *)list_head(my_list));
 *              list_pop_front(my_list);
 *          }
 *          list_delete(my_list);
 *          // list_delete frees my_data_*
 *          return 0;
 *      }
 *
 * EXAMPLE (ITERATING OVER A LIST)
 *
 *      // Iterates over LIST
 *      void *head = list_head(LIST);
 *      do {
 *          // Replace each struct item with the type stored in LIST
 *          struct item *item = (struct item *)list_head(LIST);
 *          // Process item...
 *          list_shift_down(LIST);
 *      }
 *      while (list_head(LIST) != head);
 *
 * IMPLEMENTATION
 *
 *      A list is implemented as a doubly-linked list:
 *
 *          +----------+----------+-\-----\-+----------+
 *          |   item   |   item   | /     / |   item   |
 *          |+--------+|+--------+| \ ... \ |+--------+|
 *     list --> ||  data  |||  data  || / ... / ||  data  ||
 *          |+--------+|+--------+| \     \ |+--------+|
 *          +----------+----------+-/-----/-+----------+
 *           head                tail
 */

#ifndef __LIST__
#define __LIST__

#include <assert.h>

/* Structures and functions required for internal implementation. These need
 * not be dealt with directly. */

struct item {
    void *data;
    struct item *next;
    struct item *prev;
};

struct list {
    void (*delete)(void *);
    struct item *head;
    struct item *tail;
    unsigned int size;
};

    struct item *
item_create(void *DATA)
    /* Creates a new item holding a pointer to DATA */
{
    struct item *item = malloc(sizeof(struct item));
    item->data = DATA;
    item->next = NULL;
    item->prev = NULL;
    return item;
}

    void
item_delete(struct list *LIST, struct item *ITEM)
    /* Calls ITEM's delete function and then frees ITEM */
{
    assert(ITEM);
    if (ITEM->data) LIST->delete(ITEM->data);
    free(ITEM);
}

/* Functions for use with lists */

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

    struct list *
list_create(void (*DELETE)(void *))
    /* Creates a list whose contents is meant to be deleted using DELETE. See
     * data_delete_T for more information. */
{
    struct list *list = malloc(sizeof(struct list));
    list->delete = DELETE;
    list->head = NULL;
    list->tail = NULL;
    list->size = 0;
    return list;
}

    void
list_delete(struct list *LIST)
    /* Deletes every item in LIST and frees LIST */
{
    struct item *current = NULL;
    struct item *next = NULL;
    assert(LIST);
    /* Iterate through LIST and delete all items */
    current = LIST->head;
    while (current) {
        next = current->next;
        item_delete(LIST, current);
        current = next;
    }
    free(LIST);
}

    int
list_empty(struct list *LIST)
    /* Returns a value greater than 0 if LIST is non-empty */
{
    assert(LIST);
    return !LIST->size;
}

    int
list_size(struct list *LIST)
    /* Returns the number of elements in a list */
{
    assert(LIST);
    return LIST->size;
}

    int
list_shift_down(struct list *LIST)
    /* Makes the head item the tail, shifting all items toward the head.
     * Returns the size of the list.  */
{
    assert(LIST);
    assert(!list_empty(LIST));
    if (LIST->head == LIST->tail) return 1;
    else {
        struct item *save = LIST->head->next;
        LIST->head->next->prev = NULL;
        LIST->head->next = NULL;
        LIST->head->prev = LIST->tail;
        LIST->tail->next = LIST->head;
        LIST->tail = LIST->head;
        LIST->head = save;
    }
    return LIST->size;
}

    int
list_shift_up(struct list *LIST)
    /* Makes the tail item the head, shifting all items toward the tail.
     * Returns the size of the list. */
{
    assert(LIST);
    assert(!list_empty(LIST));
    if (LIST->head == LIST->tail) return 1;
    else {
        struct item *save = LIST->tail->prev;
        LIST->tail->prev->next = NULL;
        LIST->tail->prev = NULL;
        LIST->tail->next = LIST->head;
        LIST->head->prev = LIST->tail;
        LIST->head = LIST->tail;
        LIST->tail = save;
    }
    return LIST->size;
}

    int
list_push_back(struct list *LIST,
        void *DATA)
    /* Pushes DATA onto the back of LIST and returns its new size */
{
    struct item *item = NULL;
    assert(LIST);
    item = item_create(DATA);
    if (list_empty(LIST)) {
        item->next = NULL;
        item->prev = NULL;
        LIST->head = item;
        LIST->tail = item;
    }
    else {
        item->prev = LIST->tail;
        item->next = NULL;
        LIST->tail->next = item;
        LIST->tail = item;
    }
    return ++LIST->size;
}

    int
list_push_front(struct list *LIST, void *DATA)
    /* Pushes DATA onto the front of LIST and returns its new size */
{
    struct item *item = NULL;
    assert(LIST);
    item = item_create(DATA);
    if (item) {
        if (list_empty(LIST)) {
            item->next = NULL;
            item->prev = NULL;
            LIST->head = item;
            LIST->tail = item;
        }
        else {
            item->prev = NULL;
            item->next = LIST->head;
            LIST->head->prev = item;
            LIST->head = item;
        }
        return ++LIST->size;
    }
    return -1;
}

    int
list_move_front(struct list *TO, struct list* FROM)
    /* Moves the front of FROM to the front of TO using a pointer swap.  FROM
     * must contain at least 1 item. Returns the new size of TO. */
{
    struct item *item = NULL;
    assert(FROM);
    assert(TO);
    assert(list_size(FROM) > 0);
    item = FROM->head;
    /* Perform a pop without deleting item */
    if (FROM->head == FROM->tail) {
        FROM->head = NULL;
        FROM->tail = NULL;
    }
    else {
        FROM->head->next->prev = NULL;
        FROM->head = FROM->head->next;
    }
    FROM->size--;
    /* Perform a push */
    if (list_empty(TO)) {
        item->next = NULL;
        item->prev = NULL;
        TO->head = item;
        TO->tail = item;
    }
    else {
        item->prev = NULL;
        item->next = TO->head;
        TO->head->prev = item;
        TO->head = item;
    }
    return ++TO->size;
}

    int
list_pop_front(struct list *LIST)
    /* Deletes the first item of LIST and returns LIST's new size */
{
    struct item *item = NULL;
    assert(LIST);
    item = LIST->head;
    if (LIST->head == LIST->tail) {
        LIST->head = NULL;
        LIST->tail = NULL;
    }
    else {
        LIST->head->next->prev = NULL;
        LIST->head = LIST->head->next;
    }
    if (item) {
        item_delete(LIST, item);
        LIST->size--;
    }
    return LIST->size;
}

    int
list_pop_back(struct list *LIST)
    /* Deletes the last item of LIST and returns LIST's new size */
{
    struct item *item = NULL;
    assert(LIST);
    item = LIST->tail;
    if (LIST->head == LIST->tail) {
        LIST->head = NULL;
        LIST->tail = NULL;
    }
    else {
        LIST->tail->prev->next = NULL;
        LIST->tail = LIST->tail->prev;
    }
    if (item) {
        item_delete(LIST, item);
        LIST->size--;
    }
    return LIST->size;
}

    void *
list_head(struct list *LIST)
    /* Returns the data stored in the first item of LIST */
{
    assert(LIST);
    assert(LIST->head);
    return LIST->head->data;
}

    void *
list_tail(struct list *LIST)
    /* Returns the data stored in the last item of LIST */
{
    assert(LIST);
    assert(LIST->tail);
    return LIST->tail->data;
}

    void
list_print(struct list *LIST)
    /* Displays the addresses and linkage of a list */
{
    struct item *item;
    assert(LIST);
    item = LIST->head;
    printf("======\n");
    printf("<LIST> Size: %4d\n", list_size(LIST));
    while (item != 0) {
        if (item == LIST->head) printf("<HEAD> ");
        if (item == LIST->tail) printf("<TAIL> ");
        if (item != LIST->head && item != LIST->tail) printf("       ");
        printf("Addr: %8p, Prev: %8p, Next: %8p; Data: %8p\n",
                item,
                item->prev,
                item->next,
                item->data);
        item = item->next;
    }
}

    void
list_print_str(struct list *LIST, void (*PRINT)(void *))
    /* Displays the addresses and linkage of a list, treating data like
     * strings */
{
    struct item *item;
    assert(LIST);
    item = LIST->head;
    printf("======\n");
    printf("<LIST> Size: %4d\n", list_size(LIST));
    while (item != 0) {
        if (item == LIST->head) printf("<HEAD> ");
        if (item == LIST->tail) printf("<TAIL> ");
        if (item != LIST->head && item != LIST->tail) printf("       ");
        printf("Addr: %8p, Prev: %8p, Next: %8p; Data: ",
                item,
                item->prev,
                item->next);
        PRINT(item->data);
        printf("\n");
        item = item->next;
    }
}

#endif /* __LIST__ */
