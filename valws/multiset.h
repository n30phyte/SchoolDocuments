//
// Created by Michael Kwok on 4/13/21.
//

#ifndef CMPUT379_MULTISET_H
#define CMPUT379_MULTISET_H

typedef struct set_node {
    long key;
    long count;
    long height;

    struct set_node *left;
    struct set_node *right;
} set_node_t;

set_node_t *node_insert(set_node_t *node, long key);
set_node_t *node_delete(set_node_t *root, long key);
size_t node_count(set_node_t *root);


#endif //CMPUT379_MULTISET_H
