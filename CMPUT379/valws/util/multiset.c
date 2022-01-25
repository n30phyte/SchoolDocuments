//
// Created by Michael Kwok on 4/13/21.
//
#include <stddef.h>
#include <malloc.h>

#include "multiset.h"

long height(set_node_t *node) {
    if (node == NULL)
        return 0;
    return node->height;
}

long max(long a, long b) {
    return (a > b) ? a : b;
}

set_node_t *node_new(long key) {
    set_node_t *node = malloc(sizeof(set_node_t));
    node->key = key;
    node->count = 1;
    node->height = 1;

    node->left = NULL;
    node->right = NULL;
    return node;
}

set_node_t *node_rotate_right(set_node_t *y) {
    set_node_t *x = y->left;
    set_node_t *T2 = x->right;

    // Perform rotation
    x->right = y;
    y->left = T2;

    // Update heights
    y->height = max(height(y->left), height(y->right)) + 1;
    x->height = max(height(x->left), height(x->right)) + 1;

    // Return new root
    return x;
}

set_node_t *node_rotate_left(set_node_t *x) {
    set_node_t *y = x->right;
    set_node_t *T2 = y->left;

    // Perform rotation
    y->left = x;
    x->right = T2;

    //  Update heights
    x->height = max(height(x->left), height(x->right)) + 1;
    y->height = max(height(y->left), height(y->right)) + 1;

    // Return new root
    return y;
}

// Get Balance factor of node node
long node_get_balance(set_node_t *node) {
    if (node == NULL) {
        return 0;
    }
    return height(node->left) - height(node->right);
}

set_node_t *node_min_value(set_node_t *node) {
    set_node_t *current = node;

    while (current->left != NULL)
        current = current->left;

    return current;
}

set_node_t *node_insert(set_node_t *node, long key) {
    /* 1.  Perform the normal BST insertion */
    if (node == NULL) {
        return node_new(key);
    }

    if (key < node->key) {
        node->left = node_insert(node->left, key);
    } else if (key > node->key) {
        node->right = node_insert(node->right, key);
    } else if (key == node->key) {
        node->count++;
        return node;
    } else {
        return node;
    }

    node->height = 1 + max(height(node->left),
                           height(node->right));

    long balance = node_get_balance(node);

    // Left Left Case
    if (balance > 1 && key < node->left->key) {
        return node_rotate_right(node);
    }
    // Right Right Case
    if (balance < -1 && key > node->right->key)
        return node_rotate_left(node);

    // Left Right Case
    if (balance > 1 && key > node->left->key) {
        node->left = node_rotate_left(node->left);
        return node_rotate_right(node);
    }

    // Right Left Case
    if (balance < -1 && key < node->right->key) {
        node->right = node_rotate_right(node->right);
        return node_rotate_left(node);
    }

    return node;
}

set_node_t *node_delete(set_node_t *root, long key) {
    if (root == NULL) {
        return root;
    }

    if (key < root->key) {
        root->left = node_delete(root->left, key);
    } else if (key > root->key)
        root->right = node_delete(root->right, key);
    else {

        if (root->count != 1) {
            root->count--;
            return root;
        }

        // node with only one child or no child
        if ((root->left == NULL) || (root->right == NULL)) {
            set_node_t *temp = root->left ? root->left :
                               root->right;

            // No child case
            if (temp == NULL) {
                temp = root;
                root = NULL;
            } else {
                *root = *temp;
            }
            free(temp);
        } else {
            // node with two children: Get the inorder
            // successor (smallest in the right subtree)
            set_node_t *temp = node_min_value(root->right);

            // Copy the inorder successor's data to this node
            root->key = temp->key;
            root->count = temp->count;
            temp->count = 1;
            // Delete the inorder successor
            root->right = node_delete(root->right, temp->key);
        }
    }

    // If the tree had only one node then return
    if (root == NULL) {
        return root;
    }

    // STEP 2: UPDATE HEIGHT OF THE CURRENT NODE
    root->height = 1 + max(height(root->left),
                           height(root->right));

    // STEP 3: GET THE BALANCE FACTOR OF THIS NODE (to
    // check whether this node became unbalanced)
    long balance = node_get_balance(root);

    // If this node becomes unbalanced, then there are 4 cases

    // Left Left Case
    if (balance > 1 && node_get_balance(root->left) >= 0)
        return node_rotate_right(root);

    // Left Right Case
    if (balance > 1 && node_get_balance(root->left) < 0) {
        root->left = node_rotate_left(root->left);
        return node_rotate_right(root);
    }

    // Right Right Case
    if (balance < -1 && node_get_balance(root->right) <= 0)
        return node_rotate_left(root);

    // Right Left Case
    if (balance < -1 && node_get_balance(root->right) > 0) {
        root->right = node_rotate_right(root->right);
        return node_rotate_left(root);
    }

    return root;
}


size_t node_count(set_node_t *root) {
    if (root != NULL) {
        size_t left = node_count(root->left);
        size_t right = node_count(root->right);

        return left + right + 1;
    }
    return 0;
}