/*

    AVL tree implementation.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

*/

#ifndef AVLTREE_H_INCLUDED
#define AVLTREE_H_INCLUDED

#include "defs.h"
#include <stdlib.h>
#include <string.h>

typedef int (*avltree_compare_cb)(const char *, const char *);

typedef struct avltree_s {
#ifdef MEM_DEBUG
  const int magic;
#endif
  struct avlnode_s *root;
  const char *module;
  int count;		/* number of items in tree */
  int options;
  const char *key;
  int key_sz;
  avltree_compare_cb compare;
} avltree_t;

typedef struct avlnode_s {
  int balance;
  const char *key;
  const void *data;
  struct avlnode_s *left;
  struct avlnode_s *right;
} avlnode_t;

/* options for avltree_init */

#define AVLTREE_ASCENDING		0	/* default */
#define AVLTREE_DESCENDING		1
#define AVLTREE_CASE_SENSITIVE		0	/* default */
#define AVLTREE_NON_CASE_SENSITIVE	2
#define AVLTREE_STATIC_KEYS		128
#define AVLTREE_PARANOIAC		256	/* STOP on "duplicate key" (insert) or on "not found" (delete) */

/* Allocate and initialize an AVL Tree */
avltree_t *avltree_init(const int options, const char *module);

/* Destroy an AVL Tree */
void avltree_done(avltree_t *tree);

/* Delete all items of tree [tree]; returns number of deleted items */
int avltree_clear(avltree_t *tree);

/* Get number of items in tree */
int avltree_count(const avltree_t *tree);

/* Delete item with key [key] */
int avltree_delete(avltree_t *tree, const char *key);

/* Duplicate an existing tree */
avltree_t *avltree_dupe(const avltree_t *tree, const char *module);

/* Find key [key] in tree */
int avltree_find(const avltree_t *tree, const char *key, const void **data);

/* Get the first (the MIN-) entry of the tree */
const char *avltree_first(const avltree_t *tree, const void **data);

/* Get the current height of the tree */
int avltree_height(const avltree_t *tree);

/* Insert key [key] and [data] into tree */
int avltree_insert(avltree_t *tree, const char *key, const void *data);

/* Get the next (the entry above [key]) */
const char *avltree_next(const avltree_t *tree, const char *key, const void **data);


#endif /* AVLTREE_H_INCLUDED */
