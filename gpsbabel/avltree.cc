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

#include "avltree.h"

#define MYNAME "avltree"

#ifdef DEBUG_MEM
#  define AVLTREE_MAGIC	0x41564c53
/* ((((((0L | 'A') << 8) | 'V') << 8) | 'L') << 8) | 'T'; */
#endif

#ifdef MEM_DEBUG
void avltree_check_handle(const void* tree);
#endif
static void avltree_node_free(const avltree_t* tree, avlnode_t* node);
static int avltree_node_height(avlnode_t* node);
static int avltree_insert_node(avltree_t* tree, avlnode_t** root, const char* key, const void* data);
static int avltree_delete_node(avltree_t* tree, const char* key, avlnode_t** root, int* changed);
static avlnode_t* avltree_right_rotation(avlnode_t* A);
static avlnode_t* avltree_left_rotation(avlnode_t* A);
static avlnode_t* avltree_left_right_rotation(avlnode_t* A);
static avlnode_t* avltree_right_left_rotation(avlnode_t* A);
static avlnode_t* avltree_dupe_node(const avltree_t* tree, const avlnode_t* node);
static int avltree_strcmpr(const char* s1, const char* s2);
static int avltree_case_ignore_strcmpr(const char* s1, const char* s2);
static avlnode_t* avltree_find_next(const avltree_t* tree, avlnode_t* node, const char* key);
static void avltree_save_key(avltree_t* tree, const char* key);


#ifdef MEM_DEBUG
# define AVLTREE_CHECK_HANDLE(a) avltree_valid_tree(a)
#else
# define AVLTREE_CHECK_HANDLE(a)
#endif

#define AVLTREE_INVALID_BALANCE "%s/%s.%d: Invalid balance %d at node \"%s\"!\n", \
	tree->module, MYNAME, __LINE__, node->balance, node->key


/* Allocate and initialize an AVL Tree */

avltree_t*
avltree_init(const int options, const char* module)
{
  avltree_t* tree;

  if ((module == NULL) || (*module == '\0')) {
    fatal(MYNAME ": 'avltree_init' should be called with a valid module name!\n");
  }

  tree = (avltree_t*) xcalloc(1, sizeof(*tree));
  tree->options = options;
  tree->module = module;

  if (options & AVLTREE_NON_CASE_SENSITIVE) {
    if (options & AVLTREE_DESCENDING) {
      tree->compare = avltree_case_ignore_strcmpr;  /* descending, non-case-sensitive */
    } else {
      tree->compare = case_ignore_strcmp;  /* ascending, non-case-sensitive */
    }
  } else {
    if (options & AVLTREE_DESCENDING) {
      tree->compare = avltree_strcmpr;  /* descending, case-sensitive */
    } else {
      tree->compare = strcmp;  /* ascending, case-sensitive */
    }
  }

  return tree;
}

/* Delete all items of tree [tree] */

int
avltree_clear(avltree_t* tree)
{
  int res;

  AVLTREE_CHECK_HANDLE(tree);

  res = tree->count;
  avltree_save_key(tree, NULL);
  if (res) {
    avltree_node_free(tree, tree->root);
    /* avltree_node_free doesn't touch 'count' */
    tree->count = 0;
    tree->root = NULL;
  }
  return res;
}

/* Destroy an AVL Tree */

void
avltree_done(avltree_t* tree)
{
  avltree_clear(tree);
  xfree(tree);
}


/* Get number of items in tree */

int
avltree_count(const avltree_t* tree)
{
  AVLTREE_CHECK_HANDLE(tree);

  return tree->count;
}


/* Delete item with key [key] */

int
avltree_delete(avltree_t* tree, const char* key)
{
  int changed = 0;

  AVLTREE_CHECK_HANDLE(tree);

  if (key == NULL)
    fatal("%s/%s.%d: Attempt to delete a NULL-pointer!\n",
          tree->module, MYNAME, __LINE__);

  return avltree_delete_node(tree, key, &tree->root, &changed);
}


/* Duplicate an existing tree */

avltree_t*
avltree_dupe(const avltree_t* tree, const char* module)
{
  avltree_t* dupe;

  AVLTREE_CHECK_HANDLE(tree);

  dupe = avltree_init(tree->options, module);
  if ((dupe->count = tree->count)) {
    dupe->root = avltree_dupe_node(tree, tree->root);
  }

  return dupe;
}


/* Find key [key] in tree */

int
avltree_find(const avltree_t* tree, const char* key, const void** data)
{
  avlnode_t* node;

  AVLTREE_CHECK_HANDLE(tree);

  node = tree->root;
  while (node) {
    int compare = tree->compare(key, node->key);

    if (compare < 0) {
      node = node->left;
    } else if (compare > 0) {
      node = node->right;
    } else {
      if (data) {
        (*data) = node->data;
      }
      break;
    }
  }

  return (node) ? 1 : 0;
}

int
avltree_find(const avltree_t* tree, const QString key, const void** data)
{
  const char*t = xstrdup(key.toUtf8().data());
  int r = avltree_find(tree, key, data);
  xfree(t);
  return r;
  
}


/* Get the first (the MIN-) entry of the tree */

const char*
avltree_first(const avltree_t* tree, const void** data)
{
  avlnode_t* node;

  AVLTREE_CHECK_HANDLE(tree);

  node = tree->root;
  if (! node) {
    return NULL;
  }

  while (node->left) {
    node = node->left;
  }
  avltree_save_key((avltree_t*)tree, node->key);
  if (data) {
    (*data) = node->data;
  }

  return tree->key;
}


/* Get the current height of the tree */

int
avltree_height(const avltree_t* tree)
{
  AVLTREE_CHECK_HANDLE(tree);

  if (tree->count) {
    return avltree_node_height(tree->root);
  } else {
    return 0;
  }
}


/* Insert key [key] and [data] into tree */

int
avltree_insert(avltree_t* tree, const char* key, const void* data)
{
  int count;

  AVLTREE_CHECK_HANDLE(tree);

  if (key == NULL)
    fatal("%s/%s.%d: Attempt to insert a NULL-pointer!\n",
          tree->module, MYNAME, __LINE__);

  count = tree->count;
  avltree_insert_node(tree, &tree->root, key, data);
  return (count != tree->count) ? 1 : 0;
}


/* Get the next (the entry above [key]) */

const char*
avltree_next(const avltree_t* tree, const char* key, const void** data)
{
  avlnode_t* node;

  AVLTREE_CHECK_HANDLE(tree);

  if (key == NULL)
    fatal("%s/%s.%d: Attempt to look for a NULL-pointer!\n",
          tree->module, MYNAME, __LINE__);

  node = tree->root;
  if (! node) {
    return NULL;
  }

  if ((node = avltree_find_next(tree, node, key))) {
    avltree_save_key((avltree_t*)tree, node->key);
    if (data) {
      (*data) = node->data;
    }
  } else {
    avltree_save_key((avltree_t*)tree, NULL);
  }

  return tree->key;
}


/* ------------------------------ static stuff ------------------------------ */


#ifdef MEM_DEBUG

void
avltree_check_handle(const avltree_t* tree)
{
  if (! tree) {
    fatal(MYNAME ": Invalid (NULL-) pointer!\n");
  }
  if (tree->magic != AVLTREE_MAGIC) {
    fatal(MYNAME ": Invalid (no AVL tree object) pointer!\n");
  }
}

#endif


static void
avltree_node_free(const avltree_t* tree, avlnode_t* node)
{
  if ((!(tree->options & AVLTREE_STATIC_KEYS)) && node->key) {
    xfree((char*)node->key);
  }
  if (node->left) {
    avltree_node_free(tree, node->left);
  }
  if (node->right) {
    avltree_node_free(tree, node->right);
  }
  xfree(node);
}


static int
avltree_node_height(avlnode_t* node)
{
  int height = 1;

  if (node->balance < 0) {
    height += avltree_node_height(node->left);
  } else if (node->right) {
    height += avltree_node_height(node->right);
  }

  return height;
}


static avlnode_t*
avltree_right_rotation(avlnode_t* A)
{
  /*
  >          A                         B
  >         / \                       / \
  >            \                     /   \
  >             B       -->>        A     .
  >            / \                 / \   / \
  >               \
  >                .
  >               / \
  */
  avlnode_t* B;

  B = A->right;
  A->right = B->left;
  B->left = A;

  /* update balance of all touched nodes */
  /* reference: <http://cmcrossroads.com/bradapp/ftp/src/libs/C++/AvlTrees.html> */

  B->balance--;
  A->balance = -(B->balance);

  return B;
}


static avlnode_t*
avltree_left_rotation(avlnode_t* A)
{
  /*
  >              A                     B
  >             / \                   / \
  >            /                     /   \
  >           B         -->>        .     A
  >          / \                   / \   / \
  >         /
  >        .
  >       / \
  */
  avlnode_t* B;

  B = A->left;
  A->left = B->right;
  B->right = A;

  /* update balance of all touched nodes */
  /* reference: <http://cmcrossroads.com/bradapp/ftp/src/libs/C++/AvlTrees.html> */

  B->balance++;
  A->balance = -(B->balance);

  return B;
}


static avlnode_t*
avltree_left_right_rotation(avlnode_t* A)
{
  /*
  >          A                       C
  >         / \                     / \
  >        /                       /   \
  >       B           -->>        B     A
  >      / \                     / \   / \
  >         \
  >          C
  */
  avlnode_t* B, *C;

  B = A->left;
  C = B->right;
  A->left = C->right;
  B->right = C->left;
  C->right = A;
  C->left = B;

  /* update balance of all touched nodes */
  /* reference: <http://cmcrossroads.com/bradapp/ftp/src/libs/C++/AvlTrees.html> */

  A->balance = (C->balance > 0) ? 0 : -(C->balance);
  B->balance = (C->balance < 0) ? 0 : -(C->balance);
  C->balance = 0;

  return C;
}


static avlnode_t*
avltree_right_left_rotation(avlnode_t* A)
{
  /*
  >          A                       C
  >         / \                     / \
  >            \                   /   \
  >             B      -->>       B     A
  >            / \               / \   / \
  >           /
  >          C
  */
  avlnode_t* B, *C;

  B = A->right;
  C = B->left;
  A->right = C->left;
  B->left = C->right;
  C->left = A;
  C->right = B;

  /* update balance of all touched nodes */
  /* reference: <http://cmcrossroads.com/bradapp/ftp/src/libs/C++/AvlTrees.html> */

  A->balance = (C->balance < 0) ? 0 : -(C->balance);
  B->balance = (C->balance > 0) ? 0 : -(C->balance);
  C->balance = 0;

  return C;
}


static int
avltree_insert_node(avltree_t* tree, avlnode_t** root, const char* key, const void* data)
{
  int changed = 0;
  int compare;
  avlnode_t* node = (*root);

  if (node == NULL) {
    (*root) = node = (avlnode_t*) xcalloc(1, sizeof(*node));
    if (tree->options & AVLTREE_STATIC_KEYS) {
      node->key = key;
    } else {
      node->key = xstrdup(key);
    }
    node->data = data;
    tree->count++;
    return 1;	/* anyway, our tree has been changed */
  }

  compare = tree->compare(key, node->key);

  if (compare < 0) {
    if (avltree_insert_node(tree, &node->left, key, data)) {
      changed = (--node->balance != 0);
      switch (node->balance) {
      case -2:
        if (node->left->balance < 0) {
          node = avltree_left_rotation(node);
        } else {
          node = avltree_left_right_rotation(node);
        }
        (*root) = node;
      case  0:
        changed = 0;
      case -1:
        break;
      default:
        /* should be impossible :-) */
        fatal(AVLTREE_INVALID_BALANCE);
      }
    } else {
      changed = 0;
    }
  } else if (compare > 0) {
    if (avltree_insert_node(tree, &node->right, key, data)) {
      changed = (++node->balance != 0);
      switch (node->balance) {
      case +2:
        if (node->right->balance > 0) {
          node = avltree_right_rotation(node);
        } else {
          node = avltree_right_left_rotation(node);
        }
        (*root) = node;
      case 0:
        changed = 0;
      case +1:
        break;
      default:
        /* should be impossible :-) */
        fatal(AVLTREE_INVALID_BALANCE);
      }
    } else {
      changed = 0;
    }
  } else {
    if (tree->options & AVLTREE_PARANOIAC)
      fatal("%s/%s.%d: Duplicate keys are not allowed (\"%s\")!\n",
            tree->module, MYNAME, __LINE__, key);
    changed = 0;
  }

  return changed;
}



static int
avltree_delete_node(avltree_t* tree, const char* key, avlnode_t** root, int* changed)
{
  avlnode_t* node = (*root);
  int deleted = 0;
  int compare;

  if (node == NULL) {
    if (tree->options & AVLTREE_PARANOIAC)
      fatal("%s/%s.%d: Key to delete \"%s\" not found!\n",
            tree->module, MYNAME, __LINE__, key);
    return 0;
  }

  compare = tree->compare(key, node->key);

  if (compare < 0) {
    deleted = avltree_delete_node(tree, key, &node->left, changed);
    if (*changed) {
      node->balance++;		/* shift weight to right */
      switch (node->balance) {
      case +1:
        (*changed) = 0;	/* stop rebalancing */
      case 0:
        break;
      case +2:
        if (node->right->balance >= 0) {
          node = avltree_right_rotation(node);
        } else {
          node = avltree_right_left_rotation(node);
        }
        (*root) = node;
        if (node->balance == -1) {
          (*changed) = 0;
        }
        break;
      default:
        /* should be impossible :-) */
        fatal(AVLTREE_INVALID_BALANCE);
      }
    }
  } else if (compare > 0) {
    deleted = avltree_delete_node(tree, key, &node->right, changed);
    if (*changed) {
      node->balance--;		/* shift weight to left */
      switch (node->balance) {
      case -1:
        (*changed) = 0;	/* stop rebalancing */
      case 0:
        break;
      case -2:
        if (node->left->balance <= 0) {
          node = avltree_left_rotation(node);
        } else {
          node = avltree_left_right_rotation(node);
        }
        (*root) = node;
        if (node->balance == +1) {
          (*changed) = 0;
        }
        break;
      default:
        /* should be impossible :-) */
        fatal(AVLTREE_INVALID_BALANCE);
      }
    }
  } else {
    if (node->left) {
      if (node->right) {
        const char* temp_key;
        const void* temp_data;
        avlnode_t* succ = node->right;

        while (succ->left) {
          succ = succ->left;  /* find successor */
        }

        temp_key = succ->key;			/* swap contents */
        temp_data = succ->data;
        succ->key = node->key;
        succ->data = node->data;
        node->key = temp_key;
        node->data = temp_data;

        deleted = avltree_delete_node(tree, key, &node->right, changed);

        if (*changed) {
          node->balance--;		/* shift weight to left */
          switch (node->balance) {
          case -1:
            (*changed) = 0;	/* stop rebalancing */
          case 0:
            break;
          case -2:
            if (node->left->balance <= 0) {
              node = avltree_left_rotation(node);
            } else {
              node = avltree_left_right_rotation(node);
            }
            (*root) = node;
            if (node->balance == +1) {
              (*changed) = 0;
            }
            break;
          default:
            /* should be impossible :-) */
            fatal(AVLTREE_INVALID_BALANCE);
          }
        }
        return deleted;
      } else {	/* only left branch */
        (*root) = node->left;
        node->left = NULL;
      }
    } else if (node->right) {	/* only right branch */
      (*root) = node->right;
      node->right = NULL;
    } else {	/* only a simple leaf */
      (*root) = NULL;
    }

    avltree_node_free(tree, node);
    tree->count--;
    (*changed) = 1;
    deleted = 1;
  }

  return deleted;
}


static avlnode_t*
avltree_dupe_node(const avltree_t* tree, const avlnode_t* node)
{
  avlnode_t* res = (avlnode_t*) xcalloc(1, sizeof(*res));

  if (tree->options & AVLTREE_STATIC_KEYS) {
    res->key = node->key;
  } else {
    res->key = xstrdup(node->key);
  }

  res->balance = node->balance;
  if (node->left) {
    res->left = avltree_dupe_node(tree, node->left);
  }
  if (node->right) {
    res->right = avltree_dupe_node(tree, node->right);
  }

  return res;
}


static int
avltree_strcmpr(const char* s1, const char* s2)
{
  return -(strcmp(s1, s2));
}


static int
avltree_case_ignore_strcmpr(const char* s1, const char* s2)
{
  return -(case_ignore_strcmp(s1, s2));
}


static avlnode_t*
avltree_find_next(const avltree_t* tree, avlnode_t* node, const char* key)
{
  avlnode_t* prev = NULL;

  if (key == NULL) {
    if ((node = tree->root)) {
      while (node->left) {
        node = node->left;
      }
    }
    return node;
  }

  while (node) {
    int compare = tree->compare(key, node->key);

    if (compare < 0) {
      prev = node;
      node = node->left;
    } else if (compare > 0) {
      node = node->right;
    } else {
      if ((node = node->right))
        while (node->left) {
          node = node->left;
        }
      else {
        node = prev;
      }
      return node;
    }
  }
  /* The previous node was deleted and we could not find it. */
  return prev;
}


/*
   Save [key] for a possible delete before next op. Now we have no problem with:

 	curr = NULL;
 	while ((curr = avtree_next(tree, curr, NULL))) {
		avltree_delete(tree, curr);
	}
 */
static void
avltree_save_key(avltree_t* tree, const char* key)
{
  if (tree->options & AVLTREE_STATIC_KEYS) {
    tree->key = key;
  } else {
    if (key == NULL) {
      if (tree->key_sz) {
        xfree((char*)tree->key);
        tree->key_sz = 0;
      }
      tree->key = NULL;
    } else {
      int n, n8;

      n = strlen(key) + 1;
      n8 = ((n + 7) / 8) * 8;

      if (n8 > tree->key_sz) {
        if (tree->key_sz == 0) {
          tree->key = (char*) xmalloc(n8);
        } else {
          tree->key = (char*) xrealloc((char*)tree->key, n8);
        }
        tree->key_sz = n8;
      }
      strncpy((char*)tree->key, key, n);
    }
  }
}
