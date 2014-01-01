#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>
#include <time.h>

#define KDTREE_C
#include "kdtree.h"

inline static double dist(node_ptr a, node_ptr b, int dim) {
  double t, d = 0;
  while (dim--) {
    t = a->x[dim] - b->x[dim];
    d += t * t;
  }
  return d;
}

inline static void swap(node_ptr x, node_ptr y) {
  double tmp[MAX_DIM];
  memcpy(tmp,  x->x, sizeof(tmp));
  memcpy(x->x, y->x, sizeof(tmp));
  memcpy(y->x, tmp,  sizeof(tmp));
}

/* see quickselect method */
node_ptr find_median(node_ptr start, node_ptr end, int idx) {
  node_ptr p;
  node_ptr store;
  node_ptr md;

  double pivot;

  if (end <= start) return NULL;
  if (end == start + 1)
    return start;

  md = start + (end - start) /2;

  while (1) {
    pivot = md->x[idx];

    swap(md, end - 1);
    for (store = p = start; p < end; p++) {
      if (p->x[idx] < pivot) {
        if (p != store)
          swap(p, store);
        store++;
      }
    }
    swap(store, end - 1);

    /* median has duplicate values */
    if (store->x[idx] == md->x[idx])
      return md;

    if (store > md)  end   = store;
    else             start = store;
  }
}


node_ptr make_tree(struct kd_node_t *t, int len, int i, int dim) {
  node_ptr n;

  if (!len) return 0;

  if ((n = find_median(t, t + len, i))) {
    i = (i + 1) % dim;
    n->left  = make_tree(t, n - t, i, dim);
    n->right = make_tree(n + 1, t + len - (n + 1), i, dim);
  }
  return n;
}


int nearest( node_ptr root, node_ptr nd, int i, int dim,
             KD_NODE_T **best, double *best_dist, int counter ) {

  double d, dx, dx2;
  int visited = counter;

  if (!root) return visited;
  d = dist(root, nd, dim);
  dx = root->x[i] - nd->x[i];
  dx2 = dx * dx;

  visited ++;

  if (!*best || d < *best_dist) {
    *best_dist = d;
    *best = root;
  }

  /* if chance of exact match is high */
  if (!*best_dist) return visited;

  if (++i >= dim) i = 0;

  visited = nearest(dx > 0 ? root->left : root->right, nd, i, dim, best, best_dist, visited);
  if (dx2 >= *best_dist) return visited;
  visited = nearest(dx > 0 ? root->right : root->left, nd, i, dim, best, best_dist, visited);

  return visited;
}



/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
