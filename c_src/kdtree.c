/*
 *     kdtree.c
 *
 *     k-d tree algorythms module
 *
 *     this file is part of 'yalinka' project, http://yalinka.heim.in.ua
 *
 *     Erlang NIF interface to K-dimensional trees
 *
 *     Copyright 2013 Serge A. Ribalchenko <fisher@heim.in.ua>
 *
 *     Redistribution and use in source and binary forms, with or without
 *     modification, are permitted provided that the following conditions are
 *     met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials
 *       provided with the distribution.
 *
 *     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <math.h>
#include <time.h>

#define KDTREE_C
#include "kdtree.h"


/* swap the payload of two nodes */
inline static void swap(node_3d_ptr x, node_3d_ptr y)
{
    double tmp[MAX_DIM];
    uint64_t idx;

    memcpy(tmp,  x->x, sizeof(tmp));
    memcpy(x->x, y->x, sizeof(tmp));
    memcpy(y->x, tmp,  sizeof(tmp));

    idx = x->idx;
    x->idx = y->idx;
    y->idx = idx;
}

inline static void swap_kd(node_kd_ptr x, node_kd_ptr y)
{
    double  *tmpx;
    uint64_t tmpidx;

    tmpidx = x->idx;
    x->idx = y->idx;
    y->idx = tmpidx;

    tmpx = x->x;
    x->x = y->x;
    y->x = tmpx;
}

/* see quickselect method */
node_3d_ptr find_median(node_3d_ptr start, node_3d_ptr end, int idx)
{
    node_3d_ptr p;
    node_3d_ptr store;
    node_3d_ptr md;

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

node_kd_ptr find_median_kd(node_kd_ptr start, node_kd_ptr end, int idx)
{
    node_kd_ptr p;
    node_kd_ptr store;
    node_kd_ptr md;

    double pivot;

    if (end <= start) return NULL;
    if (end == start +1)
        return start;

    md = start + (end -start) /2;

    while (1) {
        pivot = md->x[idx];

        swap_kd(md, end -1);
        for (store = p = start; p <end; p++) {
            if (p->x[idx] <pivot) {
                if (p != store)
                    swap_kd(p, store);
                store++;
            }
        }
        swap_kd(store, end -1);

        if (store->x[idx] == md->x[idx])
            return md;

        if (store > md)  end = store;
        else           start = store;
    }
}

/*
 * array : ptr to the array of nodes
 * len   : the number of nodes in array
 * i     : should be set to 0 on initial call
 * dim   : space dimension (arity of the point in space)
 *
 * returns pointer to the root of given tree
 */
node_3d_ptr make_tree_3d(node_3d_ptr array, int len, int i, int dim)
{
    node_3d_ptr n;

    if (!len) return 0;

    if ( (n = find_median(array, array + len, i)) ) {
        i = (i + 1) % dim;
        n->left  = make_tree_3d(array, n - array, i, dim);
        n->right = make_tree_3d(n + 1, array + len - (n + 1), i, dim);
    }

    return n;
}

node_kd_ptr make_tree_kd(node_kd_ptr array, int len, int i, int dim)
{
    node_kd_ptr n;

    if (!len) return 0;
    if ( (n = find_median_kd(array, array +len, i)) ) {
        i = (i +1) % dim;
        n->left  = make_tree_kd(array, n -array, i, dim);
        n->right = make_tree_kd(n +1, array +len -(n +1), i, dim);
    }

    return n;
}


inline static double dist(node_3d_ptr a, node_3d_ptr b, int dim)
{
    double t, d = 0;
    while (dim--) {
        t = a->x[dim] - b->x[dim];
        d += t * t;
    }
    return d;
}

inline static double dist_kd(node_kd_ptr a, node_kd_ptr b, int dim)
{
    double t, d = 0;
    while (dim--) {
        t = a->x[dim] - b->x[dim];
        d += t * t;
    }
    return d;
}

/*
 * root      : ptr to the root of the tree
 * point     : point to look for
 * i         : index. should be set to zero on initial call
 * dim       : dimension, arity of the points
 * best      : here we'll put down what we'll found
 * best_dist : here we'll put down the best distance, squared
 * counter   : just set it to 0 when call this function
 *
 * returns the number of visited nodes
 */
int nearest( node_3d_ptr root, node_3d_ptr point, int i, int dim,
             node_3d_ptr *best, double *best_dist, int counter ) {

  double d, dx, dx2;
  int visited = counter;

  if (!root) return visited;
  d = dist(root, point, dim);
  dx = root->x[i] - point->x[i];
  dx2 = dx * dx;

  visited ++;

  if (!*best || d < *best_dist) {
    *best_dist = d;
    *best = root;
  }

  /* if chance of exact match is high */
  if (!*best_dist) return visited;

  if (++i >= dim) i = 0;

  visited = nearest(dx > 0 ? root->left : root->right, point, i, dim, best, best_dist, visited);
  if (dx2 >= *best_dist) return visited;
  visited = nearest(dx > 0 ? root->right : root->left, point, i, dim, best, best_dist, visited);

  return visited;
}

int nearest_kd( node_kd_ptr root, node_kd_ptr point, int i, int dim,
                node_kd_ptr *best, double *best_dist, int counter ) {

  double d, dx, dx2;
  int visited = counter;

  if (!root) return visited;
  d = dist_kd(root, point, dim);
  dx = root->x[i] - point->x[i];
  dx2 = dx * dx;

  visited ++;

  if (!*best || d < *best_dist) {
    *best_dist = d;
    *best = root;
  }

  /* if chance of exact match is high */
  if (!*best_dist) return visited;

  if (++i >= dim) i = 0;

  visited = nearest_kd(dx > 0 ? root->left : root->right, point, i, dim, best, best_dist, visited);
  if (dx2 >= *best_dist) return visited;
  visited = nearest_kd(dx > 0 ? root->right : root->left, point, i, dim, best, best_dist, visited);

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
