/*
 *     kdtree.h
 *
 *     header file for k-d tree algorythms module
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

#ifndef KDTREE_H
#define KDTREE_H

#include <inttypes.h>

#define MAX_DIM 3

typedef struct node_3d_t NODE_3D_T;
typedef struct node_kd_t NODE_KD_T;

typedef union real_node {
    NODE_3D_T *node_3d;
    NODE_KD_T *node_kd;
} uni_node;

typedef NODE_3D_T * node_3d_ptr;
typedef NODE_KD_T * node_kd_ptr;

typedef struct kd_tree_t KD_TREE_T;

struct kd_tree_t {
    uint64_t size;         /* quantity of points/nodes in the array, space size */
    uint64_t dimension;    /* arity of the point in the space, space dimension */
    int      ready;        /* readiness flag, set to 1 if tree is indexed */
    uni_node root;         /* pointer to the root of the tree */
    uni_node array;        /* pointer to the array of nodes */
    double  *pts;          /* pointer to the array of points for dimensions >3 */
};

/* optimised for 2d & 3d */
struct node_3d_t {
    double      x[MAX_DIM];
    uint64_t    idx;
    node_3d_ptr left;
    node_3d_ptr right;
};

/* general case */
struct node_kd_t {
    double     *x;
    uint64_t    idx;
    node_kd_ptr left;
    node_kd_ptr right;
};

#ifndef KDTREE_C

/* extern uni_node make_tree( uni_node t, int len, int i, int dim); */

extern node_3d_ptr make_tree_3d( node_3d_ptr t, int len, int i, int dim);

extern int nearest( node_3d_ptr root, node_3d_ptr nd, int i, int dim,
                    node_3d_ptr *best, double *best_dist, int counter );

#endif

#endif

/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
