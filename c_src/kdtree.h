#ifndef KDTREE_H
#define KDTREE_H

#include <inttypes.h>

#define MAX_DIM 3

typedef struct kd_node_t KD_NODE_T;
typedef KD_NODE_T * node_ptr;
typedef struct kd_tree_t KD_TREE_T;

struct kd_tree_t {
    uint64_t size;
    node_ptr array;
};

struct kd_node_t {
    double x[MAX_DIM];
    uint64_t idx;
    node_ptr left;
    node_ptr right;
};

#ifndef KDTREE_C

extern node_ptr find_median(node_ptr start, node_ptr end, int idx);

extern node_ptr make_tree(struct kd_node_t *t, int len, int i, int dim);

extern int nearest( node_ptr root, node_ptr nd, int i, int dim,
             KD_NODE_T **best, double *best_dist, int counter );

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
