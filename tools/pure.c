#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define MAX_DIM 3

typedef struct kd_node_t KD_NODE_T;
typedef KD_NODE_T * node_ptr;

struct kd_node_t {
    double x[MAX_DIM];
    node_ptr left;
    node_ptr right;
};

inline double dist(node_ptr a, node_ptr b, int dim) {
	double t, d = 0;
	while (dim--) {
		t = a->x[dim] - b->x[dim];
		d += t * t;
	}
	return d;
}

inline void swap(node_ptr x, node_ptr y) {
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

		if (store > md)	end = store;
		else		start = store;
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


/* int visited; */

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

#define N 1000000
#define rand1()	(rand() / (double)RAND_MAX)
#define rand_pt(v) { v.x[0] = rand1(); v.x[1] = rand1(); v.x[2] = rand1(); }
int main(void) {
    int i, sum, test_runs, visited;
    KD_NODE_T wp[] = {
        {{2, 3}, NULL, NULL}, {{5, 4}, NULL, NULL}, {{9, 6}, NULL, NULL},
        {{4, 7}, NULL, NULL}, {{8, 1}, NULL, NULL}, {{7, 2}, NULL, NULL}
    };
	KD_NODE_T this = {{9, 2}, NULL, NULL};
	struct kd_node_t *root, *found, *million;
	double best_dist;

	root = make_tree(wp, sizeof(wp) / sizeof(wp[1]), 0, 2);

	/* visited = 0; */
	found = 0;
	visited = nearest(root, &this, 0, 2, &found, &best_dist, 0);

	printf(">> WP tree\nsearching for (%g, %g)\n"
		"found (%g, %g) dist %g\nseen %d nodes\n\n",
		this.x[0], this.x[1],
		found->x[0], found->x[1], sqrt(best_dist), visited);

	million = calloc(N, sizeof(KD_NODE_T));
	srand(time(0));
	for (i = 0; i < N; i++) rand_pt(million[i]);

	root = make_tree(million, N, 0, 3);
	rand_pt(this);

	found = 0;
	visited = nearest(root, &this, 0, 3, &found, &best_dist, 0);

	printf(">> Million tree\nsearching for (%g, %g, %g)\n"
		"found (%g, %g, %g) dist %g\nseen %d nodes\n",
		this.x[0], this.x[1], this.x[2],
		found->x[0], found->x[1], found->x[2],
		sqrt(best_dist), visited);

	/* search many random points in million tree to see average behavior.
	   tree size vs avg nodes visited:
	   	10		~  7
	   	100		~ 16.5
		1000		~ 25.5
		10000		~ 32.8
		100000		~ 38.3
		1000000		~ 42.6
		10000000	~ 46.7				*/

	sum = 0;
    test_runs = 100000;

	for (i = 0; i < test_runs; i++) {
		found = 0;
		rand_pt(this);
		visited = nearest(root, &this, 0, 3, &found, &best_dist, 0);
		sum += visited;
	}
	printf("\n>> Million tree\n"
		"visited %d nodes for %d random findings (%f per lookup)\n",
		sum, test_runs, sum/(double)test_runs);

	free(million);

	return 0;
}
