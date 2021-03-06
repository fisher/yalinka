/*
 *     ynif_search.c
 *
 *     yalinka:search/3 implementation
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

#include <math.h>
#include <erl_nif.h>
#include "yalinka.h"
#include "kdtree.h"
#include "lib_funs.h"

#include <stdio.h>


ERL_NIF_TERM search_nearest(ErlNifEnv*, const ERL_NIF_TERM*, uint64_t, int);

ERL_NIF_TERM search2_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    if (argc !=2) return enif_make_badarg(env);

    return search_nearest( env, argv, 1, 0 );
}

ERL_NIF_TERM search3_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    uint64_t howmuch;

    if (argc !=3) return enif_make_badarg(env);

    if (enif_is_atom(env, argv[2])) return search_nearest(env, argv, 1, 1);

    if (!enif_get_uint64(env, argv[2], (ErlNifUInt64*) &howmuch))
        return enif_make_badarg(env);

    if (howmuch != 1)
        return not_implemented(env);

    return search_nearest( env, argv, howmuch, 0 );
}

/* ********************************************************************** */

/*
 * spec search(reference(), {float(), float(), float()}, integer()) ->
 *                     [{integer(), float()}].
 */
ERL_NIF_TERM search_nearest(ErlNifEnv *env,
                            const ERL_NIF_TERM *argv,
                            uint64_t howmuch, int debug)
{
    /* pointer to the tree */
    KD_TREE_T *tree;

    /* point taken from the args */
    NODE_3D_T point;
    NODE_KD_T point_kd;

    /* temp placeholder for tuple and its arity */
    const ERL_NIF_TERM *tuple;
    int tuple_arity;

    node_3d_ptr found;
    node_kd_ptr found_kd;
    double best_dist;
    int seen;
    uint64_t idx;

    /* placeholder for the list of what we're found */
    ERL_NIF_TERM *list;

    /* placeholder for resulting term */
    ERL_NIF_TERM result;

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return error2(env, "invalid_reference", enif_make_copy(env, argv[0]));

    if (!tree->ready) return error1(env, "not_ready");

    if (!enif_get_tuple(env, argv[1], &tuple_arity, &tuple))
        return error2(env, "tuple_expected", enif_make_copy(env, argv[1]));

    if (tree->dimension != (uint64_t) tuple_arity)
        return error4 ( env, "invalid_dimension",
                        enif_make_uint64(env, tree->dimension),
                        enif_make_int(env, tuple_arity),
                        enif_make_copy(env, argv[1]));

    if (tree->size < 1)
        return error1( env, "empty_tree" );

    if (tree->dimension <= MAX_DIM) {

        for (int i=0; i<tuple_arity; i++) {
            if (!enif_get_double(env, tuple[i], &point.x[i]))
                return enif_make_badarg(env);
        }
    } else {
        point_kd.x = enif_alloc(sizeof(double) *tree->dimension);

        for (int i=0; i<tuple_arity; i++) {
            if (!enif_get_double(env, tuple[i], &point_kd.x[i])) {
                enif_free(point_kd.x);
                return enif_make_badarg(env);

            }
        }
    }

    list = (ERL_NIF_TERM *) enif_alloc( sizeof(ERL_NIF_TERM) * howmuch );

     /* meat here */

    found = 0;
    found_kd = 0;


    if (tree->dimension <= MAX_DIM) {
#ifdef DEBUG
        printf("searching for (%g, %g, %g)\r\n",
               point.x[0], point.x[1], point.x[2]);

        seen =
            nearest(tree->root.node_3d, &point, 0, tree->dimension, &found, &best_dist, 0);

        printf("search done.\r\nfor (%g, %g, %g) we've "
               "found (%g, %g, %g)\r\nidx %lu dist %g\r\nseen %d nodes\r\n\n",
               point.x[0], point.x[1], point.x[2],
               found->x[0], found->x[1], found->x[2], found->idx,
               sqrt(best_dist), seen);
#else
        seen = nearest ( tree->root.node_3d, &point, 0, tree->dimension, &found, &best_dist, 0);
#endif
        idx = found->idx;

    } else {

        seen = nearest_kd(tree->root.node_kd, &point_kd, 0,
                          tree->dimension, &found_kd, &best_dist, 0);
        idx = found_kd->idx;
    }

    list[0] = enif_make_tuple2( env,
                                enif_make_uint64(env, idx),
                                enif_make_double(env, sqrt(best_dist)));

    if (debug) {
        result = enif_make_tuple3(
            env, try_make_existing_atom(env, "ok"),
            enif_make_list_from_array(env, list, howmuch),
            enif_make_int(env, seen));
    } else {
        result = enif_make_tuple2(
            env,
            try_make_existing_atom(env, "ok"),
            enif_make_list_from_array(env, list, howmuch));
    }

    enif_free(list);

    return result;
}

/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
