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

/*
 * spec search(reference(), {float(), float(), float()}, integer()) ->
 *                     [{integer(), float()}].
 */
ERL_NIF_TERM search_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    /* pointer to the tree */
    KD_TREE_T *tree;

    /* point taken from the args */
    KD_NODE_T point;

    /* how much nearest points we should return */
    uint64_t howmuch;

    /* temp placeholder for tuple and its arity */
    const ERL_NIF_TERM *tuple;
    int tuple_arity;

    node_ptr found;
    double best_dist;

    /* placeholder for resulting term */
    ERL_NIF_TERM result;

    /* codeseg */
    if (argc != 3) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return enif_make_badarg(env);

    if (!enif_is_tuple(env, argv[1])) return enif_make_badarg(env);

    if (!enif_is_number(env, argv[2])) return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &tuple_arity, &tuple) ||
        tuple_arity != 3) return enif_make_badarg(env);

    if (!enif_get_uint64(env, argv[2], (ErlNifUInt64*) &howmuch))
        return enif_make_badarg(env);

    if (tree->size < 1)
        return enif_make_tuple2(
            env,
            try_make_existing_atom(env, "error"),
            try_make_existing_atom(env, "empty_tree"));

    for (int i=0; i<tuple_arity; i++) {
        if (!enif_get_double(env, tuple[i], &point.x[i]))
            return enif_make_badarg(env);
    }

    /* meat here */

	printf("searching for (%g, %g, %g)\r\n",
           point.x[0], point.x[1], point.x[2]);

    found = 0;
    int visited = nearest( tree->root, &point, 0, tree->dimension, &found, &best_dist, 0);

	printf("search done.\r\nfor (%g, %g, %g) we've "
           "found (%g, %g, %g)\r\nidx %lu dist %g\r\nseen %d nodes\r\n\n",
           point.x[0], point.x[1], point.x[2],
           found->x[0], found->x[1], found->x[2], found->idx,
           sqrt(best_dist), visited);

    printf("ok\r\n");

    result = enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_list1(
            env,
            enif_make_tuple2 (
                env,
                enif_make_uint64(env, found->idx),
                enif_make_double(env, sqrt(best_dist)))));

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
