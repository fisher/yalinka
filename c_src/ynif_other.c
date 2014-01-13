/*
 *     ynif_other.c
 *
 *     various auxiliary erlang functions implementation
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
#include <string.h>
#include <erl_nif.h>
#include "yalinka.h"
#include "lib_funs.h"
#include "kdtree.h"

ERL_NIF_TERM size_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{

    ERL_NIF_TERM result;

    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **)&tree))
       return enif_make_badarg(env);

    result = enif_make_tuple2( env,
                               try_make_existing_atom(env, "ok"),
                               enif_make_int(env, tree->size));
    return result;
}

ERL_NIF_TERM clear_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return enif_make_badarg(env);


    /* for now our resource objects are GC'ed automatically */

    printf("clear_nif, tree size: %" PRIu64 ".\r\n", tree->size);

    enif_release_resource(tree);

    return try_make_existing_atom(env, "ok");
}

ERL_NIF_TERM dimension_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return enif_make_badarg(env);

    return enif_make_tuple2( env,
                             try_make_existing_atom(env, "ok"),
                             enif_make_int(env, tree->dimension));
}

ERL_NIF_TERM root_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM *point;

    ERL_NIF_TERM result;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **)&tree))
        return enif_make_badarg(env);

    if (tree->size <1) return error1 (env, "empty_tree");

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    for (unsigned int i = 0; i<tree->dimension; i++) {
        point[i] = enif_make_double(env, tree->root->x[i]);
    }

    result = enif_make_tuple3(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_uint64(env, tree->root->idx),
        enif_make_tuple_from_array(env, point, tree->dimension));

    enif_free(point);

    return result;
}

ERL_NIF_TERM node_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    uint64_t idx;

    ERL_NIF_TERM *point;

    ERL_NIF_TERM result;

    if (argc != 2) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return enif_make_badarg(env);

    if (!enif_get_uint64(env, argv[1], &idx)) return enif_make_badarg(env);

    if (tree->size < idx) return error2 (env, "index_out_of_range", idx);

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    for (unsigned int i = 0; i<tree->dimension; i++) {

#ifdef DEBUG
        printf("double [%d] = %g\r\n", i, tree->array[idx].x[i]);
#endif
        point[i] = enif_make_double(env, tree->array[idx].x[i]);
    }

    result = enif_make_tuple3(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_uint64(env, tree->array[idx].idx),
        enif_make_tuple_from_array(env, point, tree->dimension));

    enif_free(point);

    return result;
}


ERL_NIF_TERM gettree_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM *list, *point;

    ERL_NIF_TERM result;

    if (argc !=1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return error2(env, "invalid_reference", enif_make_copy(env, argv[0]));

    list = (ERL_NIF_TERM *) enif_alloc( sizeof(ERL_NIF_TERM) * tree->size );

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    for (unsigned int i=0; i<tree->size; i++) {

        for (unsigned int j=0; j<tree->dimension; j++) {
            point[j] = enif_make_double(env, tree->array[i].x[j]);
        }

        list[i] = enif_make_tuple2(env,
                                   enif_make_uint64(env, tree->array[i].idx),
                                   enif_make_tuple_from_array(env, point, tree->dimension));

    }

    result = enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_list_from_array(env, list, tree->size));

    enif_free(point);
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
