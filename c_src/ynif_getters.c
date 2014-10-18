/*
 *     ynif_getters.c
 *
 *     simple getters for existing object, all they are read-only
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
        return invalid_ref(env, argv[0]);

    result = enif_make_tuple2( env,
                               try_make_existing_atom(env, "ok"),
                               enif_make_int(env, tree->size));
    return result;
}

ERL_NIF_TERM dimension_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    return enif_make_tuple2( env,
                             try_make_existing_atom(env, "ok"),
                             enif_make_int(env, tree->dimension));
}

ERL_NIF_TERM is_ready_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    if (tree->ready) return try_make_existing_atom(env, "true");
    else return try_make_existing_atom(env, "false");
}

ERL_NIF_TERM compare_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree1, *tree2;

    if (argc != 2) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree1))
        return invalid_ref(env, argv[0]);

    if (!enif_get_resource(env, argv[1], KDTREE_RESOURCE, (void **) &tree2))
        return error2(env, "invalid_reference", enif_make_copy(env, argv[1]));

    if (tree1->size != tree2->size) return try_make_existing_atom(env, "diff");

    if (tree1->dimension != tree2->dimension) return try_make_existing_atom(env, "diff");

    if (tree1->ready != tree2->ready) return try_make_existing_atom(env, "diff");

    /* can't use memcmp because of pointers, need to traverse through */
    if (tree1->dimension <= MAX_DIM) {
        for (unsigned int i=0; i<tree1->size; i++) {
            if ( tree1->array.node_3d[i].idx != tree2->array.node_3d[i].idx )
                return try_make_existing_atom(env, "diff");
            for (unsigned int j=0; j<tree1->dimension; j++) {
                if ( tree1->array.node_3d[i].x[j] != tree2->array.node_3d[i].x[j] )
                    return try_make_existing_atom(env, "diff");
            }
        }
    } else {
        for (unsigned int i=0; i<tree1->size; i++) {
            if (tree1->array.node_kd[i].idx != tree2->array.node_kd[i].idx)
                return try_make_existing_atom(env, "diff");
            for (unsigned int j=0; j<tree1->dimension; j++) {
                if (tree1->array.node_kd[i].x[j] != tree2->array.node_kd[i].x[j] )
                    return try_make_existing_atom(env, "diff");
            }
        }
    }

    return try_make_existing_atom(env, "equal");

}

ERL_NIF_TERM gettree_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM *list, *point;

    ERL_NIF_TERM result;

    if (argc !=1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    list = (ERL_NIF_TERM *) enif_alloc( sizeof(ERL_NIF_TERM) * tree->size );

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    if (tree->dimension <= MAX_DIM) {
        for (unsigned int i=0; i<tree->size; i++) {

            for (unsigned int j=0; j<tree->dimension; j++)
                point[j] = enif_make_double(env, tree->array.node_3d[i].x[j]);

            list[i] = enif_make_tuple2
                ( env,
                  enif_make_uint64(env, tree->array.node_3d[i].idx),
                  enif_make_tuple_from_array(env, point, tree->dimension) );

        }
    } else {
        for (unsigned int i=0; i<tree->size; i++) {

            for (unsigned int j=0; j<tree->dimension; j++)
                point[j] = enif_make_double(env, tree->array.node_kd[i].x[j] );

            list[i] = enif_make_tuple2
                ( env,
                  enif_make_uint64 (env, tree->array.node_kd[i].idx),
                  enif_make_tuple_from_array (env, point, tree->dimension) );

        }
    }

    result = enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_list_from_array(env, list, tree->size));

    enif_free(point);
    enif_free(list);

    return result;
}

ERL_NIF_TERM root_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM *point;

    ERL_NIF_TERM result;

    uint64_t i;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **)&tree))
        return invalid_ref(env, argv[0]);

    if (tree->size <1) return error1 (env, "empty_tree");

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    if (tree->dimension <= MAX_DIM) {

        for (i = 0; i<tree->dimension; i++) {
            point[i] = enif_make_double(env, tree->root.node_3d->x[i]);
        }

        i = tree->root.node_3d->idx;

    } else {
        for (i=0; i<tree->dimension; i++)
            point[i] = enif_make_double(env, tree->root.node_kd->x[i]);
        i = tree->root.node_kd->idx;
    }

    result = enif_make_tuple3(
                              env,
                              try_make_existing_atom(env, "ok"),
                              enif_make_uint64(env, i),
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
        return invalid_ref(env, argv[0]);

    if (!enif_get_uint64(env, argv[1], &idx)) return enif_make_badarg(env);

    if (! (tree->size > idx))
        return error2 (env, "index_out_of_range", enif_make_uint64(env, idx));

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    if ( NULL == point ) return error1 (env, "memory_allocation_error");

    if (tree->dimension <= MAX_DIM) {

        for (unsigned int i = 0; i<tree->dimension; i++)
            point[i] = enif_make_double(env, tree->array.node_3d[idx].x[i]);

        idx = tree->array.node_3d[idx].idx;

    } else {
        for (unsigned int i=0; i<tree->dimension; i++)
            point[i] = enif_make_double(env, tree->array.node_kd[idx].x[i]);
        idx = tree->array.node_kd[idx].idx;
    }

    result = enif_make_tuple3(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_uint64(env, idx),
        enif_make_tuple_from_array(env, point, tree->dimension));

    enif_free(point);

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
