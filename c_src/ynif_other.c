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


ERL_NIF_TERM clear_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    /* for now our resource objects are GC'ed automatically */

    printf("clear_nif, tree size: %" PRIu64 ".\r\n", tree->size);

    enif_release_resource(tree);

    return try_make_existing_atom(env, "ok");
}


/* this is just a quick and dirty hack. It allocates new memory,
   copyes previos data to newly allocated memory, then adds new data
   at the end of existing data and calls reindex function. Yes, it
   will work, but it is inefficient and needs to be remade. */
ERL_NIF_TERM insert_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM list_ptr, head, tail, result;
    unsigned int list_size;

    /* temp placeholder for an array */
    /* TODO: not to allocate new mem & copy existing tree array but
       realloc the needed amount. The problem is that there is no
       enif_realloc function, only enif_alloc */
    node_3d_ptr array;
    node_kd_ptr kd_array;
    double *pts;

    /* placeholder for the outer tuple of size 2 */
    const ERL_NIF_TERM *tuple;
    int tuple_arity;

    /* placeholder for the inner tuple with point */
    const ERL_NIF_TERM *point_tuple;
    int point_dim;

    int i;

    if (argc != 2) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    if (!enif_get_list_length(env, argv[1], &list_size))
        return error2(env, "list_expected", enif_make_copy(env, argv[1]));

    if (tree->dimension <= MAX_DIM) {
        array = enif_alloc(sizeof(NODE_3D_T) * (tree->size + list_size));
        memcpy(array, tree->array.node_3d, sizeof(NODE_3D_T) * tree->size);
    } else {
        kd_array = enif_alloc(sizeof(NODE_KD_T) * (tree->size +list_size));
        memcpy(kd_array, tree->array.node_kd, sizeof(NODE_KD_T) * tree->size);
    }

    i = tree->size;
    list_ptr = argv[1];

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_get_tuple(env, head, &tuple_arity, &tuple)
            || tuple_arity != 2) {
            ENIF_FREE(array, kd_array);
            return error2(env, "expected_tuple2", enif_make_copy(env, head));
        }

        if(tree->dimension <= MAX_DIM) {
            if ((result = fill_node_tag(env, tuple[0], &array[i].idx))) {
                enif_free(array);
                return result;
            }
        } else {
            if ((result = fill_node_tag(env, tuple[0], &kd_array[i].idx))) {
                enif_free(kd_array);
                return result;
            }

            pts = enif_alloc(sizeof(double) * tree->dimension);
            kd_array[i].x = pts;
        }

        if (!enif_get_tuple(env, tuple[1], &point_dim, &point_tuple)
            || (unsigned int) point_dim != tree->dimension) {
            ENIF_FREE(array, kd_array);
            return error4(env, "invalid_dimension_in_data",
                          enif_make_uint64(env, tree->dimension),
                          enif_make_int(env, point_dim),
                          enif_make_copy(env, tuple[1]));
        }

        for (int j=0; j<point_dim; j++) {
            double fp;

            if (!enif_get_double(env, point_tuple[j], (double*) &fp)) {
                ENIF_FREE(array, kd_array);
                return error4(env, "invalid_node_spec",
                              try_make_existing_atom(env, "float"),
                              enif_make_copy(env, point_tuple[j]),
                              enif_make_copy(env, head));
            }

            if (tree->dimension <= MAX_DIM) {
                array[i].x[j] = fp;
            } else {
                kd_array[i].x[j] = fp;
            }
        }

        list_ptr = tail;
        i++;
    }

    /* at this point we have constructed array from previous data
       concatenated with newly added data. Thus, we clear old
       memory. Only reindex the new and then swap it up */

    if (tree->dimension <= MAX_DIM) {

        tree->root.node_3d = make_tree_3d
            ( array, tree->size + list_size, 0, tree->dimension );

        enif_free(tree->array.node_3d);

        tree->array.node_3d = array;

    } else {

        tree->root.node_kd = make_tree_kd
            ( kd_array, tree->size + list_size, 0, tree->dimension );

        enif_free(tree->array.node_kd);

        tree->array.node_kd = kd_array;
    }

    tree->size += list_size;
    tree->ready = 1;

    return try_make_existing_atom(env, "ok");
}


/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
