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
#include <errno.h>
#include <erl_nif.h>
#include "yalinka.h"
#include "lib_funs.h"
#include "kdtree.h"


#define FMARK "yalinka1"

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

ERL_NIF_TERM is_ready_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    if (tree->ready) return try_make_existing_atom(env, "true");
    else return try_make_existing_atom(env, "false");
}

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

ERL_NIF_TERM root_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM *point;

    ERL_NIF_TERM result;

    if (argc != 1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **)&tree))
        return invalid_ref(env, argv[0]);

    if (tree->size <1) return error1 (env, "empty_tree");

    if (tree->dimension <= MAX_DIM) {
        point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

        for (unsigned int i = 0; i<tree->dimension; i++) {
            point[i] = enif_make_double(env, tree->root.node_3d->x[i]);
        }

        result = enif_make_tuple3(
                                  env,
                                  try_make_existing_atom(env, "ok"),
                                  enif_make_uint64(env, tree->root.node_3d->idx),
                                  enif_make_tuple_from_array(env, point, tree->dimension));

        enif_free(point);
    } else {
        result = enif_make_tuple3(
                                  env,
                                  try_make_existing_atom(env, "ok"),
                                  enif_make_uint64(env, tree->root.node_kd->idx),
                                  enif_make_tuple_from_array(env, tree->array.node_kd->x, tree->dimension));
    }

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

    if (tree->size < idx) return error2 (env, "index_out_of_range", idx);

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    for (unsigned int i = 0; i<tree->dimension; i++) {

#ifdef DEBUG
        printf("double [%d] = %g\r\n", i, tree->array.node_3d[idx].x[i]);
#endif
        point[i] = enif_make_double(env, tree->array.node_3d[idx].x[i]);
    }

    result = enif_make_tuple3(
        env,
        try_make_existing_atom(env, "ok"),
        enif_make_uint64(env, tree->array.node_3d[idx].idx),
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
        return invalid_ref(env, argv[0]);

    list = (ERL_NIF_TERM *) enif_alloc( sizeof(ERL_NIF_TERM) * tree->size );

    point = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * tree->dimension);

    if (tree->dimension <= MAX_DIM) {
        for (unsigned int i=0; i<tree->size; i++) {

            for (unsigned int j=0; j<tree->dimension; j++) {
                point[j] = enif_make_double(env, tree->array.node_3d[i].x[j]);
            }

            list[i] = enif_make_tuple2(env,
                                       enif_make_uint64(env, tree->array.node_3d[i].idx),
                                       enif_make_tuple_from_array(env, point, tree->dimension));

        }
    } else {
        for (unsigned int i=0; i<tree->size; i++) {

            for (unsigned int j=0; j<tree->dimension; j++) {
                point[j] = enif_make_double(env,
                                            /* should be: */
                                            /*tree->array[i]->x[j]*/
                                            tree->array.node_3d[i].x[j]
                                            );
            }

            list[i] = enif_make_tuple2(env,
                                       enif_make_uint64(env, tree->array.node_3d[i].idx),
                                       enif_make_tuple_from_array(env, point, tree->dimension));

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

/* this is just a quick and dirty hack. It allocates new memory,
   copyes previos data to newly allocated memory, then adds new data
   at the end of existing data and calls reindex function. Yes, it
   will work, but it is inefficient and needs redo. */
ERL_NIF_TERM insert_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    ERL_NIF_TERM list_ptr, head, tail, result;
    unsigned int list_size;

    /* temp placeholder for array */
    /* TODO: not to allocate new mem & copy existing tree array but
       realloc the needed amount. The problem is that there is no
       enif_realloc function, only enif_alloc */
    node_3d_ptr array;

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
        return not_implemented(env);
    }

    i = tree->size;
    list_ptr = argv[1];

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_get_tuple(env, head, &tuple_arity, &tuple)
            || tuple_arity != 2) {
            enif_free(array);
            return error2(env, "expected_tuple2", enif_make_copy(env, head));
        }

        if ((result = fill_node_tag(env, tuple[0], &array[i].idx))) {
            enif_free(array);
            return result;
        }

        if (!enif_get_tuple(env, tuple[1], &point_dim, &point_tuple)
            || (unsigned int) point_dim != tree->dimension) {
            enif_free(array);
            return error4(env, "invalid_dimension_in_data",
                          enif_make_uint64(env, tree->dimension),
                          enif_make_int(env, point_dim),
                          enif_make_copy(env, tuple[1]));
        }

        for (int j=0; j<point_dim; j++) {
            double fp;

            if (!enif_get_double(env, point_tuple[j], (double*) &fp)) {
                enif_free(array);
                return error4(env, "invalid_node_spec",
                              try_make_existing_atom(env, "float"),
                              enif_make_copy(env, point_tuple[j]),
                              enif_make_copy(env, head));
            }

            array[i].x[j] = fp;

        }

        list_ptr = tail;
        i++;
    }

    /* at this point we have constructed array from previous data
       concatenated with newly added data. Thus, we clear old
       memory. Only reindex the new and then swap it up */

    tree->root.node_3d = make_tree_3d( array, tree->size + list_size, 0, tree->dimension);

    enif_free(tree->array.node_3d);

    tree->array.node_3d = array;
    tree->size += list_size;
    tree->ready = 1;

    return try_make_existing_atom(env, "ok");
}

ERL_NIF_TERM store_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    char *filename, *err;

    FILE *file;

    /* ERL_NIF_TERM *list, *point; */

    if (argc !=2 ) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    filename = gimme_string(env, &argv[1]);
    if (filename == NULL)
        error2(env, "invalid_filename", enif_make_copy(env, argv[1]));

    file = fopen (filename, "w");

    if (file == NULL) {
        err = strerror(errno);
        return
            error3(env, "cannot_open_file",
                   enif_make_copy(env, argv[1]),
                   enif_make_string(env, err, ERL_NIF_LATIN1));
    }

    fwrite(FMARK, 8, 1, file);

    fwrite(tree, sizeof(KD_TREE_T), 1, file);

    if (tree->dimension <= MAX_DIM) {
        fwrite(tree->array.node_3d, sizeof(NODE_3D_T), tree->size, file);
    } else {
        enif_free(filename);
        fclose(file);
        return not_implemented(env);
    }

    enif_free(filename);

    fclose(file);

    return try_make_existing_atom(env, "ok");
}

ERL_NIF_TERM load_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    char mark[8];

    char *filename, *err;

    FILE *file;

    ERL_NIF_TERM rsrce_term;

    size_t got;

    if (argc != 1) return enif_make_badarg(env);

    if (NULL == (filename = gimme_string(env, &argv[0])))
        error2(env, "invalid_filename", enif_make_copy(env, argv[0]));

    file = fopen (filename, "r");

    if (file == NULL) {
        err = strerror(errno);
        return error3(
            env, "cannot_open_file",
            enif_make_copy(env, argv[0]),
            enif_make_string(env, err, ERL_NIF_LATIN1));
    }

    fread(mark, 8, 1, file);

    if (strncmp(mark, FMARK, 8)) {
        fclose(file);
        return error3(env, "file_version_mismatch",
                      enif_make_copy(env, argv[0]),
                      enif_make_string(env, mark, ERL_NIF_LATIN1));
    }

    tree = enif_alloc_resource(KDTREE_RESOURCE, sizeof(KD_TREE_T));

    fread(tree, sizeof(KD_TREE_T), 1, file);

#ifdef DEBUG
    printf("read, got header: size %"PRIu64" dimension %"PRIu64"\r\n",
           tree->size, tree->dimension);
#endif

    if (tree->dimension <= MAX_DIM) {
        tree->array.node_3d = enif_alloc(sizeof(NODE_3D_T) * tree->size);
        got = fread(tree->array.node_3d, sizeof(NODE_3D_T), tree->size, file);
    } else {
        fclose(file);
        return not_implemented(env);
    }


#ifdef DEBUG
    printf("read, got the body: %lu\r\n", got);
#endif

    fclose(file);

    if (tree->size != got)
        return error3(env, "file_stream_size_mismatch",
                      enif_make_int(env, tree->size),
                      enif_make_int(env, got));

#ifdef DEBUG

    printf("indexing...");
    fflush(stdout);
    tree->root.node_3d =
        make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);
    printf("done.\r\n");

#else
    tree->root.node_3d =
        make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);
#endif

    tree->ready = 1;

    rsrce_term = enif_make_resource(env, tree);

    enif_release_resource(tree);

    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        rsrce_term);
}

/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
