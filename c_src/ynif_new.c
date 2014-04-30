/*
 *     ynif_new.c
 *
 *     yalinka:new/1 implementation
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



/* just for debug purposes. please close your eyes, you don't see this */
void print_tree(KD_TREE_T *tree)
{
    if (tree->size<10) {

        printf("got tree of size %"PRIu64", dimension %"PRIu64"\r\n",
               tree->size, tree->dimension);

        if (tree->dimension <= MAX_DIM) {
            for (unsigned int i = 0; i< tree->size; i++) {
                printf(" \\ [%"PRIu64"] (%g", tree->array.node_3d[i].idx, tree->array.node_3d[i].x[0]);
                for (unsigned int j = 1; j< tree->dimension; j++) {
                    printf(", %g", tree->array.node_3d[i].x[j]);
                }
                printf(")\r\n");
            }
        } else {
            for (unsigned int i = 0; i< tree->size; i++) {
                printf(" \\ [%"PRIu64"] (%g", tree->array.node_kd[i].idx, tree->array.node_kd[i].x[0]);
                for (unsigned int j = 1; j< tree->dimension; j++) {
                    printf(", %g", tree->array.node_kd[i].x[j]);
                }
                printf(")\r\n");
            }
        }
    } else {
        printf("got TOO BIG tree of size %"PRIu64", dimension %"PRIu64". NOT PRINTING.\r\n",
               tree->size, tree->dimension);
    }
}


/*
 * takes the first cell of the list from the arguments and inspects
 * it. After that fill some fields in the tree like dimension and sets
 * the type of what we've found for further processing.
 */

ERL_NIF_TERM inspect_first_cell( ErlNifEnv *env,
                                 ERL_NIF_TERM list,
                                 KD_TREE_T *tree,
                                 int *type)
{
    unsigned int list_size;
    ERL_NIF_TERM head, tail;

    int arity;
    const ERL_NIF_TERM *tuple, *tuple2;

    if (!enif_get_list_cell(env, list, &head, &tail))
        return error2(env, "list_expected", enif_make_copy(env, list));

    if (!enif_get_tuple(env, head, &arity, &tuple))
        return error2(env, "tuple_expected", enif_make_copy(env, head));

    if (enif_is_number(env, tuple[1])) {

        *type = 1;

        tree->dimension = (uint64_t) arity -1;

    } else if (enif_is_tuple (env, tuple[1])) {

        *type = 2;

        if (!enif_get_tuple(env, tuple[1], &arity, &tuple2))
            return error2(env, "inner_tuple_expected", enif_make_copy(env, head));

        tree->dimension = (uint64_t) arity;

    } else if (enif_is_list  (env, tuple[1])) {

        *type = 3;

        if (!enif_get_list_length(env, tuple[1], &list_size))
            return error2(env, "inner_list_expected", enif_make_copy(env, head));

        tree->dimension = (uint64_t) list_size;

    } else { *type = 0; tree->dimension = 0; }


    if (!enif_get_list_length(env, list, &list_size))
        return error1(env, "error_getting_list_length");

    tree->size = (uint64_t) list_size;

    return 0;
}


/*
 * {Idx :: integer(), Coordinate :: float(), ...}
 * size of the tuple in tree->dimension
 * lenght of the list in tree->size
 */
ERL_NIF_TERM fill_tree_from_plain_tuple( ErlNifEnv *env,
                                         ERL_NIF_TERM list,
                                         KD_TREE_T *tree )
{

    ERL_NIF_TERM list_ptr, head, tail;
    const ERL_NIF_TERM *tuple;

    /* iterator */
    unsigned int i;

    int arity;
    /* uint64_t idx; */

    node_3d_ptr array;
    node_kd_ptr kd_array;
    double  *pts;

    ERL_NIF_TERM result;

    /* should be de-allocated in d-tor */
    if (tree->dimension <= MAX_DIM) {
        array = enif_alloc(sizeof(NODE_3D_T) * tree->size);
        tree->array.node_3d = array;
    } else {
        kd_array = enif_alloc(sizeof(NODE_KD_T) * tree->size);
        pts = enif_alloc(sizeof(double) * tree->size * tree->dimension);
        tree->array.node_kd = kd_array;
        tree->pts = pts;
    }

    i = 0;
    list_ptr = list;

    tree->dimension++;

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_is_tuple(env, head))
            return error2(env, "tuple_expected", enif_make_copy(env, head));

        if (!enif_get_tuple(env, head, &arity, &tuple))
            return error2(env, "getting_tuple", enif_make_copy(env, head));

        if (tree->dimension != (uint64_t) arity)
            return error4 ( env, "invalid_tuple_arity",
                            enif_make_uint64(env, tree->dimension),
                            enif_make_int(env, arity),
                            enif_make_copy(env, head) );

        if ((result = fill_node_tag(env, tuple[0], &array[i].idx)))
            return result;

        if (tree->dimension <= (MAX_DIM +1)) {

            for (int j = 1; j<arity; j++) {
                double inp;

                if (!enif_get_double(env, tuple[j], (double*) &inp)) {
                    return error4(env, "invalid_node_spec",
                                  try_make_existing_atom(env, "float"),
                                  enif_make_copy(env, tuple[j]),
                                  enif_make_copy(env, head));
                }

                array[i].x[j-1] = inp;

            }
        } else {

            /* kd_array[i].x = pts; */
            /* pts += (tree->dimension -1); */

            printf("asdf\r\n");
            for (int j = 1; j<arity; j++) {
                double inp2;

                printf("if, j=%d/%d\r\n", j, arity);
                if (!enif_get_double(env, tuple[j], (double*) &inp2)) {
                    if (enif_is_number(env, tuple[j])) {
                        printf("is_number: true\r\n");
                    }
                    printf("eggog \r\n");
                    return error4(env, "invalid_node_spec",
                                  try_make_existing_atom(env, "float"),
                                  enif_make_copy(env, tuple[j]),
                                  enif_make_copy(env, head));
                }

                printf("qwer (i): %d, %d\r\n", i, j);
                kd_array[i].x[j-1] = inp2;
                printf("qwer (f): %g\r\n", inp2);
            }
        }

        list_ptr = tail;
        i++;
    }

    printf("zxcv\r\n");
    tree->dimension--;

    return 0;
}

/*
 * {Idx :: integer(), { Coordinate :: float(), ... } }
 * size of the tuple in tree->dimension
 * lenght of the list in tree->size
 */

ERL_NIF_TERM fill_tree_from_tuple( ErlNifEnv *env,
                                   ERL_NIF_TERM list,
                                   KD_TREE_T *tree )
{

    ERL_NIF_TERM list_ptr, head, tail, result;
    const ERL_NIF_TERM *ext_tuple, *int_tuple;

    /* iterator */
    unsigned int i;

    int arity;

    node_3d_ptr array;

    /* should be de-allocated in d-tor */
    if (tree->dimension <= MAX_DIM) {
        array = enif_alloc(sizeof(NODE_3D_T) * tree->size);
    } else {
        return not_implemented(env);
    }

    tree->array.node_3d = array;

    i = 0;
    list_ptr = list;

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_get_tuple(env, head, &arity, &ext_tuple))
            return error2(env, "tuple_expected", enif_make_copy(env, head));

        if (arity != 2)
            return error2(env, "expected_tuple2", enif_make_copy(env, head));

        if ((result = fill_node_tag(env, ext_tuple[0], &array[i].idx)))
            return result;

        if (!enif_get_tuple(env, ext_tuple[1], &arity, &int_tuple))
            return error2(env, "tuple_expected", enif_make_copy(env, ext_tuple[1]));

        if (tree->dimension != (uint64_t) arity)
            return error4( env, "invalid_dimension_in_data",
                            enif_make_uint64(env, tree->dimension),
                            enif_make_int(env, arity),
                            enif_make_copy(env, head) );

        for (int j = 0; j<arity; j++) {
            double inp;
            if (!enif_get_double(env, int_tuple[j], (double*) &inp))
                return error4(env, "invalid_node_spec",
                              try_make_existing_atom(env, "float"),
                              enif_make_copy(env, int_tuple[j]),
                              enif_make_copy(env, head));

            array[i].x[j] = inp;

        }

        list_ptr = tail;
        i++;
    }

    return 0;
}

/*
 * {Idx :: integer(), [ Coordinate :: float(), ... ] }
 * In:
 * size of the inner list in tree->dimension
 * lenght of the whole outer list with nodes is in tree->size
 */

ERL_NIF_TERM fill_tree_from_list( ErlNifEnv *env,
                                  ERL_NIF_TERM list,
                                  KD_TREE_T *tree )
{

    /* the whole list of tuples */
    ERL_NIF_TERM list_ptr, head, tail;

    /* placeholder for the tuple container and its arity */
    const ERL_NIF_TERM *tuple;
    int arity;

    /* iterator */
    unsigned int i;

    /* inner list of points */
    ERL_NIF_TERM inner_list, ihead, itail;
    unsigned int list_size;

    ERL_NIF_TERM result;

    node_3d_ptr array;

    double inp;
    int j;

    /* should be de-allocated in d-tor */
    if (tree->dimension <= MAX_DIM) {
        array = enif_alloc(sizeof(NODE_3D_T) * tree->size);
    } else {
        return not_implemented(env);
    }

    tree->array.node_3d = array;

    i = 0;
    list_ptr = list;

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_get_tuple(env, head, &arity, &tuple) || arity != 2)
            return error2(env, "expected_tuple2", enif_make_copy(env, head));

        if ((result = fill_node_tag(env, tuple[0], &array[i].idx)))
            return result;

        /* get all the points from the inner list */
        if (!enif_get_list_length(env, tuple[1], &list_size))
            return error2(env, "inner_list_expected",
                          enif_make_copy(env, head));

        if (tree->dimension != (uint64_t) list_size)
            return error4 ( env,
                            "invalid_dimension_in_data",
                            enif_make_uint64(env, tree->dimension),
                            enif_make_uint(env, list_size),
                            enif_make_copy(env, head) );


        inner_list = tuple[1];
        j = 0;
        while (enif_get_list_cell(env, inner_list, &ihead, &itail)) {

            if (!enif_get_double(env, ihead, &inp))
                return error4(env, "invalid_node_spec",
                              try_make_existing_atom(env, "float"),
                              enif_make_copy(env, ihead),
                              enif_make_copy(env, head));

            array[i].x[j] = inp;

            inner_list = itail;
            j++;

        }

        list_ptr = tail;
        i++;
    }

    return 0;
}

ERL_NIF_TERM add_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

	node_3d_ptr array;

    unsigned int list_size;

    ERL_NIF_TERM list_ptr, head, tail;

    int tuple_arity;
    const ERL_NIF_TERM *tuple;

    int dim;
    const ERL_NIF_TERM *int_tuple;

    int i;
    ERL_NIF_TERM result;

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

        if (!enif_get_tuple(env, head, &tuple_arity, &tuple) || tuple_arity != 2) {
            enif_free(array);
            return error2(env, "expected_tuple2", enif_make_copy(env, head));
        }

        if((result = fill_node_tag(env, tuple[0], &array[i].idx))) {
            enif_free(array);
            return result;
        }

        if(!enif_get_tuple(env, tuple[1], &dim, &int_tuple)
           || (unsigned int) dim != tree->dimension) {
            enif_free(array);
            return error4(env, "invalid_dimension_in_data",
                          enif_make_uint64(env, tree->dimension),
                          enif_make_int(env, dim),
                          enif_make_copy(env, tuple[1]));
        }

        for (int j = 0; j<dim; j++) {
            double inp;
            if(!enif_get_double(env, int_tuple[j], (double*) &inp)) {
                enif_free(array);
                return error4(env, "invalid_node_spec",
                              try_make_existing_atom(env, "float"),
                              enif_make_copy(env, int_tuple[j]),
                              enif_make_copy(env, head));
            }
            array[i].x[j] = inp;
        }

        list_ptr = tail;
        i++;

    }

    enif_free(tree->array.node_3d);

    tree->array.node_3d = array;
    tree->size += list_size;
    tree->ready = 0;

    return try_make_existing_atom(env, "ok");
}


/*
 * create new kdtree object from incoming list. This is a function
 * listed in ErlNifFunc and the only function exported from this
 * module.
 */
ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    /* resource reference term */
    ERL_NIF_TERM term;

    /* placeholder for resulting term, like {ok, Reference} */
    ERL_NIF_TERM result;

    /* type of the argument */
    int type;

    /* new tree object */
    KD_TREE_T *tree;

    /* just to make sure. and to satisfy compiler dilemma
       '(parameter name omitted' vs 'unused variable' */
    if (argc != 1) return enif_make_badarg(env);

    if (!enif_is_list(env, argv[0])) {
        if (enif_is_tuple(env, argv[0])) {
            return not_implemented(env);
        } else return enif_make_badarg(env);
    }

    tree = enif_alloc_resource(KDTREE_RESOURCE, sizeof(KD_TREE_T));

    if ((result = inspect_first_cell(env, argv[0], tree, &type)))
        return result;

#ifdef DEBUG

    printf("alloc %" PRIx64 " for %" PRIu64 "x%" PRIu64 "\r\n",
           (unsigned long int) tree, tree->dimension, tree->size);

    printf("list length %"PRIu64", detected type %d of input,"
           " dimension %"PRIu64"\r\n",
           tree->size, type, tree->dimension);
#endif

    switch(type) {
    case 1:
        if ((result = fill_tree_from_plain_tuple(env, argv[0], tree)))
            return result;
        break;
    case 2:
        if ((result = fill_tree_from_tuple(env, argv[0], tree)))
            return result;
        break;
    case 3:
        if ((result = fill_tree_from_list(env, argv[0], tree)))
            return result;
        break;
    default:
        return enif_make_badarg(env);
    }

#ifdef DEBUG
    print_tree(tree);

    printf("indexing...");
    tree->root.node_3d =
        make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);
    printf("done.\r\n");

    print_tree(tree);
#else
    tree->root.node_3d =
        make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);
#endif

    tree->ready = 1;

    term = enif_make_resource(env, tree);

    enif_release_resource(tree);

    result = enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        term);

    return result;
}

ERL_NIF_TERM index_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    KD_TREE_T *tree;

    if(argc !=1) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    tree->root.node_3d = make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);

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
