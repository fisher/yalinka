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

#include <erl_nif.h>
#include "yalinka.h"
#include "lib_funs.h"
#include "kdtree.h"



/* just for debug purposes. please close your eyes, you don't see this */
void print_tree(KD_TREE_T *tree)
{
    printf("got tree of size %"PRIu64", dimension %"PRIu64"\r\n",
           tree->size, tree->dimension);
    for (unsigned int i = 0; i< tree->size; i++) {
        printf(" \\ [%"PRIu64"] (%g", tree->array[i].idx, tree->array[i].x[0]);
        for (unsigned int j = 1; j< tree->dimension; j++) {
            printf(", %g", tree->array[i].x[j]);
        }
        printf(")\r\n");
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
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, head, &arity, &tuple))
        return enif_make_badarg(env);

    if (enif_is_number(env, tuple[1])) {

        *type = 1;

        /* don't forget to decrement it after we check all the tuples
         * in a cycle below. I did it cause it seems stupid to
         * decrement dimension everytime there when we compare arity
         * of the tuple to the dimension. */
        tree->dimension = (uint64_t) arity;

    } else if (enif_is_tuple (env, tuple[1])) {

        *type = 2;

        if (!enif_get_tuple(env, tuple[1], &arity, &tuple2))
            return enif_make_badarg(env);

        tree->dimension = (uint64_t) arity;

    } else if (enif_is_list  (env, tuple[1])) {

        *type = 3;

        if (!enif_get_list_length(env, tuple[1], &list_size))
            return enif_make_badarg(env);

        tree->dimension = (uint64_t) arity;

    } else { *type = 0; tree->dimension = 0; }

    if (!enif_get_list_length(env, list, &list_size))
        return enif_make_badarg(env);


#ifdef DEBUG
    printf("list length %d, detected type %d of input, dimension %" PRIu64 "\r\n",
           list_size, *type, tree->dimension);
#endif

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
    uint64_t idx;

    node_ptr array;

    /* should be de-allocated in d-tor */
    array = enif_alloc(sizeof(KD_NODE_T) * tree->size);

    tree->array = array;

    i = 0;
    list_ptr = list;

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_is_tuple(env, head)) return enif_make_badarg(env);

        if (!enif_get_tuple(env, head, &arity, &tuple)
            || tree->dimension != (uint64_t) arity) {
            if (tree->dimension != (uint64_t) arity) {
                return enif_make_tuple2(
                    env,
                    try_make_existing_atom(env, "error"),
                    try_make_existing_atom(env, "invalid_dimension_in_data"));
            } else {
                return enif_make_badarg(env);
            }
        }

        for (int j = 0; j<arity; j++) {
            if (!enif_is_number(env, tuple[j])) return enif_make_badarg(env);
        }

        if (!enif_get_uint64(env, tuple[0], (ErlNifUInt64*) &idx))
            return enif_make_badarg(env);

        array[i].idx = idx;

        for (int j = 1; j<arity; j++) {
            double inp;
            if (!enif_get_double(env, tuple[j], &inp))
                return enif_make_badarg(env);

            array[i].x[j-1] = inp;

        }

        list_ptr = tail;
        i++;
    }


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

    ERL_NIF_TERM list_ptr, head, tail;
    const ERL_NIF_TERM *ext_tuple, *int_tuple;

    /* iterator */
    unsigned int i;

    int arity;
    uint64_t idx;

    node_ptr array;

    /* should be de-allocated in d-tor */
    array = enif_alloc(sizeof(KD_NODE_T) * tree->size);

    tree->array = array;

    i = 0;
    list_ptr = list;

    while (enif_get_list_cell(env, list_ptr, &head, &tail)) {

        if (!enif_is_tuple(env, head)) return enif_make_badarg(env);

        if (!enif_get_tuple(env, head, &arity, &ext_tuple))
            return enif_make_badarg(env);

        if (arity != 2)
            return enif_make_tuple2(
                env,
                try_make_existing_atom(env, "error"),
                try_make_existing_atom(env, "invalid_input_data"));

        if (!enif_is_number(env, ext_tuple[0])) return enif_make_badarg(env);

        if (!enif_get_uint64(env, ext_tuple[0], (ErlNifUInt64*) &idx))
            return enif_make_badarg(env);

        array[i].idx = idx;

        if (!enif_get_tuple(env, ext_tuple[1], &arity, &int_tuple))
            return enif_make_badarg(env);

        if (tree->dimension != (uint64_t) arity)
            return enif_make_tuple2(
                env,
                try_make_existing_atom(env, "error"),
                try_make_existing_atom(env, "invalid_dimension_in_data"));

        for (int j = 0; j<arity; j++) {
            if (!enif_is_number(env, int_tuple[j])) return enif_make_badarg(env);
        }

        for (int j = 0; j<arity; j++) {
            double inp;
            if (!enif_get_double(env, int_tuple[j], &inp))
                return enif_make_badarg(env);

            array[i].x[j-1] = inp;

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
    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "error"),
        try_make_existing_atom(env, "not_implemented_yet"));
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

    if (!enif_is_list(env, argv[0])) return enif_make_badarg(env);

    tree = enif_alloc_resource(KDTREE_RESOURCE, sizeof(KD_TREE_T));

    if ((result = inspect_first_cell(env, argv[0], tree, &type)))
        return result;

#ifdef DEBUG
    printf("detected dimension is %" PRIu64 "\r\n", tree->dimension);
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
    tree->root = make_tree( tree->array, tree->size, 0, tree->dimension);
    tree->sorted = 1;
    printf("done.\r\n");

    print_tree(tree);
#else
    tree->root = make_tree( tree->array, tree->size, 0, tree->dimension);
#endif

    tree->sorted = 1;

    term = enif_make_resource(env, tree);

    enif_release_resource(tree);

    result = enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        term);

    return result;
}
