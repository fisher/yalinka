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


ERL_NIF_TERM inspect_first_cell( ErlNifEnv *env,
                                 ERL_NIF_TERM list,
                                 KD_TREE_T *tree,
                                 int *type)
{
    unsigned int list_size;
    ERL_NIF_TERM head, tail;

    int arity;
    const ERL_NIF_TERM *tuple;

    if (!enif_get_list_cell(env, list, &head, &tail))
        return enif_make_badarg(env);

    if (!enif_get_tuple(env, head, &arity, &tuple))
        return enif_make_badarg(env);

    if      (!enif_is_number(env, head)) *type = 1;
    else if (!enif_is_tuple (env, head)) *type = 2;
    else if (!enif_is_list  (env, head)) *type = 3;
    else                                 *type = 0;

    if (!enif_get_list_length(env, list, &list_size))
        return enif_make_badarg(env);

    /* don't forget to decrement it after we check all the tuples in a
     * cycle below. I did it cause it seems stupid to decrement
     * dimension everytime there when we compare arity of the tuple to
     * the dimension. */
    tree->dimension = (uint64_t) arity;

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
            printf("error getting tuple\r\n");
            return enif_make_badarg(env);
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

/* create new kdtree from incoming list */
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
    printf("detected dimension is %d\r\n", tree->dimension -1);
#endif

    if ((result = fill_tree_from_plain_tuple(env, argv[0], tree)))
        return result;

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
