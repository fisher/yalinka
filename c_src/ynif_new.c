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
#include "kdtree.h"

/* create new kdtree from incoming list */
ERL_NIF_TERM new_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{
    /* list */
    unsigned int list_size;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;

    /* iterator */
    unsigned int i;

    /* tuple */
    int arity;
    const ERL_NIF_TERM *tuple;

    /* resource reference term */
    ERL_NIF_TERM term;

    uint64_t asdf;

    /* just to make sure. and to satisfy compiler dilemma
       '(parameter name omitted' vs 'unused variable' */
    if (argc != 1) return enif_make_badarg(env);

    if (!enif_is_list(env, argv[0])) return enif_make_badarg(env);

    if (!enif_get_list_length(env, argv[0], &list_size))
        return enif_make_badarg(env);

    /* allocate memory here */

    printf("got list on input of size %d\r\nallocating %lu x %d bytes\r\n",
           list_size, sizeof(KD_NODE_T), list_size);

    KD_TREE_T tree = enif_alloc_resource(kdtree_resource_t, sizeof(node_ptr) *list_size);

    /* KD_TREE_T tree = (KD_TREE_T) malloc (sizeof(KD_NODE_T) *list_size); */

    i = 0;
    list = argv[0];

    while (enif_get_list_cell(env, list, &head, &tail)) {

        if (!enif_is_tuple(env, head)) return enif_make_badarg(env);

        if (!enif_get_tuple(env, head, &arity, &tuple)
            || arity != 4) {
            printf("error getting tuple\r\n");
            return enif_make_badarg(env);
        }


        for (int j = 0; j<4; j++) {
            if (!enif_is_number(env, tuple[j])) return enif_make_badarg(env);
        } 

        printf("trying to assign\r\n");

        if (!enif_get_uint64(env, tuple[0], (ErlNifUInt64*) &asdf)) return enif_make_badarg(env);

        tree[i] = (node_ptr) enif_alloc (sizeof(KD_NODE_T));

        printf("trying to assign [%d] to %" PRIu64 ".\r\n", i, asdf);

        tree[i]->idx = asdf;

        for (int j = 1; j<4; j++) {
            double inp;
            if (!enif_get_double(env, tuple[j], &inp)) return enif_make_badarg(env);

            printf("trying to assign [%d][%d] to %g\r\n", i, j, inp);
            tree[i]->x[j] = inp;

        }

        /* tree[i]->x[0] = 1.1; */
        /* tree[i]->x[1] = 1.2; */
        /* tree[i]->x[2] = 1.3; */

        printf("passed\r\n");

        list = tail;
        i++;
    }

    term = enif_make_resource(env, tree);

    /* if (keep_a_reference_of_our_own) { */
    /*     /\* store 'obj' in static variable, private data or other resource object *\/ */
    /* } */
    /* else { */
        enif_release_resource(tree);
    /*     /\* resource now only owned by "Erlang" *\/ */
    /* } */
    return term;
}
