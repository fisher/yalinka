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

#include <erl_nif.h>
#include "yalinka.h"
#include "kdtree.h"
#include "lib_funs.h"

/*
 * spec search(reference(), {float(), float(), float()}, integer()) ->
 *                     [{integer(), float()}].
 */
ERL_NIF_TERM search_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv)
{

    ERL_NIF_TERM result;

    KD_TREE_T *tree;

    uint64_t howmuch;

    const ERL_NIF_TERM *tuple;
    int tuple_arity;

    if (argc != 3) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return enif_make_badarg(env);

    if (!enif_is_tuple(env, argv[1])) return enif_make_badarg(env);

    if (!enif_is_number(env, argv[2])) return enif_make_badarg(env);

    if (!enif_get_tuple(env, argv[1], &tuple_arity, &tuple) ||
        tuple_arity != 3) return enif_make_badarg(env);

    if (!enif_get_uint64(env, argv[2], (ErlNifUInt64*) &howmuch))
        return enif_make_badarg(env);

    /* meat here */

    result = enif_make_tuple3( env,
                               try_make_existing_atom(env, "ok"),
                               enif_make_int(env, howmuch),
                               enif_make_int(env, tree->size));

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
