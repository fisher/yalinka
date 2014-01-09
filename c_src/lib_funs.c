/*
 *     lib_funs.c
 *
 *     common c functions library, erl_nif related
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

/*
 * helper fun. In: erlang atom() | binary() in *term
 *            Out: C-style null-terminated string in *buff or NULL if errors
 */
char* gimme_string(ErlNifEnv *env, ERL_NIF_TERM *term, char *buff) {

  /* needed as a placeholder for binary */
  ErlNifBinary erlbin;
  /* needed for dealing with atom */
  unsigned int atom_len;

  if (enif_is_binary(env, *term)) {
    if(!enif_inspect_binary(env, *term, &erlbin)) {
      return NULL;
    }

    buff = (char *) enif_alloc(erlbin.size +1);
    memcpy(buff, erlbin.data, erlbin.size);
    buff[erlbin.size] = 0;

  } else if (enif_is_atom(env, *term)) {
    if(!enif_get_atom_length(env, *term, &atom_len, ERL_NIF_LATIN1)) {
        return NULL;
    }

    /* the len should be +1 for extra null character both in alloc and in getter */
    buff = (char *) enif_alloc(atom_len +1);
    if(!enif_get_atom(env, *term, buff, atom_len+1, ERL_NIF_LATIN1)) {
      printf("error copying atom to char*\r\n");
      return NULL;
    }

  } else if (enif_is_list(env, *term)) {

    if(!enif_inspect_iolist_as_binary(env, *term, &erlbin)) return NULL;
    buff = (char *) enif_alloc(erlbin.size +1);
    memcpy(buff, erlbin.data, erlbin.size);
    buff[erlbin.size] = 0;

    /* alternative approach is to use a termorary buffer
     * m_len = enif_get_string(env, *term, string, PAYLOAD_MAX_LEN, ERL_NIF_LATIN1);
     * if (atom_len < 1) return NULL;
     * buff = (char *) enif_alloc(atom_len);
     * memcpy(buff, string, atom_len);
     */

  } else {
    printf("not a binary nor an atom\r\n");
    return NULL;
  }

  return buff;
}

/*
 * helper fun to prevent creation of the same atoms
 *         In: atom_name
 *        Out: atom in term
 */
ERL_NIF_TERM try_make_existing_atom(ErlNifEnv *env, const char *atom_name) {

    ERL_NIF_TERM atom;

    if (!enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom_name);
    }

    return atom;
}

/*
 * helper fun to return {error, Explanation}
 */
ERL_NIF_TERM error1(ErlNifEnv *env, char *explanation) {
    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "error"),
        try_make_existing_atom(env, explanation));
}

/*
 * helper fun to return {error, {Explanation, Causing_term}}
 */
ERL_NIF_TERM error2(ErlNifEnv *env, char *explanation, ERL_NIF_TERM term) {
    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "error"),
        enif_make_tuple2(
            env,
            try_make_existing_atom(env, explanation),
            term));
}

/*
 * helper fun to return {error, {Explanation,
 *                               [{expected, Expected},
 *                                {got, Got},
 *                                {term, Causing_term}]}}
 */

ERL_NIF_TERM error4 ( ErlNifEnv *env,
                      char *explanation,
                      ERL_NIF_TERM expected,
                      ERL_NIF_TERM got,
                      ERL_NIF_TERM term)
{
    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "error"),
        enif_make_tuple2(
            env,
            try_make_existing_atom(env, explanation),
            enif_make_list3(
                env,
                enif_make_tuple2( env, try_make_existing_atom(env, "expected"), expected),
                enif_make_tuple2( env, try_make_existing_atom(env, "got"), got),
                enif_make_tuple2( env, try_make_existing_atom(env, "term"), term ))));
}

/*
 * helper function to return {error, not_implemented} back to erlang
 */
ERL_NIF_TERM not_implemented(ErlNifEnv *env)
{
    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "error"),
        try_make_existing_atom(env, "not_implemented_yet"));
}


/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
