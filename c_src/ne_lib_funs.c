/*
 * Pure C module
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
ERL_NIF_TERM try_make_existing_atom(ErlNifEnv * env, const char *atom_name) {

    ERL_NIF_TERM atom;

    if (!enif_make_existing_atom(env, atom_name, &atom, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom_name);
    }

    return atom;
}
