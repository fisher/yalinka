#ifndef NE_LIB_FUNS_H
#define NE_LIB_FUNS_H

#include <erl_nif.h>

extern char* gimme_string(ErlNifEnv *env, ERL_NIF_TERM *term, char *buff);
extern ERL_NIF_TERM try_make_existing_atom(ErlNifEnv *env, const char *atom_name);

#endif
