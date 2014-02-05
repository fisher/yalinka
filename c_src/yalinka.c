/*
 *     yalinka.c
 *
 *     Erlang NIF interface to K-dimensional trees
 *
 *     this file is part of 'yalinka' project, http://yalinka.heim.in.ua
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
#include "ynif_new.h"
#include "ynif_search.h"
#include "ynif_other.h"

static int  init_mod  (ErlNifEnv*, void** priv_data, ERL_NIF_TERM load_info);
static void unload_mod(ErlNifEnv*, void*  priv_data);

static ErlNifFunc nif_funcs[] = {
    /* fun, arity, c-fun */
    {"new", 1, new_nif},
    {"clear", 1, clear_nif},
    {"size", 1, size_nif},
    {"dimension", 1, dimension_nif},
    {"search", 3, search3_nif},
    {"search", 2, search2_nif},
    {"store", 2, store_nif},
    {"load", 1, load_nif},
    {"root", 1, root_nif},
    {"node", 2, node_nif},
    {"index", 1, index_nif},
    {"add", 2, add_nif},
    {"insert", 2, insert_nif},
    {"compare", 2, compare_nif},
    {"is_ready", 1, is_ready_nif},
    {"gettree", 1, gettree_nif}
};

ERL_NIF_INIT(yalinka, nif_funcs, &init_mod, NULL, NULL, &unload_mod)


typedef struct state {
  int64_t stateVersion;
  char *configuration;
} STATE;

STATE *global_state;


void kdtree_dtor(ErlNifEnv* env, void* arg)
{
  KD_TREE_T *handler = (KD_TREE_T *) arg;

#ifdef DEBUG

  printf("d-tor %" PRIx64 " for %" PRIu64 "x%" PRIu64 " started...",
         (unsigned long int) handler, handler->dimension, handler->size);

  enif_free(handler->array);

  printf("done\r\n");

#else

  enif_free(handler->array);

#endif

}


/*
 * this fun is called on module load. concept here is to store some
 * state between NIF fun calls, in special place, **priv_data
 *
 */
static int init_mod(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int64_t arg;

#ifdef DEBUG
    printf("*** yalinka module init...\r\n");
#endif

    /* if we cannot get int from load_info, module loading in erlang
       will fail */
    if (!enif_get_int64(env, load_info, &arg)) {
        return 1;
    }

    ErlNifResourceFlags flags =
        (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

    KDTREE_RESOURCE = enif_open_resource_type
        (env, "kdtree_rsrc_t", "kd3_t", &kdtree_dtor, flags, NULL);

    global_state = (STATE*) enif_alloc (sizeof(STATE));

    global_state->configuration = (char *) enif_alloc (8);

    memcpy (global_state->configuration, "sldkfj", 8);

    global_state->stateVersion = arg;

    /* storing global state between NIF calls */
    *priv_data = global_state;

#ifdef DEBUG
    printf("*** initialization done, load info: %" PRIu64 "\r\n", arg);
#endif
    return 0;
}


static void unload_mod(ErlNifEnv* env, void* priv_data)
{
    STATE *state = (STATE *) enif_priv_data(env);

#ifdef DEBUG
    printf("*** unload sequence for yalinka NIF module\r\n");
#endif

    enif_free(state->configuration);

    enif_free(priv_data);

    return;
}


/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
