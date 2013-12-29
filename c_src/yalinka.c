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
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/time.h>
#include <errno.h>
#include <erl_nif.h>
#include "yalinka.h"
#include "ne_lib_funs.h"
/*#include "nif_external.h"*/

static int init_mod(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static ERL_NIF_TERM unixtime_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM tuple_return_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM list2_return_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM listn_return_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM list_return_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM revlist_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM test_nif(ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM getting_proplist_nif (ErlNifEnv*, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM normalize_to_bin_nif (ErlNifEnv*, int, const ERL_NIF_TERM []);

static ErlNifFunc nif_funcs[] = {
  /* fun, arity, c-fun */
  {"unixtime", 0, unixtime_nif},
  {"tuple_return", 0, tuple_return_nif},
  {"list2_return", 0, list2_return_nif},
  {"listn_return", 0, listn_return_nif},
  {"list_return", 1, list_return_nif},
  {"test", 1, test_nif},
  /*    {"test_external", 1, test_external_nif}, */
  {"revlist", 1, revlist_nif},
  {"normalize_to_bin", 1, normalize_to_bin_nif},
  {"getting_proplist", 1, getting_proplist_nif}
};

ERL_NIF_INIT(nif_example, nif_funcs, &init_mod, NULL, NULL, NULL)


typedef struct state {
  int64_t stateVersion;
  char *configuration;
} STATE;

STATE *global_state;

/* null-arity fun - returns an integer to erlang */
static ERL_NIF_TERM unixtime_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM* )
{
  timeval tv;

  memset(&tv, 0, sizeof(struct timeval));
  if(gettimeofday( &tv, NULL )) return enif_make_badarg(env);

  return enif_make_int64(env, tv.tv_sec);
}

/* key concept is how to return a tuple */
static ERL_NIF_TERM tuple_return_nif(ErlNifEnv* env, int, const ERL_NIF_TERM*)
{
  timeval tv;
  int error;

  /* return proper error from errno if any errors in there */
  if (gettimeofday( &tv, NULL )) {
    error = errno;
    return enif_make_tuple2( env,
                             enif_make_atom(env, "error"),
                             enif_make_atom(env, strerror(error)));
  }

  /* return tuple of size 2 with two integers in there */
  return enif_make_tuple2( env,
                           enif_make_int64(env, tv.tv_sec),
                           enif_make_int64(env, tv.tv_usec) );
}


/* key concept is to return a simple list */
static ERL_NIF_TERM list2_return_nif(ErlNifEnv* env, int, const ERL_NIF_TERM*)
{
  timeval tv;
  int error;

  /* return proper error if any */
  if (gettimeofday( &tv, NULL )) {
    error = errno;
    return enif_make_tuple2( env,
                             enif_make_atom(env, "error"),
                             enif_make_atom(env, strerror(error)));
  }

  /*just like with tuple */
  return enif_make_list2( env,
                          enif_make_int64(env, tv.tv_sec),
                          enif_make_int64(env, tv.tv_usec));
}


/* return list with variable length, from an array */
static ERL_NIF_TERM listn_return_nif(ErlNifEnv* env, int, const ERL_NIF_TERM*)
{

#define ASIZE 3

  ERL_NIF_TERM a[ASIZE];
  int i;

  for (i=0; i<ASIZE; i++) {
    printf("%d\r\n", i);
    a[i] = enif_make_int(env, i +1);
  }

  /* just like with tuple */
  return enif_make_list_from_array( env, a, ASIZE);
}


/* return list with variable length, taken from an argument */
static ERL_NIF_TERM list_return_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  int size;
  ERL_NIF_TERM *a;
  int i;

  if (argc != 1) return enif_make_badarg(env);

  if (!enif_get_int(env, argv[0], &size)) return enif_make_badarg(env);

  a = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) *size);

  for (i=0; i<size; i++) {
    a[i] = enif_make_int(env, i +1);
  }

  /* just like with tuple */
  return enif_make_list_from_array( env, a, size);
}

/* concept-fun: get an arg as a list and return in reverse order */
static ERL_NIF_TERM revlist_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  /* we need to modify this pointer */
  ERL_NIF_TERM list = argv[0];
  unsigned int list_size;
  ERL_NIF_TERM head, tail, result;
  ERL_NIF_TERM *arr;
  /* int *args; */
  int arg;

  int i = 0;

  /* if the arity =/= 1 - generate badarg */
  if (argc != 1) return enif_make_badarg(env);

  /* if it is not a list, return badarg to erlang */
  if(!enif_is_list(env, argv[0])) return enif_make_badarg(env);

  /* trying to get list lenght into list_size */
  if (!enif_get_list_length(env, argv[0], &list_size)) {
    return enif_make_badarg(env);
  }


  /* args = (int *) enif_alloc(sizeof(int) * list_size); */
  arr = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * list_size);

  /* printf("entering the cycle\r\n"); */
  while(enif_get_list_cell(env, list, &head, &tail)) {
    if(!enif_get_int(env, head, &arg)) {
      /* printf("error getting integer\r\n"); */
      return enif_make_badarg(env);
    }
    arr[i++] = enif_make_int(env, arg);
    list = tail;
  }

  result = enif_make_list_from_array( env, arr, list_size );

  /* be sure to free resources before returning from nif function */
  enif_free(arr);

  return result;

}


/*
 * concept-fun: get an arg, either binary or string, copy it into
 * null-terminated char*, and then return back to erlang binary from
 * char*
 */
static ERL_NIF_TERM test_nif(ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  int string_length;
  char buf[PAYLOAD_MAX_LEN];
  char *targetbuf;
  ERL_NIF_TERM retval;
  ErlNifBinary bindata;

  /* check if arg1 is binary() */
  if (enif_is_binary(env, argv[0])) {

    /* copy erlang binary() to C bindata */
    if (!enif_inspect_binary(env, argv[0], &bindata)
        || bindata.size >= PAYLOAD_MAX_LEN ) {
      return enif_make_badarg(env);
    }

    /* copy bindata to our custom char* (beware of zeroes in *bindata.data) */
    memcpy(buf, bindata.data, bindata.size);
    buf[bindata.size] = 0;
    string_length = bindata.size;

  } else if(enif_is_list(env, argv[0])) {

    /* trying to deal with arg1 as a string */
    string_length = enif_get_string(env, argv[0], buf, PAYLOAD_MAX_LEN, ERL_NIF_LATIN1);

    /* ret is bytes copied in buf including tailing zero */
    if (string_length < 1) return enif_make_badarg(env);

    string_length--;

  } else {

    /* it isn't a binary nor a string; it's fatal. */
    return enif_make_badarg(env);

  }

  /* at this point we have a null-terminated string in buf
   * now get a copy in new ErlNifBinary */

  targetbuf = (char *) enif_make_new_binary(env, string_length, &retval);

  /* we can modify data using targetbuf pointer
   * ErlNifBinary->size is set to size_t ret, don't write above */
  memcpy(targetbuf, buf, string_length);

  /* actual ErlNifBinary instance is hidden in retval */
  return enif_make_tuple2(env, enif_make_int(env, string_length), retval);

}

/* getting proplist */
static ERL_NIF_TERM getting_proplist_nif (ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
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

  /* placeholder for data */
  typedef struct kvpair {
    char *key;
    char *value;
  } KVPAIR;

  KVPAIR *kv;

  /* temorary storage for erlang binary term */
  ErlNifBinary erlbin;

  /* used to store atom_len when inspecting term */
  unsigned int atom_len;

  /* here we store a normalized proplist to return back */
  ERL_NIF_TERM *arr;

  /* the final result will be placed here */
  ERL_NIF_TERM result;

  /* temp buf */
  size_t len;
  char *targetbuf;

  /* if it is not a list return an error immidiately */
  if (!enif_is_list(env, argv[0])) return enif_make_badarg(env);

  /* trying to get list lenght into list_size */
  if (!enif_get_list_length(env, argv[0], &list_size)) {
    return enif_make_badarg(env);
  }

  /* allocate memory for our placeholder */
  kv = (KVPAIR *) enif_alloc(sizeof(KVPAIR) * list_size);

  /* init the index and pointer to list */
  i = 0;
  list = argv[0];

  while(enif_get_list_cell(env, list, &head, &tail)) {

    if (!enif_is_tuple(env, head)) return enif_make_badarg(env);

    if (!enif_get_tuple(env, head, &arity, &tuple)
        || arity != 2)
      {
        printf("error getting tuple\r\n");
        return enif_make_badarg(env);
      }

    if (enif_is_binary(env, tuple[0])) {
      if(!enif_inspect_binary(env, tuple[0], &erlbin)) {
        return enif_make_badarg(env);
      }

      kv[i].key = (char *) enif_alloc(erlbin.size +1);
      memcpy(kv[i].key, erlbin.data, erlbin.size);
      kv[i].key[erlbin.size] = 0;

    } else if (enif_is_atom(env, tuple[0])) {
      if(!enif_get_atom_length(env, tuple[0], &atom_len, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
      }

      /* the len should be +1 for extra null character both in alloc and in getter */
      kv[i].key = (char *) enif_alloc(atom_len +1);
      if(!enif_get_atom(env, tuple[0], kv[i].key, atom_len+1, ERL_NIF_LATIN1)) {
        printf("error copying atom to char* in tuples key\r\n");
        return enif_make_badarg(env);
      }

    } else {
      printf("cant decode a key from tuple\r\n");
      return enif_make_badarg(env);
    }

    /* and now for something completely different
     * value I mean */

    if (enif_is_binary(env, tuple[1])) {
      if(!enif_inspect_binary(env, tuple[1], &erlbin)) {
        return enif_make_badarg(env);
      }

      kv[i].value = (char *) enif_alloc(erlbin.size +1);
      memcpy(kv[i].value, erlbin.data, erlbin.size);
      kv[i].value[erlbin.size] = 0;

    } else if (enif_is_atom(env, tuple[1])) {
      if(!enif_get_atom_length(env, tuple[1], &atom_len, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
      }

      kv[i].value = (char *) enif_alloc(atom_len +1);
      if(!enif_get_atom(env, tuple[1], kv[i].value, atom_len+1, ERL_NIF_LATIN1)) {
        printf("error copying atom to char* in tuples key\r\n");
        return enif_make_badarg(env);
      }

    } else {
      printf("cant decode value from tuple\r\n");
      return enif_make_badarg(env);
    }

    list = tail;
    i++;
  }

  /* here we have 'kv' filled by null-terminated strings from erlang list */

  /* we can transform the content of kv */

  /* starting from here we're making resulting term from kv */

  /* allocate memory for our terms to return back */
  arr = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * list_size);

  for (i=0; i<list_size; i++) {

    /* see comments for test_fun_nif() */
    len = strlen(kv[i].value);
    targetbuf = (char *) enif_make_new_binary(env, len, &result);
    memcpy(targetbuf, kv[i].value, len);

    arr[i] = enif_make_tuple2( env,
                               try_make_existing_atom(env, kv[i].key),
                               result);
  }

  /* need to be free */
  enif_free (kv);

  result = enif_make_list_from_array( env, arr, list_size );

  /* we don't want the leakage, aren't we? */
  enif_free (arr);

  return result;
}

/* transform incoming list of atoms/strings to list of binaries */
static ERL_NIF_TERM normalize_to_bin_nif (ErlNifEnv* env, int, const ERL_NIF_TERM argv[])
{
  /* list */
  unsigned int list_size;
  ERL_NIF_TERM list;
  ERL_NIF_TERM head, tail;

  /* iterator */
  unsigned int i;

  /* here we store a normalized proplist to return back */
  ERL_NIF_TERM *arr;

  /* the final result will be placed here */
  ERL_NIF_TERM result;

  /* temp buf */
  char **container;
  size_t len;
  char *targetbuf;

  /* if it is not a list return an error immidiately */
  if (!enif_is_list(env, argv[0])) return enif_make_badarg(env);

  /* trying to get list lenght into list_size */
  if (!enif_get_list_length(env, argv[0], &list_size)) {
    return enif_make_badarg(env);
  }

  /* allocate memory for an array */
  container = (char **) enif_alloc(sizeof(char *) * list_size);

  /* init the index and pointer to list */
  i = 0;
  list = argv[0];

  while(enif_get_list_cell(env, list, &head, &tail)) {

    if(NULL == (container[i] = gimme_string(env, &head, container[i])))
      return enif_make_badarg(env);

    list = tail;
    i++;
  }

  arr = (ERL_NIF_TERM *) enif_alloc(sizeof(ERL_NIF_TERM) * list_size);

  for (i=0; i<list_size; i++) {
    len = strlen(container[i]);
    targetbuf = (char *) enif_make_new_binary(env, len, &result);
    memcpy(targetbuf, container[i], len);

    arr[i] = result;
  }

  enif_free(container);

  result = enif_make_list_from_array( env, arr, list_size );

  enif_free(arr);

  return result;

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
  cout << "*** example_nif module init...\r\n";
#endif

  /* if we cannot get int from load_info, module loading in erlang will fail */
  if (!enif_get_int64(env, load_info, &arg)) {
    return 1;
  }

  global_state = (STATE*) enif_alloc (sizeof(STATE));

  global_state->configuration = (char *) enif_alloc (8);

  memcpy (global_state->configuration, "sldkfj", 8);

  global_state->stateVersion = arg;

  /* storing global state between NIF calls */
  *priv_data = global_state;

#ifdef DEBUG
  cout << "*** initialization done, load info: " << arg << "\r\n";
#endif
  return 0;
}



/* Local Variables: */
/* indent-tabs-mode: nil */
/* show-trailing-whitespace: t */
/* mode: c */
/* End: */
