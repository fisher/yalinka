/*
 *     ynif_storage.c
 *
 *     store/2 and load/1 implementation
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
#include <errno.h>
#include <erl_nif.h>
#include "yalinka.h"
#include "lib_funs.h"
#include "kdtree.h"


/* current file version watermark, 8 octets */
#define FMARK "yalinka2"


/* storing tree object into given file */
ERL_NIF_TERM store_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    char *filename, *err;

    FILE *file;

    if (argc !=2 ) return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], KDTREE_RESOURCE, (void **) &tree))
        return invalid_ref(env, argv[0]);

    filename = gimme_string(env, &argv[1]);
    if (filename == NULL)
        error2(env, "invalid_filename", enif_make_copy(env, argv[1]));

    file = fopen (filename, "w");

    if (file == NULL) {
        err = strerror(errno);
        return
            error3(env, "cannot_open_file",
                   enif_make_copy(env, argv[1]),
                   enif_make_string(env, err, ERL_NIF_LATIN1));
    }

    fwrite(FMARK, 8, 1, file);

    fwrite(tree, sizeof(KD_TREE_T), 1, file);

    if (tree->dimension <= MAX_DIM) {
        fwrite(tree->array.node_3d, sizeof(NODE_3D_T), tree->size, file);
    } else {
        fwrite(tree->array.node_kd, sizeof(NODE_KD_T), tree->size, file);
        for (unsigned int i=0; i<tree->size; i++) {
            fwrite(tree->array.node_kd[i].x, sizeof(double), tree->dimension, file);
        }
    }

    enif_free(filename);

    fclose(file);

    return try_make_existing_atom(env, "ok");
}

/* loading stored tree object into memory */
ERL_NIF_TERM load_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    KD_TREE_T *tree;

    char mark[8];

    char *filename, *err;

    FILE *file;

    ERL_NIF_TERM rsrce_term;

    size_t got;

    if (argc != 1) return enif_make_badarg(env);

    if (NULL == (filename = gimme_string(env, &argv[0])))
        error2(env, "invalid_filename", enif_make_copy(env, argv[0]));

    file = fopen (filename, "r");

    if (file == NULL) {
        err = strerror(errno);
        return error3(
            env, "cannot_open_file",
            enif_make_copy(env, argv[0]),
            enif_make_string(env, err, ERL_NIF_LATIN1));
    }

    fread(mark, 8, 1, file);

    if (strncmp(mark, FMARK, 8)) {
        fclose(file);
        return error3(env, "file_version_mismatch",
                      enif_make_copy(env, argv[0]),
                      enif_make_string(env, mark, ERL_NIF_LATIN1));
    }

    tree = enif_alloc_resource(KDTREE_RESOURCE, sizeof(KD_TREE_T));

    fread(tree, sizeof(KD_TREE_T), 1, file);

#ifdef DEBUG
    printf("read, got header: size %"PRIu64" dimension %"PRIu64"\r\n",
           tree->size, tree->dimension);
#endif

    if (tree->dimension <= MAX_DIM) {
        tree->array.node_3d = enif_alloc(sizeof(NODE_3D_T) * tree->size);
        got = fread(tree->array.node_3d, sizeof(NODE_3D_T), tree->size, file);
    } else {
        tree->array.node_kd = enif_alloc(sizeof(NODE_KD_T) * tree->size);
        got = fread(tree->array.node_kd, sizeof(NODE_KD_T), tree->size, file);
        for (unsigned int i=0; i<tree->size; i++) {
            tree->array.node_kd[i].x = enif_alloc(sizeof(double) *tree->dimension);
            fread(tree->array.node_kd[i].x, sizeof(double), tree->dimension, file);
        }
    }


#ifdef DEBUG
    printf("read, got the body: %lu\r\n", got);
#endif

    fclose(file);

    if (tree->size != got)
        return error3(env, "file_stream_size_mismatch",
                      enif_make_int(env, tree->size),
                      enif_make_int(env, got));

#ifdef DEBUG
    printf("indexing...");
    fflush(stdout);
#endif

    if (tree->dimension <= MAX_DIM) {
        tree->root.node_3d =
            make_tree_3d( tree->array.node_3d, tree->size, 0, tree->dimension);
    } else {
        tree->root.node_kd =
            make_tree_kd( tree->array.node_kd, tree->size, 0, tree->dimension);
    }

#ifdef DEBUG
    printf("done.\r\n");
#endif

    tree->ready = 1;

    rsrce_term = enif_make_resource(env, tree);

    enif_release_resource(tree);

    return enif_make_tuple2(
        env,
        try_make_existing_atom(env, "ok"),
        rsrce_term);
}

/*
 * Local Variables:
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * mode: c
 * End:
 *
 */
