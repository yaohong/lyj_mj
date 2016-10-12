#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "erl_nif.h"

typedef struct {
	int v1_;
} Foo;

static ERL_NIF_TERM change(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	enif_inspect_binary(env, argv[0], &nifBin);
	Foo *foo = (Foo *)(nifBin.data);
	
	int newValue = 0;
    enif_get_int(env, argv[1], &newValue);
	foo->v1_ = newValue;
	return enif_make_atom(env, "success");
}


static ERL_NIF_TERM init_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	int Ret = enif_alloc_binary(sizeof(Foo), &nifBin);
	printf("enif_alloc_binary %d\n", Ret);
	Foo *foo = (Foo *)(nifBin.data);
	foo->v1_ = 1;
	ERL_NIF_TERM binTerm = enif_make_binary(env, &nifBin);
	return enif_make_tuple(env, 2, enif_make_atom(env, "success"), binTerm);
}


static ErlNifFunc nif_funcs[] = {
	{"change", 2, change},
	{"init_nif", 0, init_nif}
};


ERL_NIF_INIT(nif_test, nif_funcs, NULL, NULL, NULL, NULL);