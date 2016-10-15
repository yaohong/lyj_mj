#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "erl_nif.h"
#include "game_logic.h"

static ERL_NIF_TERM change(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	enif_inspect_binary(env, argv[0], &nifBin);
	MainLogic *foo = (MainLogic *)(nifBin.data);
	
	int newValue = 0;
    enif_get_int(env, argv[1], &newValue);
	foo->v1_ = newValue;
	return enif_make_atom(env, "success");
}


static ERL_NIF_TERM game_start(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	int Ret = enif_alloc_binary(sizeof(MainLogic), &nifBin);
	printf("enif_alloc_binary %d\n", Ret);

	int brankerNumer = -1;
	enif_get_int(env, argv[1], &brankerNumer);
	MainLogic *foo = (MainLogic *)(nifBin.data);
	foo->Init();
	ERL_NIF_TERM binTerm = enif_make_binary(env, &nifBin);
	return enif_make_tuple(env, 2, enif_make_atom(env, "success"), binTerm);
}


static ErlNifFunc nif_funcs[] = {
	{"game_start", 1, game_start}
};


ERL_NIF_INIT(mj_nif, nif_funcs, NULL, NULL, NULL, NULL);