#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "erl_nif.h"
#include "hh_game_logic.h"

static ERL_NIF_TERM test_func(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	enif_inspect_binary(env, argv[0], &nifBin);
	MainLogic *foo = (MainLogic *)(nifBin.data);
	
	return enif_make_badarg(env);
}


static ERL_NIF_TERM game_start(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	int brankerNumber = -1;
	int Ret = enif_alloc_binary(sizeof(HH_MainLogic), &nifBin);
	printf("enif_alloc_binary %d\n", Ret);
	HH_MainLogic *foo = (HH_MainLogic *)(nifBin.data);
	enif_get_int(env, argv[1], &brankerNumber);
	Init(foo, (int8)brankerNumber);
	ERL_NIF_TERM binTerm = enif_make_binary(env, &nifBin);
	return enif_make_tuple(env, 2, enif_make_atom(env, "success"), binTerm);
}


static ErlNifFunc nif_funcs[] = {
	{"game_start", 1, game_start},
	{"test_func", 0, test_func}
};


ERL_NIF_INIT(mj_nif, nif_funcs, NULL, NULL, NULL, NULL);
