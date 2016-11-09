#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>
#include "erl_nif.h"
#include "hh_game_logic.h"

static ERL_NIF_TERM test_func(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	ErlNifBinary nifBin;
	enif_inspect_binary(env, argv[0], &nifBin);
    hh::MainLogic *foo = (hh::MainLogic *)(nifBin.data);

	return enif_make_badarg(env);
}

ERL_NIF_TERM make_failed(ErlNifEnv *env, const char *fmt, ...) {
	va_list va;
	va_start(va, fmt);
	char data[1024];
	vsnprintf(data, 1024, fmt, va);
	ERL_NIF_TERM strTerm = enif_make_string(env, data, ERL_NIF_LATIN1);
	return enif_make_tuple(env, 2, enif_make_atom(env, "failed"), strTerm);
}

ERL_NIF_TERM make_success(ErlNifEnv *env, ERL_NIF_TERM reply) {
	return enif_make_tuple(env, 2, enif_make_atom(env, "success"), reply);
}

static ERL_NIF_TERM game_start(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char errBuff[128];
	//获取游戏类型
	int gameType = -1;
	int brankerNumber = -1;
	unsigned int randSeed = 0;
	if (!enif_get_int(env, argv[0], &gameType)) {
		return make_failed(env, "get param gameType failed.");
	}

	if (!enif_get_int(env, argv[1], &brankerNumber)) {
		return make_failed(env, "get param brankerNumber failed.");
	}

	if (!enif_get_uint(env, argv[2], &randSeed)) {
		return make_failed(env, "get param randSeed failed.");
	}

	if (0 == gameType) {
		//晃晃
		ErlNifBinary nifBin;
		if (!enif_alloc_binary(sizeof(hh::MainLogic), &nifBin)) {
			return make_failed(env, "enif_alloc_binary failed, len=%u", sizeof(hh::MainLogic));
		}

		hh::MainLogic *foo = (hh::MainLogic *)(nifBin.data);
		hh::Init(foo, (int8)brankerNumber);
		return make_success(env, enif_make_binary(env, &nifBin));
	} else if (1 == gameType) {

	} else if (2 == gameType) {

	} else {

	}

	return make_failed(env, "error gameType[%d].", gameType);
}


static ErlNifFunc nif_funcs[] = {
	{"game_start", 3, game_start},
	{"test_func", 0, test_func}
};


ERL_NIF_INIT(mj_nif, nif_funcs, NULL, NULL, NULL, NULL);
