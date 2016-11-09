#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include <stdarg.h>
#include "erl_nif.h"
#include "hh_game_logic.h"

ERL_NIF_TERM MakeFailed(ErlNifEnv *env, const char *fmt, ...) {
	va_list va;
	va_start(va, fmt);
	char data[1024];
	vsnprintf(data, 1024, fmt, va);
	ERL_NIF_TERM strTerm = enif_make_string(env, data, ERL_NIF_LATIN1);
	return enif_make_tuple(env, 2, enif_make_atom(env, "failed"), strTerm);
}

ERL_NIF_TERM MakeSuccess1(ErlNifEnv *env, ERL_NIF_TERM reply) {
	return enif_make_tuple(env, 2, enif_make_atom(env, "success"), reply);
}

ERL_NIF_TERM MakeSuccess0(ErlNifEnv *env) {
	return enif_make_tuple(env, 1, enif_make_atom(env, "success"));
}


ERL_NIF_TERM generateReturnValue(ErlNifEnv *env, hh::MainLogic *logic) {

}


static ERL_NIF_TERM game_start(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	//获取游戏类型
	int gameType = -1;
	int brankerNumber = -1;
	unsigned int randSeed = 0;
	if (!enif_get_int(env, argv[0], &gameType)) {
		return MakeFailed(env, "get param gameType failed.");
	}

	if (!enif_get_int(env, argv[1], &brankerNumber)) {
		return MakeFailed(env, "get param brankerNumber failed.");
	}

	if (!enif_get_uint(env, argv[2], &randSeed)) {
		return MakeFailed(env, "get param randSeed failed.");
	}

	if (0 == gameType) {
		//晃晃
		ErlNifBinary nifBin;
		if (!enif_alloc_binary(sizeof(hh::MainLogic), &nifBin)) {
			return MakeFailed(env, "enif_alloc_binary failed, len=%u", sizeof(hh::MainLogic));
		}
		hh::MainLogic *logic = (hh::MainLogic *)(nifBin.data);
		hh::Init(logic, (int8)brankerNumber, randSeed);
		ERL_NIF_TERM returnValue = hh::GenerateReturnValue(env, foo);
		return MakeSuccess1(env, enif_make_tuple(env, 2, returnValue, enif_make_binary(env, &nifBin)));
	} else if (1 == gameType) {

	} else if (2 == gameType) {

	} else {

	}

	return MakeFailed(env, "error gameType %d.", gameType);
}

static ERL_NIF_TERM game_oper(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int gameType = -1;
	ErlNifBinary nifBin;
	if (!enif_get_int(env, argv[0], &gameType)) {
		return MakeFailed(env, "get param gameType failed.");
	}

	if (!enif_inspect_binary(env, argv[1], &nifBin)) {
		return MakeFailed(env, "get param nifBin failed.");
	}

	hh::MainLogic *foo = (hh::MainLogic *)(nifBin.data);

	if (0 == gameType) {
		//晃晃
		hh::MainLogic *foo = (hh::MainLogic *)(nifBin.data);
		return MakeSuccess0(env);
	} else if (1 == gameType) {

	} else if (2 == gameType) {

	} else {

	}

	return MakeFailed(env, "error gameType %d.", gameType);
}


static ErlNifFunc nif_funcs[] = {
	{ "game_start", 3, game_start },
	{ "game_oper", 2, game_oper }
};


ERL_NIF_INIT(mj_nif, nif_funcs, NULL, NULL, NULL, NULL);
