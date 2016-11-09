#ifndef _game_logic_h__
#define _game_logic_h__
#include "common.h"
extern "C"
{
	#include "erl_nif.h"
}
//最大麻将牌数
#define HH_MAX_COUNT 3 * 9 * 4 + 3*4
namespace hh
{
    struct Seat
    {
        uint8   chi_[4];				//吃的牌，存小数字
        uint8   peng_[4];				//碰的牌
        uint8   gang_[4];				//杠的牌
        uint8   pai_[14];				//手里的牌
        uint8   writeIndex_;            //写入的索引
        uint8   operFlag_;                 //能够进行的操作
    };

    struct MainLogic
    {
        uint8	pool_[HH_MAX_COUNT];        //牌池里的牌
        uint8	poolHeadReadIndex_;			//麻将池的头部读取索引
        uint8   poolTailReadIndex_;         //麻将池的尾部读取索引
        Seat	seats_[4];                  //四个座位
		int8    bankerSeatNumber_;          //庄家的座位号
		int8	currentSeatNumber_;         //当前操作的座位
        uint32	lastTime_;                  //开始操作的时间

    };

	void Init(MainLogic *logic, int8 bankerSeatNumber, uint32 randSeed);

	ERL_NIF_TERM GenerateReturnValue(ErlNifEnv *env, MainLogic *logic);
}


#endif
