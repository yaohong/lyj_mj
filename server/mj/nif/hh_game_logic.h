#ifndef _game_logic_h__
#define _game_logic_h__
#include "common.h"
//最大麻将牌数
#define HH_MAX_COUNT 3 * 9 * 4 + 3*4
namespace hh
{
    struct Seat
    {
		qp_uint8   chi_[4];				//吃的牌，存小数字
		qp_uint8   peng_[4];				//碰的牌
		qp_uint8   gang_[4];				//杠的牌
		qp_uint8   pai_[14];				//手里的牌
		qp_uint8   writeIndex_;            //写入的索引
		qp_uint8   operFlag_;                 //能够进行的操作
    };

    struct MainLogic
    {
		qp_uint8	pool_[HH_MAX_COUNT];        //牌池里的牌
		qp_uint8	poolHeadReadIndex_;			//麻将池的头部读取索引
		qp_uint8	poolTailReadIndex_;         //麻将池的尾部读取索引
        Seat		seats_[4];                  //四个座位
		qp_int8		bankerSeatNumber_;          //庄家的座位号
		qp_int8		currentSeatNumber_;         //当前操作的座位
		qp_uint32	lastTime_;                  //开始操作的时间

    };

	void Init(MainLogic *logic, qp_int8 bankerSeatNumber, qp_uint32 randSeed);
	void Oper(MainLogic *logic, qp_int8 bankerSeatNumber, qp_uint8 v1, qp_uint8 v2);
}


#endif
