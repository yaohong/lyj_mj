#ifndef _game_logic_h__
#define _game_logic_h__
#include "common.h"
//最大麻将牌数
#define HH_MAX_COUNT 3 * 9 * 4 + 3*4
#define HH_LOG_LEN 255
namespace hh
{
    struct Seat
    {
		qp_uint8   chi_[4];					//吃的牌，存小数字
		qp_uint8   peng_[4];				//碰的牌
		qp_uint8   gang_[4];				//杠的牌
		qp_uint8   pai_[14];				//手里的牌
		qp_uint8   writeIndex_;				//写入的索引(手牌数量)
    };

    struct MainLogic
    {
		qp_uint8	pool_[HH_MAX_COUNT];        //牌池里的牌
		qp_uint8	poolHeadReadIndex_;			//麻将池的头部读取索引
		qp_uint8	poolTailReadIndex_;         //麻将池的尾部读取索引
        Seat		seats_[4];                  //四个座位
		qp_int8		bankerSeatNumber_;          //庄家的座位号

		qp_uint8	specialOperQueue_[3][2];	//特殊操作队列 (吃，碰，杠)
		qp_uint8	specialOperCount_;			//特殊操作队列元素的数量
		qp_uint8	specialOperIndex_;			//当前进行特殊操作的索引

		qp_uint8	huOperQueue_[3];			//胡牌的队列
		qp_uint8	huOperCount_;				//胡牌队列的数量
		qp_uint8	huOperIndex_;				//当前胡牌操作的索引
		
		qp_int8		chuPaiSeatNumber_;			//保存出牌的座位号
		qp_uint8	chuPaiValue_;				//出的牌

		qp_int8		oldOperSeatNumber_;			//之前操作的座位
		qp_uint8	oldOperFlag_;				//之前能够做的操作
		qp_uint8	oldOperType_;				//之前做的类型
		qp_uint8	oldOperValue1_;				//之前操作的值1
		qp_uint8	oldOperValue2_;				//之前操作的值2
		//qp_int8		oldOperValueSeatNumber_;	//存放chuPaiSeatNumber_的值，默认为-1供erlang调用

		qp_int8		nextOperSeatNumber_;		//下一个操作的座位号
		qp_uint8	nextOperFlag_;				//下一个能够做的操作
		qp_uint8	nextOperValue1_;			//下一个操作的值1
		qp_uint8	nextOperValue2_;			//下一个操作的值2



		qp_uint8    stateFlag_;					//状态标志(0为游戏中,大于0为结束原因）
		qp_uint8	errorFlag_;					//1表示出错了


		qp_int8		hupaiSeatNumber_;			//胡牌的座位号
		qp_uint8	hupaiValue_;				//胡牌的值
		qp_uint8	hupaiType_;					//胡牌类型(0：自摸，1：吃胡)
		qp_int8		fangpaoSeatNumber_;			//如果是吃胡则是放炮的座位号

		char		errorLogBuff_[HH_LOG_LEN];		//存放日志的buff
		qp_uint8	errorLogLen;			//日志的写入偏移

    };
	void clearSpecialOper(MainLogic *logic);
	void clearHuOper(MainLogic *logic);
	void Init(MainLogic *logic, qp_int8 bankerSeatNumber, qp_uint32 randSeed);
	void Oper(MainLogic *logic, qp_int8 operSeatNumber, qp_uint8 operType, qp_uint8 v1, qp_uint8 v2);
}


#endif
