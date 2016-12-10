#ifndef _game_logic_h__
#define _game_logic_h__
#include "common.h"
//最大麻将牌数
#define HH_MAX_COUNT 120 //3 * 9 * 4 + 3*4
#define HH_LOG_LEN 255

#define HU_PI 1						//有吃碰明杠
#define HU_DAO 2					//没有吃碰明杠
#define HU_QI_DUI 3					//有七对牌 ,不能杠
#define HU_HAOHUA_QI_DUI 4			//有一个四个的七对
#define HU_CHAOHAOHUA_QI_DUI 5		//有二个四个的七对

namespace hh
{
	static qp_uint8 HH_PAI_ARRAY[HH_MAX_COUNT] = {
		PAI(1, 1), PAI(1, 2), PAI(1, 3), PAI(1, 4), PAI(1, 5), PAI(1, 6), PAI(1, 7), PAI(1, 8), PAI(1, 9),        //万
		PAI(1, 1), PAI(1, 2), PAI(1, 3), PAI(1, 4), PAI(1, 5), PAI(1, 6), PAI(1, 7), PAI(1, 8), PAI(1, 9),
		PAI(1, 1), PAI(1, 2), PAI(1, 3), PAI(1, 4), PAI(1, 5), PAI(1, 6), PAI(1, 7), PAI(1, 8), PAI(1, 9),
		PAI(1, 1), PAI(1, 2), PAI(1, 3), PAI(1, 4), PAI(1, 5), PAI(1, 6), PAI(1, 7), PAI(1, 8), PAI(1, 9),

		PAI(2, 1), PAI(2, 2), PAI(2, 3), PAI(2, 4), PAI(2, 5), PAI(2, 6), PAI(2, 7), PAI(2, 8), PAI(2, 9),        //条
		PAI(2, 1), PAI(2, 2), PAI(2, 3), PAI(2, 4), PAI(2, 5), PAI(2, 6), PAI(2, 7), PAI(2, 8), PAI(2, 9),
		PAI(2, 1), PAI(2, 2), PAI(2, 3), PAI(2, 4), PAI(2, 5), PAI(2, 6), PAI(2, 7), PAI(2, 8), PAI(2, 9),
		PAI(2, 1), PAI(2, 2), PAI(2, 3), PAI(2, 4), PAI(2, 5), PAI(2, 6), PAI(2, 7), PAI(2, 8), PAI(2, 9),

		PAI(3, 1), PAI(3, 2), PAI(3, 3), PAI(3, 4), PAI(3, 5), PAI(3, 6), PAI(3, 7), PAI(3, 8), PAI(3, 9),        //筒
		PAI(3, 1), PAI(3, 2), PAI(3, 3), PAI(3, 4), PAI(3, 5), PAI(3, 6), PAI(3, 7), PAI(3, 8), PAI(3, 9),
		PAI(3, 1), PAI(3, 2), PAI(3, 3), PAI(3, 4), PAI(3, 5), PAI(3, 6), PAI(3, 7), PAI(3, 8), PAI(3, 9),
		PAI(3, 1), PAI(3, 2), PAI(3, 3), PAI(3, 4), PAI(3, 5), PAI(3, 6), PAI(3, 7), PAI(3, 8), PAI(3, 9),

		PAI(5, 1), PAI(5, 2), PAI(5, 3),  //中发白   
		PAI(5, 1), PAI(5, 2), PAI(5, 3),
		PAI(5, 1), PAI(5, 2), PAI(5, 3),
		PAI(5, 1), PAI(5, 2), PAI(5, 3)
	};
	struct SeatGang
	{
		qp_uint8 pai_;
		qp_uint8 type_;
		qp_int8 seatNumber_;
	};

	struct SeatChi
	{
		qp_uint8 pai_;
		qp_uint8 type_;
	};

	struct SeatPeng
	{
		qp_uint8 pai_;
		qp_int8 seatNumber_;
	};

    struct Seat
    {
		SeatChi		chi_[4];					//吃的牌，存小数字 8
		SeatPeng	peng_[4];					//碰的牌	8
		SeatGang	gang_[4];					//杠的牌,杠的类型,如果是明杠则是放杠的座位号	12
		qp_uint8	pai_[14];					//手里的牌	14
		qp_uint8	writeIndex_;				//写入的索引(手牌数量) 1
    };

	struct SpecialOperItem
	{
		qp_int8 seatNumber_;
		qp_uint8 operType_;
	};

    struct MainLogic
    {
		qp_uint8	pool_[HH_MAX_COUNT];        //牌池里的牌
		qp_uint8	poolHeadReadIndex_;			//麻将池的头部读取索引
		qp_uint8	poolTailReadIndex_;         //麻将池的尾部读取索引
        Seat		seats_[4];                  //四个座位
		qp_int8		bankerSeatNumber_;          //庄家的座位号

		SpecialOperItem	specialOperQueue_[3];	//特殊操作队列 (吃，碰，杠)
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
		qp_uint8	errorFlag_;					//0游戏中，1表示出错了


		qp_int8		hupaiSeatNumber_;			//胡牌的座位号
		qp_uint8	hupaiValue_;				//胡牌的值
		qp_uint8	hupaiType_;					//胡牌类型(0：自摸，1：吃胡)
		qp_uint8	hupaiLevel_;				//胡牌的级别（屁胡，刀胡，七对，豪华七对，超豪华七对）
		qp_int8		fangpaoSeatNumber_;			//如果是吃胡则是放炮的座位号

		char		errorLogBuff_[HH_LOG_LEN];	//存放日志的buff
		qp_uint8	errorLogLen;				//日志的写入偏移

    };
	void clearSpecialOper(MainLogic *logic);
	void clearHuOper(MainLogic *logic);
	void Init(MainLogic *logic, qp_int8 bankerSeatNumber, qp_uint32 randSeed);
	void Oper(MainLogic *logic, qp_int8 operSeatNumber, qp_uint8 operType, qp_uint8 v1, qp_uint8 v2);
}


#endif
