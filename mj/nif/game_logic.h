#ifndef _game_logic_h__
#define _game_logic_h__
#include <string>
//麻将类型
#define WAN 1
#define TIAO 2
#define TONG 3

#define TYPE(p) (p >> 4)
#define VALUE(p) (p & 0x0f)
#define PAI(t, v) ((t) << 4 | v)

#define uint8 unsigned char 
#define int8 char
#define uint32 unsigned int

//最大麻将牌数
#define MAX_COUNT 3 * 4 * 9


//能够进行的操作
#define OP_NONE 0				
#define OP_CHI 1 				//1 << 0
#define OP_PENG 2 				//1 << 1
#define OP_GANG 4				//1 << 2
#define OP_HU 8					//1 << 3
#define OP_CHU 16				//1 << 4




struct Seat 
{
	uint8   chi_[4];				//吃的牌，存小数字
	uint8   peng_[4];				//碰的牌
	uint8   gang_[4];				//杠的牌
	uint8   pai_[14];				//手里的牌
    int8    writeIndex_;            //写入的索引
    uint8   opers_;                 //能够进行的操作
};

struct MainLogic 
{
	uint8	pool_[MAX_COUNT];           //牌池里的牌
	uint8	poolHeadReadIndex_;			//麻将池的头部读取索引
    uint8   poolTailReadIndex_;         //麻将池的尾部读取索引
	Seat	seats_[4];                  //四个座位
    int8    bankerSeatNumber_;          //庄家的座位号
	int8	currentSeatNumber_;         //当前操作的座位
	uint32	lastTime_;                  //开始操作的时间

};

void Init(MainLogic *logic);
std::string getPaiString(uint8 p);
#endif
