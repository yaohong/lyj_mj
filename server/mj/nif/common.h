#ifndef _common_h__
#define _common_h__
#include <string>
#include <assert.h>
#include <string.h>
#include <map>
#include <time.h>
#include <iostream>
#include <vector>

//麻将类型
#define WAN 1 //(1-9)
#define TIAO 2  //(1-9)
#define TONG 3 //(1-9)
#define FENG 4  //(东(1) 南(2) 西(3) 北(4))
#define FA 5 //(中(1) 发(2) 白(3))

#define TYPE(p) (p >> 4)
#define VALUE(p) (p & 0x0f)
#define PAI(t, v) ((t) << 4 | v)

#define qp_uint8 unsigned char 
#define qp_int8 char
#define qp_uint32 unsigned int
#define qp_int32 int

//能够进行的操作
#define OP_NONE 0				
#define OP_CHI 1 				//1 << 0
#define OP_PENG 2 				//1 << 1
#define OP_GANG 4				//1 << 2
#define OP_HU 8					//1 << 3
#define OP_CHU 16				//1 << 4
#define OP_GUO 32				//1 << 5
namespace common {
	static qp_uint8 MAX_TITLE_INDEX = 34;
	static qp_uint8 PAI_ARRAY[34] = {
		PAI(1, 1), PAI(1, 2), PAI(1, 3), PAI(1, 4), PAI(1, 5), PAI(1, 6), PAI(1, 7), PAI(1, 8), PAI(1, 9),        //万
		PAI(2, 1), PAI(2, 2), PAI(2, 3), PAI(2, 4), PAI(2, 5), PAI(2, 6), PAI(2, 7), PAI(2, 8), PAI(2, 9),        //条
		PAI(3, 1), PAI(3, 2), PAI(3, 3), PAI(3, 4), PAI(3, 5), PAI(3, 6), PAI(3, 7), PAI(3, 8), PAI(3, 9),        //筒
		PAI(4, 1), PAI(4, 2), PAI(4, 3), PAI(4, 4),        //东 南西北
		PAI(5, 1), PAI(5, 2), PAI(5, 3)  //中发白         
	};

	struct HuBasicResult {
		qp_uint8 sequence_[4][3];
		qp_uint8 sequenceLen_;
		qp_uint8 pair_[2];
		HuBasicResult()
		{
			for (qp_int8 i = 0; i < 4; i++)
			{
				for (qp_int8 j = 0; j < 3; j++)
				{
					sequence_[i][j] = 0;
				}
			}
			sequenceLen_ = 0;
			for (qp_int8 i = 0; i < 2; i++)
			{
				pair_[i] = 0;
			}
		}
	};
	std::string getPaiString(qp_uint8 p);
	//对牌进行排序
	void Sort(qp_uint8 pai[], qp_uint8 count);
	//随机牌
	void Random(qp_uint8 pai[], qp_uint8 count);
	//校验
	void Crc(qp_uint8 pai[], qp_uint8 count);
	//从source里删除dest的牌
	void RemoveSinglePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 removePai);
	void RemovePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 dest[], qp_uint8 destLen);
	//将牌添加到source
	void AddSinglePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 addPai);
	void AddPai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 addPai[], qp_uint8 addLen);
	//检测指定source的牌在dest里是否存在
	bool CheckPai(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 dest[], qp_uint8 destLen);
	//获取某张牌的数量
	qp_uint8 GetPaiCount(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 p);
	//获取吃的牌,chiCount大于0表示可以吃
	void GetChi(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p, qp_uint8 chi[3][2], qp_uint8 &chiCount);
	bool IsChi(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p);
	bool IsPeng(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p);
	bool IsGang1(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p);

	//获取我手里四张的数量
	void GetGang(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 gang[3], qp_uint8 &gangCount);
	bool IsGang(qp_uint8 pool[], qp_uint8 poolLen);

	void GetBasicHuPai(qp_uint8 source[], qp_uint8 sourceLen, std::vector<HuBasicResult> &results);
	bool CheckBasicHuPai(qp_uint8 source[], qp_uint8 sourceLen);
	bool IsHu(qp_uint8 source[], qp_uint8 sourceLen);


	bool IsTing(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 pai);
}

#endif