#include "hh_game_logic.h"
#include <stdarg.h>
namespace hh
{
	void writeLog(MainLogic *logic, const char *fmt, ...)
	{
		va_list va;
		va_start(va, fmt);
		char data[1024];
		qp_int32 rt = vsnprintf(data, 1024, fmt, va);
		va_end(va);

		if (logic->logWriteOffset_ + rt >= HH_LOG_LEN)
		{
			//满了
			return;
		}
		memcpy(logic->logBuff_ + logic->logWriteOffset_, data, rt);
		logic->logWriteOffset_ += rt;
	}
    void InitPool( MainLogic *logic )
    {
        //初始化池里的牌 (万子 条子 筒子)
		qp_uint8 writeOffset = 0;
		for (qp_uint8 i = 0; i < 3; i++)
        {
			for (qp_uint8 j = 0; j < 9; j++)
            {
				for (qp_uint8 k = 0; k < 4; k++)
                {
                    logic->pool_[writeOffset++] = PAI( i + 1, j + 1 );
                }
            }
        }

        //插入中发白
		for (qp_uint8 i = 0; i < 3; i++)
        {
			for (qp_uint8 j = 0; j < 4; j++)
            {
                logic->pool_[writeOffset++] = PAI( FA, i + 1 );
            }
        }

        assert( HH_MAX_COUNT == writeOffset );
    }

	void Init(MainLogic *logic, qp_int8 bankerSeatNumber, qp_uint32 randSeed)
    {
        memset( logic, 0, sizeof(MainLogic) );
		srand(randSeed);
		InitPool(logic);
		common::Random(logic->pool_, HH_MAX_COUNT);
		common::Crc(logic->pool_, HH_MAX_COUNT);
		logic->chuPaiSeatNumber_ = -1;
        //随即一个庄家
        logic->poolHeadReadIndex_ = 0;
        logic->poolTailReadIndex_ = HH_MAX_COUNT - 1;
        if (-1 == bankerSeatNumber)
        {
            logic->bankerSeatNumber_ = rand() % 4;
        }
        else
        {
			assert(bankerSeatNumber >= 0 && bankerSeatNumber <= 3);
            logic->bankerSeatNumber_ = bankerSeatNumber;
        }

        logic->nextOperSeatNumber_ = logic->bankerSeatNumber_;

        //发三轮四张
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
				qp_int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + j) % 4;
                assert( tmpOperSeatNumer >= 0 && tmpOperSeatNumer < 4 );
                Seat &seat = logic->seats_[tmpOperSeatNumer];
                for (int k = 0; k < 4; k++)
                {
                    seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];
                }
            }
        }

        //发一轮一张
        for (int i = 0; i < 4; i++)
        {
			qp_int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + i) % 4;
            assert( tmpOperSeatNumer >= 0 && tmpOperSeatNumer < 4 );
            Seat &seat = logic->seats_[tmpOperSeatNumer];
            seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];
        }

        //给庄家在发一张
        Seat &seat = logic->seats_[logic->bankerSeatNumber_];
		seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];

        //给牌排序
        for (int i = 0; i < 4; i++)
        {
            Seat &seat = logic->seats_[i];
			common::Sort(seat.pai_, seat.writeIndex_);
        }

		//看庄家能够做的操作，杠
		Seat &bankerSeat = logic->seats_[logic->bankerSeatNumber_];
		if (common::IsGang(bankerSeat.pai_, bankerSeat.writeIndex_))
		{
			logic->nextOperFlag_ |= OP_GANG;
		}

		logic->nextOperFlag_ |= OP_CHU;
		logic->nextOperValue1_ = 0;
		logic->nextOperValue2_ = 0;

		clearSpecialOper(logic);
    }

	bool isEnd(MainLogic *logic)
	{
		return (logic->poolHeadReadIndex_ == 0 && logic->poolTailReadIndex_ == 0);
	}

	void addSpecialOper(MainLogic *logic, qp_uint8 seatNum, qp_uint8 operType)
	{
		assert(logic->specialOperCount_ < 3);
		for (qp_uint8 i = 0; i < logic->specialOperCount_; i++)
		{
			if (logic->specialOperQueue_[i][0] == seatNum)
			{
				logic->specialOperQueue_[i][1] |= operType;
				return;
			}
		}

		//没有找到则添加到末尾
		logic->specialOperQueue_[logic->specialOperCount_][0] = seatNum;
		logic->specialOperQueue_[logic->specialOperCount_][1] = operType;
		logic->specialOperCount_++;
		
	}

	//杠吃碰 (不考虑暗杠)
	void generateSpecialOper(MainLogic *logic, qp_uint8 operSeatNum, qp_uint8 cp)
	{
		logic->specialOperCount_ = 0;
		logic->specialOperIndex_ = 0;
		memset(logic->specialOperQueue_, 0, 3 * 2);

		if (isEnd(logic))
		{
			//没有牌摸了
			return;
		}
		//查找能够杠牌的玩家
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (i != operSeatNum)
			{
				Seat &tmpSeat = logic->seats_[i];
				if (common::IsGang1(tmpSeat.pai_, tmpSeat.writeIndex_, cp))
				{
					//可以杠
					addSpecialOper(logic, i, OP_GANG | OP_GUO);
					//只有一个玩家能杠
					break;
				}
			}
		}

		//查找能够碰的玩家
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (i != operSeatNum)
			{
				Seat &tmpSeat = logic->seats_[i];
				if (common::IsPeng(tmpSeat.pai_, tmpSeat.writeIndex_, cp))
				{
					//可以杠
					addSpecialOper(logic, i, OP_PENG | OP_GUO);
					//只有一个玩家能碰
					break;
				}
			}
		}

		//看我下家是否能吃
		{
			qp_uint8 nextSeatNumber = (operSeatNum + 1) % 4;
			Seat &nextSeat = logic->seats_[nextSeatNumber];
			if (common::IsChi(nextSeat.pai_, nextSeat.writeIndex_, cp))
			{
				addSpecialOper(logic, nextSeatNumber, OP_CHI | OP_GUO);
			}
		}

	}

	void clearSpecialOper(MainLogic *logic)
	{
		//清理特殊操作
		logic->specialOperCount_ = 0;
		logic->specialOperIndex_ = 0;
		memset(logic->specialOperQueue_, 0, 3 * 2);
		logic->chuPaiSeatNumber_ = -1;
		logic->chuPaiValue_ = 0;
	}

	bool pickingSpecialOper(MainLogic *logic, qp_int8 &operSeatNumber, qp_uint8 &operType)
	{
		if (logic->specialOperCount_ == logic->specialOperIndex_) 
		{
			return false;
		}

		operSeatNumber = logic->specialOperQueue_[logic->specialOperIndex_][0];
		operType = logic->specialOperQueue_[logic->specialOperIndex_][1];
		logic->specialOperIndex_++;
		return true;
	}

	qp_uint8 getNewPaiByHead(MainLogic *logic)
	{
		assert(!isEnd(logic));
		qp_uint8 index = logic->poolHeadReadIndex_;
		if (logic->poolHeadReadIndex_ == logic->poolTailReadIndex_)
		{
			//最后一张了
			logic->poolHeadReadIndex_ = 0;
			logic->poolTailReadIndex_ = 0;
		}
		else {
			logic->poolHeadReadIndex_ ++;
		}

		return logic->pool_[index];
	}

	qp_uint8 getNewPaiByTail(MainLogic *logic)
	{
		assert(!isEnd(logic));
		qp_uint8 index = logic->poolTailReadIndex_;
		if (logic->poolHeadReadIndex_ == logic->poolTailReadIndex_)
		{
			//最后一张了
			logic->poolHeadReadIndex_ = 0;
			logic->poolTailReadIndex_ = 0;
		}
		else {
			logic->poolTailReadIndex_--;
		}

		return logic->pool_[index];
	}


	void addChi(MainLogic *logic, qp_int8 seatNumber, qp_uint8 v)
	{
		assert(seatNumber >= 0 && seatNumber < 4);
		assert(v > 0);
		Seat &seat = logic->seats_[seatNumber];
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (seat.chi_[i] == 0)
			{
				seat.chi_[i] = v;
				return;
			}
		}

		assert(false);
	}

	void addPeng(MainLogic *logic, qp_int8 seatNumber, qp_uint8 v)
	{
		assert(seatNumber >= 0 && seatNumber < 4);
		assert(v > 0);
		Seat &seat = logic->seats_[seatNumber];
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (seat.peng_[i] == 0)
			{
				seat.peng_[i] = v;
				return;
			}
		}

		assert(false);
	}

	void addGang(MainLogic *logic, qp_int8 seatNumber, qp_uint8 v)
	{
		assert(seatNumber >= 0 && seatNumber < 4);
		assert(v > 0);
		Seat &seat = logic->seats_[seatNumber];
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (seat.gang_[i] == 0)
			{
				seat.gang_[i] = v;
				return;
			}
		}

		assert(false);
	}


	bool isBuGang(MainLogic *logic, qp_int8 seatNumber, qp_uint8 v)
	{
		assert(seatNumber >= 0 && seatNumber < 4);
		assert(v > 0);
		Seat &seat = logic->seats_[seatNumber];
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (seat.gang_[i] == v)
			{
				return true;
			}
		}

		return false;
	}

	void Oper(MainLogic *logic, qp_int8 operSeatNumber, qp_uint8 operType, qp_uint8 v1, qp_uint8 v2)
	{
		if (logic->nextOperSeatNumber_ != operSeatNumber)
		{
			//座位号不对
			writeLog(logic, "logic->nextOperSeatNumber_=%d, operSeatNumber=%d\n", logic->nextOperSeatNumber_, operSeatNumber);
			return;
		}

		if (!(operType & logic->nextOperFlag_))
		{
			//操作类型不对
			writeLog(logic, "logic->nextOperFlag_=%d, operType=%d\n", logic->nextOperFlag_, operType);
			return;
		}

		logic->oldOperSeatNumber_ = operSeatNumber;
		logic->oldOperFlag_ = logic->nextOperFlag_;
		logic->oldOperType_ = operType;
		logic->oldOperValue1_ = v1;
		logic->oldOperValue2_ = v2;

		Seat &operSeat = logic->seats_[operSeatNumber];

		switch (operType)
		{
			case OP_CHI:
			{
				//特殊操作
				assert(logic->specialOperCount_ > 0);
				if (logic->nextOperValue1_ != v1)
				{
					//操作值1不对
					writeLog(logic, "OP_CHI logic->nextOperValue1_=%d, v1=%d\n", logic->nextOperValue1_, v1);
					return;
				}

				//风和中发白的检查
				qp_uint8 paiType = TYPE(v1);
				qp_uint8 paiValue = VALUE(v1);
				assert(paiType >= 1 && paiType < 4);
				assert(paiValue >= 1 && paiValue <= 9);

				qp_uint8 tmpChiPai[2] = { 0, 0 };
				qp_uint8 chiSmallValue = 0;
				if (0 == v2)
				{
					//左chi
					if (paiValue > 7)
					{
						//牌值异常，不能左吃
						writeLog(logic, "oper[chi] type=0, error paiValue=%d\n", paiValue);
						return;
					}
					tmpChiPai[0] = PAI(paiType, paiValue + 1);
					tmpChiPai[1] = PAI(paiType, paiValue + 2);
					chiSmallValue = v1;
				}
				else if (1 == v2) 
				{
					//中吃
					if (paiValue == 1 || paiValue == 9)
					{
						//牌值异常，不能中吃
						writeLog(logic, "oper[chi]  type=1, error paiValue=%d\n", paiValue);
						return;
					}
					tmpChiPai[0] = PAI(paiType, paiValue - 1);
					tmpChiPai[1] = PAI(paiType, paiValue + 1);
					chiSmallValue = tmpChiPai[0];
				}
				else if (2 == v2)
				{
					//右吃
					if (paiValue < 3 )
					{
						//牌值异常，不能左吃
						writeLog(logic, "oper[chi]  type=2, error paiValue=%d\n", paiValue);
						return;
					}
					tmpChiPai[0] = PAI(paiType, paiValue - 2);
					tmpChiPai[1] = PAI(paiType, paiValue - 1);
					chiSmallValue = tmpChiPai[0];
				}
				else 
				{
					writeLog(logic, "oper[chi] error chiType=%d\n", v2);
					return;
				}

				if (!common::CheckPai(operSeat.pai_, operSeat.writeIndex_, tmpChiPai, 2))
				{
					//牌不存在
					return;
				}

				//牌从手里移除
				common::RemovePai(operSeat.pai_, operSeat.writeIndex_, tmpChiPai, 2);
				assert(operSeat.writeIndex_ > 0);
				//添加到吃里面
				addChi(logic, operSeatNumber, chiSmallValue);

				//////////////////////////////////////////////////////////////////////////////////////
				logic->nextOperSeatNumber_ = operSeatNumber;
				//看自己能做的操作 杠？出
				if (common::IsGang(operSeat.pai_, operSeat.writeIndex_))
				{
					logic->nextOperFlag_ |= OP_GANG;
				}

				logic->nextOperFlag_ |= OP_CHU;
				logic->nextOperValue1_ = 0;
				logic->nextOperValue2_ = 0;

				clearSpecialOper(logic);
			}
				break;
			case OP_PENG:
			{
				//特殊操作
				assert(logic->specialOperCount_ > 0);
				if (logic->nextOperValue1_ != v1)
				{
					//操作值1不对
					writeLog(logic, "OP_PENG logic->nextOperValue1_=%d, v1=%d\n", logic->nextOperValue1_, v1);
					return;
				}

				//看牌的数量
				qp_uint8 paiCount = common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1);
				if (paiCount < 2)
				{
					writeLog(logic, "oper[peng]  type=2, paiValue=%d paiCount=%d\n", v1, paiCount);
					return;
				}

				//牌从手里移除
				qp_uint8 removePai[2] = {v1, v1};
				common::RemovePai(operSeat.pai_, operSeat.writeIndex_, removePai, 2);
				assert(operSeat.writeIndex_ > 0);
				//添加到吃里面
				addPeng(logic, operSeatNumber, v1);

				/////////////////////////////////////////////////////////////////////////////////////////////////
				logic->nextOperSeatNumber_ = operSeatNumber;
				//看自己能做的操作 杠？出
				if (common::IsGang(operSeat.pai_, operSeat.writeIndex_))
				{
					logic->nextOperFlag_ |= OP_GANG;
				}

				logic->nextOperFlag_ |= OP_CHU;
				logic->nextOperValue1_ = 0;
				logic->nextOperValue2_ = 0;

				clearSpecialOper(logic);
			}
				break;
			case OP_GANG:
			{
				//分明杠和(暗杠,补杠(从碰的对子里杠)		
				if (logic->nextOperValue1_ != v1)
				{
					//暗杠,补杠
					writeLog(logic, "minggang %d\n", v1);
				}
				else 
				{
					//特殊操作,明杠
					assert(logic->specialOperCount_ > 0);
					//看牌的数量
					qp_uint8 paiCount = common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1);
					if (paiCount < 3)
					{
						writeLog(logic, "oper[gang] , paiValue=%d paiCount=%d\n", v1, paiCount);
						return;
					}

					//牌从手里移除
					qp_uint8 removePai[3] = { v1, v1, v1};
					common::RemovePai(operSeat.pai_, operSeat.writeIndex_, removePai, 3);
					assert(operSeat.writeIndex_ > 0);
					//添加到杠里面
					addGang(logic, operSeatNumber, v1);
					//屁股摸一张牌
					assert(!isEnd(logic));
					qp_uint8 newPai = getNewPaiByTail(logic);
					common::AddSinglePai(operSeat.pai_, operSeat.writeIndex_, newPai);
					assert(operSeat.writeIndex_ <= 14);

					if (common::IsGang(operSeat.pai_, operSeat.writeIndex_))
					{
						logic->nextOperFlag_ |= OP_GANG;
					}
					else {
						//看是否能够补杠
						if (isBuGang(logic, operSeatNumber, newPai))
						{
							logic->nextOperFlag_ |= OP_GANG;
						}
					}
					logic->nextOperFlag_ |= OP_CHU;
					logic->nextOperValue1_ = newPai;
					logic->nextOperValue2_ = 0;

					clearSpecialOper(logic);
					////////////////////////////////////////////////////////////////////////////////
				}

			}
				break;
			case OP_GUO:
			{
				//特殊操作
				assert(logic->specialOperCount_ > 0);
				assert(logic->chuPaiSeatNumber_ != -1);
				assert(logic->chuPaiValue_ != 0);
				//获取下一个做操作的玩家
				qp_int8 newOperSeatNumber = -1;
				qp_uint8 newOperFlag = 0;
				if (pickingSpecialOper(logic, newOperSeatNumber, newOperFlag))
				{
					//有人可以做操作,设置下一个操作的相关信息(给erlang用的)
					logic->nextOperSeatNumber_ = newOperSeatNumber;			//下一个操作的座位号
					logic->nextOperFlag_ = newOperFlag;						//下一个能够做的操作类型 gang peng chi
					logic->nextOperValue1_ = logic->chuPaiValue_;			//下一个操作的值 操作对应的牌，也就是上家出的那张牌
					logic->nextOperValue2_ = 0;								//为0
				}
				else 
				{
					//所有人都点过了
					if (isEnd(logic))
					{
						//游戏穿掉了
						logic->stateFlag_ = 1;
						return;
					}

					//没有人可以操作,下家摸一张牌
					logic->nextOperSeatNumber_ = ((logic->chuPaiSeatNumber_ + 1) % 4);
					logic->nextOperFlag_ = 0;
					Seat &nextSeat = logic->seats_[logic->nextOperSeatNumber_];
					qp_uint8 newPai = getNewPaiByHead(logic);
					common::AddSinglePai(nextSeat.pai_, nextSeat.writeIndex_, newPai);
					assert(nextSeat.writeIndex_ <= 14);
					if (common::IsGang(nextSeat.pai_, nextSeat.writeIndex_))
					{
						logic->nextOperFlag_ |= OP_GANG;
					}
					else {
						//看是否能够补杠
						if (isBuGang(logic, logic->nextOperSeatNumber_, newPai))
						{
							logic->nextOperFlag_ |= OP_GANG;
						}
					}
					logic->nextOperFlag_ |= OP_CHU;
					logic->nextOperValue1_ = newPai;
					logic->nextOperValue2_ = 0;

					clearSpecialOper(logic);
				}
			}
				break;
			case OP_HU:
			{

			}
				break;
			case OP_CHU:
			{
				assert(-1 == logic->chuPaiSeatNumber_);
				if (common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1) == 0)
				{
					//自己没有这张牌
					writeLog(logic, "seatNumber[%d] not pai[%d]\n", operSeatNumber, v1);
					return;
				}
				//把这张牌从手牌里拿掉
				common::RemoveSinglePai(operSeat.pai_, operSeat.writeIndex_, v1);
				assert(operSeat.writeIndex_ > 0);

				//////////////////////////////////////////////////////////////////////////////
				//另外三家看谁能[杠 碰 吃] 
				generateSpecialOper(logic, operSeatNumber, v1);
				//选取一个能够做操作的玩家
				qp_int8 newOperSeat = -1;
				qp_uint8 newOperFlag = 0;
				if (pickingSpecialOper(logic, newOperSeat, newOperFlag))
				{
					//保存出牌的座位号（一轮都过了以后，让下一个玩家摸牌）
					logic->chuPaiSeatNumber_ = operSeatNumber;
					logic->chuPaiValue_ = v1;
					//有人可以做操作,设置下一个操作的相关信息(给erlang用的)
					logic->nextOperSeatNumber_ = newOperSeat;			//下一个操作的座位号
					logic->nextOperFlag_ = newOperFlag;					//下一个能够做的操作类型 gang peng chi
					logic->nextOperValue1_ = v1;						//下一个操作的值 操作对应的牌，也就是上家出的那张牌
					logic->nextOperValue2_ = 0;							//为0
				}
				else 
				{
					if (isEnd(logic))
					{
						//游戏穿掉了
						logic->stateFlag_ = 1;
						return;
					}
					
					//没有人可以操作,下家摸一张牌
					logic->nextOperSeatNumber_ = ((operSeatNumber + 1) % 4);
					logic->nextOperFlag_ = 0;
					Seat &nextSeat = logic->seats_[logic->nextOperSeatNumber_];
					qp_uint8 newPai = getNewPaiByHead(logic);
					common::AddSinglePai(nextSeat.pai_, nextSeat.writeIndex_, newPai);
					assert(nextSeat.writeIndex_ <= 14);
					if (common::IsGang(nextSeat.pai_, nextSeat.writeIndex_))
					{
						//可以暗杠
						logic->nextOperFlag_ |= OP_GANG;
					}
					else 
					{
						//看是否能够补杠
						if (isBuGang(logic, logic->nextOperSeatNumber_, newPai))
						{
							logic->nextOperFlag_ |= OP_GANG;
						}
					}

					

					logic->nextOperFlag_ |= OP_CHU;
					logic->nextOperValue1_ = newPai;
					logic->nextOperValue2_ = 0;

					clearSpecialOper(logic);
				}
			}
				break;
			default:
				break;
		}
	}
}



#ifdef HH_EXE
int main( int argc, const char * argv[] )
{
	//insert code here...
	hh::MainLogic logic;
	hh::Init(&logic, -1, (qp_uint32)time(NULL));
	printf("banker_seat_number=%d\n", logic.bankerSeatNumber_);
	for (int i = 0; i < 4; i++)
	{
		hh::Seat &seat = logic.seats_[i];
		printf("seat_number[%d]:", i);
		for (int j = 0; j < seat.writeIndex_; j++)
		{
			std::string v = common::getPaiString(seat.pai_[j]);
			printf("%s ", v.c_str());
		}
		printf("\n");
		printf("chi:   ");
		for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
		{
			qp_uint8 chi[3][2];
			qp_uint8 chiCount = 0;
			common::GetChi(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k], chi, chiCount);
			if (chiCount > 0)
			{
				std::string v = common::getPaiString(common::PAI_ARRAY[k]);
				printf("%s ", v.c_str());
			}
		}
		printf("\n");

		printf("peng:   ");
		for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
		{
			if (common::IsPeng(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k]))
			{
				std::string v = common::getPaiString(common::PAI_ARRAY[k]);
				printf("%s ", v.c_str());
			}
		}
		printf("\n");

		printf("gang:   ");
		for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
		{
			if (common::IsGang1(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k]))
			{
				std::string v = common::getPaiString(common::PAI_ARRAY[k]);
				printf("%s ", v.c_str());
			}
		}
		printf("\n");
	}
	//qp_uint8 dest[16] = {1,2,3,4,15,5,5,6,7,8,9,10,11,13,14};
	//qp_uint8 source[5] = { 4, 15, 5, 5,14};
	//bool ret = common::CheckPai(source, 5, dest, 14);
    return 0;
}
#endif



