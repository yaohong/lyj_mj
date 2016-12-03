#include "hh_game_logic.h"
#include <stdarg.h>
namespace hh
{
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

		//是否能胡
		if (common::IsHu(bankerSeat.pai_, bankerSeat.writeIndex_))
		{
			logic->nextOperFlag_ |= OP_HU;
		}

		logic->nextOperSeatNumber_ = logic->bankerSeatNumber_;
		logic->nextOperFlag_ |= OP_CHU;
		logic->nextOperValue1_ = 0;
		logic->nextOperValue2_ = 0;

		clearSpecialOper(logic);
		clearHuOper(logic);
    }

	void setError(MainLogic *logic, const char *fmt, ...)
	{
		//		assert(logic->errorFlag_ == 0);
		va_list va;
		va_start(va, fmt);
		qp_int32 rt = vsnprintf(logic->errorLogBuff_ + logic->errorLogLen, HH_LOG_LEN - logic->errorLogLen, fmt, va);
		va_end(va);
		logic->errorLogLen += rt;
		logic->errorFlag_ = 1;
	}


	void writeLog(MainLogic *logic, const char *fmt, ...)
	{
		va_list va;
		va_start(va, fmt);
		qp_int32 rt = vsnprintf(logic->errorLogBuff_ + logic->errorLogLen, HH_LOG_LEN - logic->errorLogLen, fmt, va);
		va_end(va);
		logic->errorLogLen += rt;
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
		clearSpecialOper(logic);

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

	void clearSpecialOper(MainLogic *logic)
	{
		//清理特殊操作
		logic->specialOperCount_ = 0;
		logic->specialOperIndex_ = 0;
		memset(logic->specialOperQueue_, 0, 3 * 2);
	}

	void generateHuOper(MainLogic *logic, qp_uint8 operSeatNum, qp_uint8 cp)
	{
		clearHuOper(logic);

		for (qp_uint8 i = 1; i < 4; i++)
		{
			qp_uint8 nextSeatNum = (operSeatNum + i) % 4;
			Seat &nextSeat = logic->seats_[nextSeatNum];
			writeLog(logic, "generateHuOper nextSeatNum=%d cp=%d\n", nextSeatNum, cp);
			if (common::IsTing(nextSeat.pai_, nextSeat.writeIndex_, cp))
			{
				logic->huOperQueue_[logic->huOperCount_++] = nextSeatNum;
			}
		}
	}

	bool pickingHuOper(MainLogic *logic, qp_int8 &operSeatNumber)
	{
		if (logic->huOperCount_ == logic->huOperIndex_)
		{
			return false;
		}
		operSeatNumber = logic->huOperQueue_[logic->huOperIndex_];
		logic->huOperIndex_++;
		return true;

	}

	void clearHuOper(MainLogic *logic)
	{
		logic->huOperCount_ = 0;
		logic->huOperIndex_ = 0;
		memset(logic->huOperQueue_, 0, 3);
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

	void removePeng(MainLogic *logic, qp_int8 seatNumber, qp_uint8 v)
	{
		assert(seatNumber >= 0 && seatNumber < 4);
		assert(v > 0);
		qp_int8 removeIndex = -1;
		Seat &seat = logic->seats_[seatNumber];
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (seat.peng_[i] == v)
			{
				seat.peng_[i] = 0;
				removeIndex = i;
				break;
			}
		}

		if (removeIndex != -1)
		{
			for (qp_uint8 i = removeIndex; i < 3; i++)
			{
				seat.peng_[i] = seat.peng_[i+1];
				seat.peng_[i + 1] = 0;
			}

			return;
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
			if (seat.peng_[i] == v)
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
			setError(logic, "logic->nextOperSeatNumber_=%d, operSeatNumber=%d\n", logic->nextOperSeatNumber_, operSeatNumber);
			return;
		}

		if (!(operType & logic->nextOperFlag_))
		{
			//操作类型不对
			setError(logic, "logic->nextOperFlag_=%d, operType=%d\n", logic->nextOperFlag_, operType);
			logic->errorFlag_ = 1;
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

				assert(logic->chuPaiSeatNumber_ != -1);
				assert(logic->chuPaiValue_ != 0);

				if (logic->nextOperValue1_ != v1)
				{
					//操作值1不对
					setError(logic, "seatNumber[%d] OP_CHI logic->nextOperValue1_=%d, v1=%d\n", operSeatNumber, logic->nextOperValue1_, v1);
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
						setError(logic, "seatNumber[%d] oper[chi] type=0, error paiValue=%d\n", operSeatNumber, paiValue);
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
						setError(logic, "seatNumber[%d] oper[chi]  type=1, error paiValue=%d\n", operSeatNumber, paiValue);
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
						setError(logic, "seatNumber[%d] oper[chi]  type=2, error paiValue=%d\n", operSeatNumber, paiValue);
						return;
					}
					tmpChiPai[0] = PAI(paiType, paiValue - 2);
					tmpChiPai[1] = PAI(paiType, paiValue - 1);
					chiSmallValue = tmpChiPai[0];
				}
				else 
				{
					setError(logic, "oper[chi] error chiType=%d\n", v2);
					return;
				}

				if (!common::CheckPai(operSeat.pai_, operSeat.writeIndex_, tmpChiPai, 2))
				{
					//牌不存在
					setError(logic, "seatNumber[%d] [chi] CheckPai[%d, %d] failed\n", operSeatNumber, tmpChiPai[0], tmpChiPai[1]);
					return;
				}

				//牌从手里移除
				common::RemovePai(operSeat.pai_, operSeat.writeIndex_, tmpChiPai, 2);
				assert(operSeat.writeIndex_ > 0);

				//logic->oldOperValueSeatNumber_ = logic->chuPaiSeatNumber_;
				//添加到吃里面
				addChi(logic, operSeatNumber, chiSmallValue);

				//////////////////////////////////////////////////////////////////////////////////////
				logic->nextOperSeatNumber_ = operSeatNumber;
				logic->nextOperFlag_ = 0;
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

				assert(logic->chuPaiSeatNumber_ != -1);
				assert(logic->chuPaiValue_ != 0);
				if (logic->nextOperValue1_ != v1)
				{
					//操作值1不对
					setError(logic, "seatNumber[%d] OP_PENG logic->nextOperValue1_=%d, v1=%d\n", operSeatNumber, logic->nextOperValue1_, v1);
					return;
				}

				//看牌的数量
				assert( common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1) >= 2);

				//牌从手里移除
				qp_uint8 removePai[2] = {v1, v1};
				common::RemovePai(operSeat.pai_, operSeat.writeIndex_, removePai, 2);
				assert(operSeat.writeIndex_ > 0);

				//添加到碰里面
				addPeng(logic, operSeatNumber, v1);

				/////////////////////////////////////////////////////////////////////////////////////////////////
				logic->nextOperSeatNumber_ = operSeatNumber;
				//看自己能做的操作 杠？出
				logic->nextOperFlag_ = 0;
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
				if (logic->nextOperValue1_ == 0)
				{
					//必须是暗杠或者补杠
					//检查特殊操作(在特殊操作下不能出现暗杠或者补杠)
					assert(logic->specialOperCount_ == 0);
					if (0 == v1)
					{
						//V1不能为0，必须为正确的牌
						setError(logic, "seatNumber[%d] not ming gang v1=0\n", operSeatNumber);
						return;
					}
					//检测暗杠或者补杠
					qp_uint8 count = common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1);
					if (count == 4)
					{
						//暗杠成功
						qp_uint8 removePai[4] = { v1, v1, v1, v1 };
						common::RemovePai(operSeat.pai_, operSeat.writeIndex_, removePai, 4);
						assert(operSeat.writeIndex_ > 0);
						addGang(logic, operSeatNumber, v1);
					}
					else 
					{
						//检查是否能补杠,补杠的这张牌必须是之前摸的牌
						assert(logic->nextOperValue2_ != 0);
						if (v1 != logic->nextOperValue2_)
						{
							setError(logic, "seatNumber[%d] bugang, pai error, logic->nextOperValue2_=%d, v2=%d\n", operSeatNumber, logic->nextOperValue2_, v1);
							return;
						}
						assert( 1 == common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1));
						assert(isBuGang(logic, operSeatNumber, v1));
						//从牌池里移除这张牌
						common::RemoveSinglePai(operSeat.pai_, operSeat.writeIndex_, v1);
						assert(operSeat.writeIndex_ > 0);
						//补杠成功，将碰的牌放到杠
						removePeng(logic, operSeatNumber, v1);
						addGang(logic, operSeatNumber, v1);
					}

				}
				else 
				{
					//明杠
					assert(logic->specialOperCount_ > 0);

					assert(logic->chuPaiSeatNumber_ != -1);
					assert(logic->chuPaiValue_ != 0);


					assert(logic->nextOperValue1_ != 0);
					if (logic->nextOperValue1_ != v1)
					{
						//明杠杠的牌必须和服务器指定的相等
						setError(logic, "seatNumber[%d] minggang logic->nextOperValue1_=%d v1=%d\n", operSeatNumber, logic->nextOperValue1_, v1);
						return;
					}
					qp_uint8 paiCount = common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1);
					if (paiCount < 3)
					{
						setError(logic, "seatNumber[%d] oper[ming gang] , paiValue=%d paiCount=%d\n", operSeatNumber, v1, paiCount);
						return;
					}
					qp_uint8 removePai[3] = { v1, v1, v1 };
					common::RemovePai(operSeat.pai_, operSeat.writeIndex_, removePai, 3);
					assert(operSeat.writeIndex_ > 0);
					addGang(logic, operSeatNumber, v1);
				}

				//屁股摸一张牌
				assert(!isEnd(logic));
				qp_uint8 newPai = getNewPaiByTail(logic);
				common::AddSinglePai(operSeat.pai_, operSeat.writeIndex_, newPai);
				assert(operSeat.writeIndex_ <= 14);

				logic->nextOperFlag_ = 0;
				if (common::IsGang(operSeat.pai_, operSeat.writeIndex_))
				{
					logic->nextOperFlag_ |= OP_GANG;
				}
				else 
				{
					//看是否能够补杠
					if (isBuGang(logic, operSeatNumber, newPai))
					{
						logic->nextOperFlag_ |= OP_GANG;
					}
				}

				if (common::IsHu(operSeat.pai_, operSeat.writeIndex_))
				{
					logic->nextOperFlag_ |= OP_HU;
				}

				logic->nextOperFlag_ |= OP_CHU;
				logic->nextOperValue1_ = 0;
				logic->nextOperValue2_ = newPai;					//摸的牌放在V2

				clearSpecialOper(logic);
				////////////////////////////////////////////////////////////////////////////////
			}
				break;
			case OP_GUO:
			{
				assert(logic->chuPaiSeatNumber_ != -1);
				assert(logic->chuPaiValue_ != 0);
				//可能是胡牌的过也可能是特殊操作的过
				if (logic->huOperCount_ > 0)
				{
					//胡牌的过
					qp_int8 nextHuSeatNumber = -1;
					if (pickingHuOper(logic, nextHuSeatNumber))
					{
						logic->nextOperSeatNumber_ = nextHuSeatNumber;			
						logic->nextOperFlag_ = OP_HU | OP_GUO;
						logic->nextOperValue1_ = logic->chuPaiValue_;
						logic->nextOperValue2_ = 0;
						return;
					}
					clearHuOper(logic);
					if (isEnd(logic)) {
						//结束了
						logic->stateFlag_ = 1;
						return;
					}
					//没有人选择胡牌
					//那么构建特殊操作
					generateSpecialOper(logic, logic->chuPaiSeatNumber_, logic->chuPaiValue_);
				}
				//特殊操作的过
				//assert(logic->specialOperCount_ > 0);

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
					//肯定还有牌
					assert(!isEnd(logic));

					//没有人可以操作,下家摸一张牌
					
					logic->nextOperSeatNumber_ = ((logic->chuPaiSeatNumber_ + 1) % 4);
					
					Seat &nextSeat = logic->seats_[logic->nextOperSeatNumber_];
					qp_uint8 newPai = getNewPaiByHead(logic);
					common::AddSinglePai(nextSeat.pai_, nextSeat.writeIndex_, newPai);
					assert(nextSeat.writeIndex_ <= 14);
					logic->nextOperFlag_ = 0;
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

					if (common::IsHu(nextSeat.pai_, nextSeat.writeIndex_))
					{
						logic->nextOperFlag_ |= OP_HU;
					}

					logic->nextOperFlag_ |= OP_CHU;
					logic->nextOperValue1_ = 0;						//补杠或者暗杠不指定杠的牌
					logic->nextOperValue2_ = newPai;				//摸的牌放在v2

					clearSpecialOper(logic);
				}
			}
				break;
			case OP_HU:
			{
				//吃胡或者自摸胡
				if (logic->nextOperValue1_ == 0)
				{
					//自摸胡
					assert(operSeat.writeIndex_ % 3 == 2);
					assert(common::IsHu(operSeat.pai_, operSeat.writeIndex_));
					assert(logic->nextOperValue2_ != 0);

					logic->hupaiSeatNumber_ = operSeatNumber;
					logic->hupaiValue_ = logic->nextOperValue2_;
					logic->hupaiType_ = 0;
					logic->fangpaoSeatNumber_ = -1;

					logic->stateFlag_ = 2;
				}
				else 
				{
					//吃胡
					if (logic->nextOperValue1_ != v1)
					{
						setError(logic, "seatNumber[%d] oper[hu] , nextOperValue1_=%d v1=%d\n", operSeatNumber, logic->nextOperValue1_, v1);
						return;
					}
					//手牌肯定是3n+1
					assert(operSeat.writeIndex_ % 3 == 1);
					assert(common::IsTing(operSeat.pai_, operSeat.writeIndex_, v1));
					assert(logic->chuPaiSeatNumber_ != -1);
					common::AddSinglePai(operSeat.pai_, operSeat.writeIndex_, v1);

					logic->hupaiSeatNumber_ = operSeatNumber;
					logic->hupaiValue_ = logic->nextOperValue1_;
					logic->hupaiType_ = 1;
					logic->fangpaoSeatNumber_ = logic->chuPaiSeatNumber_;

					logic->stateFlag_ = 2;
				}
			}
				break;
			case OP_CHU:
			{
				//assert(-1 == logic->chuPaiSeatNumber_);
				if (common::GetPaiCount(operSeat.pai_, operSeat.writeIndex_, v1) == 0)
				{
					//自己没有这张牌
					setError(logic, "seatNumber[%d] not pai[%d]\n", operSeatNumber, v1);
					return;
				}
				//把这张牌从手牌里拿掉
				common::RemoveSinglePai(operSeat.pai_, operSeat.writeIndex_, v1);
				assert(operSeat.writeIndex_ > 0);

				//保存出牌的座位号（所有特殊操作的玩家都选过了以后，让出牌的下一个玩家摸牌）
				logic->chuPaiSeatNumber_ = operSeatNumber;
				logic->chuPaiValue_ = v1;
				//看有没谁能胡牌
				//
				generateHuOper(logic, operSeatNumber, v1);
				qp_int8 huSeatNumber = -1;
				if (pickingHuOper(logic, huSeatNumber))
				{
					logic->nextOperSeatNumber_ = huSeatNumber;
					logic->nextOperFlag_ = OP_HU | OP_GUO;
					logic->nextOperValue1_ = v1;
					logic->nextOperValue2_ = 0;
					return;
				}
				//clearHuOper(logic);

				//没有人可以胡牌
				//看还能不能摸牌
				if (isEnd(logic))
				{
					//游戏穿掉了
					logic->stateFlag_ = 1;
					return;
				}

				//////////////////////////////////////////////////////////////////////////////
				//另外三家看谁能[杠 碰 吃] 
				generateSpecialOper(logic, operSeatNumber, v1);
				//选取一个能够做操作的玩家
				qp_int8 newOperSeatNumber = -1;
				qp_uint8 newOperFlag = 0;
				if (pickingSpecialOper(logic, newOperSeatNumber, newOperFlag))
				{
					//有人可以做操作,设置下一个操作的相关信息(给erlang用的)
					logic->nextOperSeatNumber_ = newOperSeatNumber;			//下一个操作的座位号
					logic->nextOperFlag_ = newOperFlag;					//下一个能够做的操作类型 gang peng chi
					logic->nextOperValue1_ = v1;						//下一个操作的值 操作对应的牌，也就是上家出的那张牌(用来存放明杠,吃，碰的牌)
					logic->nextOperValue2_ = 0;							//为0
				}
				else 
				{
					//没有人可以做操作,下家摸一张牌
					logic->nextOperSeatNumber_ = ((operSeatNumber + 1) % 4);
					
					Seat &nextSeat = logic->seats_[logic->nextOperSeatNumber_];
					qp_uint8 newPai = getNewPaiByHead(logic);
					common::AddSinglePai(nextSeat.pai_, nextSeat.writeIndex_, newPai);
					assert(nextSeat.writeIndex_ <= 14);
					logic->nextOperFlag_ = 0;
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

					if (common::IsHu(nextSeat.pai_, nextSeat.writeIndex_))
					{
						logic->nextOperFlag_ |= OP_HU;
					}

					logic->nextOperFlag_ |= OP_CHU;
					logic->nextOperValue1_ = 0;					//暗杠和补杠不指定杠的牌
					logic->nextOperValue2_ = newPai;			//新摸的牌放在V2
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
//	int a[2][2];
//	a[0][0] = 2;
//	a[0][1] = 3;
//	a[1][0] = 9;
//	a[1][1] = 11;
//
//
//	int v1 = *(int *)a;
//	int v2 = *((int *)a + 1);
//	int v3 = *((int *)a + 2);
//	int v4 = *((int *)a + 3);
//	insert code here...
	//hh::MainLogic logic;
	//hh::Init(&logic, -1, (qp_uint32)time(NULL));
	//printf("banker_seat_number=%d\n", logic.bankerSeatNumber_);
	//for (int i = 0; i < 4; i++)
	//{
	//	hh::Seat &seat = logic.seats_[i];
	//	printf("seat_number[%d]:", i);
	//	for (int j = 0; j < seat.writeIndex_; j++)
	//	{
	//		std::string v = common::getPaiString(seat.pai_[j]);
	//		printf("%s ", v.c_str());
	//	}
	//	printf("\n");
	//	printf("chi:   ");
	//	for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
	//	{
	//		qp_uint8 chi[3][2];
	//		qp_uint8 chiCount = 0;
	//		common::GetChi(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k], chi, chiCount);
	//		if (chiCount > 0)
	//		{
	//			std::string v = common::getPaiString(common::PAI_ARRAY[k]);
	//			printf("%s ", v.c_str());
	//		}
	//	}
	//	printf("\n");

	//	printf("peng:   ");
	//	for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
	//	{
	//		if (common::IsPeng(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k]))
	//		{
	//			std::string v = common::getPaiString(common::PAI_ARRAY[k]);
	//			printf("%s ", v.c_str());
	//		}
	//	}
	//	printf("\n");

	//	printf("gang:   ");
	//	for (qp_uint8 k = 0; k < common::MAX_TITLE_INDEX; k++)
	//	{
	//		if (common::IsGang1(seat.pai_, seat.writeIndex_, common::PAI_ARRAY[k]))
	//		{
	//			std::string v = common::getPaiString(common::PAI_ARRAY[k]);
	//			printf("%s ", v.c_str());
	//		}
	//	}
	//	printf("\n");
	//}
	////qp_uint8 dest[16] = {1,2,3,4,15,5,5,6,7,8,9,10,11,13,14};
	////qp_uint8 source[5] = { 4, 15, 5, 5,14};
	////bool ret = common::CheckPai(source, 5, dest, 14);
	printf("size=%u\n", sizeof(hh::MainLogic));
	//qp_uint8 pai[14] = { 18, 18, 18, 18, 19, 19, 19, 19, 20,20,20,20, 21,21 };
	////qp_uint8 pai[14] = { 18, 18, 19, 19, 20, 20, 21, 21, 22,22,22,23,23,23};
	////qp_uint8 pai[14] = { 18, 18 };
	////qp_uint8 pai[14] = { 18, 18, 18, 19, 20, 21, 22, 23, 23, 23, 22, 23, 23, 23 };
	//std::vector<common::HuBasicResult> results;
	//common::GetBasicHuPai(pai, 14, results);
	//std::vector<common::HuBasicResult>::iterator p_it = results.begin();
	//for (; p_it != results.end(); ++p_it)
	//{
	//	common::HuBasicResult &result = (*p_it);
	//	printf("pair: %s-%s\n", common::getPaiString(result.pair_[0]).c_str(), common::getPaiString(result.pair_[1]).c_str());
	//	printf("sequence: ");
	//	for (qp_uint8 i = 0; i < result.sequenceLen_; i++)
	//	{
	//		qp_uint8 p0 = result.sequence_[i][0];
	//		qp_uint8 p1 = result.sequence_[i][1];
	//		qp_uint8 p2 = result.sequence_[i][2];
	//		printf("%s-%s-%s ", common::getPaiString(p0).c_str(), common::getPaiString(p1).c_str(), common::getPaiString(p2).c_str());
	//	}
	//	printf("\n");
	//}
	//qp_uint8 pai[13] = {17,17,17, 18, 19, 20, 21, 22, 23, 24, 25, 25, 25 };
	//qp_uint8 pai[10] = { 18, 18, 18, 19, 20, 21, 22, 23, 23, 23 };
	qp_uint8 pai[4] = { 21, 22, 34, 34};
	printf("ting: \n");
	for (qp_uint8 i = 0; i < common::MAX_TITLE_INDEX; i++)
	{
		if (common::IsTing(pai, 4, common::PAI_ARRAY[i]))
		{
			printf("%s ", common::getPaiString(common::PAI_ARRAY[i]).c_str());
		}
	}
	printf("\n");
    return 0;
}
#endif



