#include "hh_game_logic.h"

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

        logic->currentSeatNumber_ = logic->bankerSeatNumber_;

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
		bankerSeat.operFlag_ = 0;
		if (common::IsGang(bankerSeat.pai_, bankerSeat.writeIndex_))
		{
			bankerSeat.operFlag_ |= OP_GANG;
		}

		bankerSeat.operFlag_ |= OP_CHU;
    }

	//更新操作
	void updateOper(MainLogic *logic, qp_uint8 operSeatNum, qp_uint8 cp)
	{
		for (qp_uint8 i = 0; i < 4; i++)
		{
			logic->seats_[i].operFlag_ = OP_NONE;
		}
		//查找能够胡的玩家

		//查找能够杠的玩家
		for (qp_uint8 i = 0; i < 4; i++)
		{
			if (i != operSeatNum)
			{
				Seat &tmpSeat = logic->seats_[i];
				if (common::IsGang1(tmpSeat.pai_, tmpSeat.writeIndex_, cp))
				{
					//可以杠
					tmpSeat.operFlag_ |= (OP_GANG | OP_GUO);
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
					tmpSeat.operFlag_ |= (OP_PENG | OP_GUO);
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
				nextSeat.operFlag_ |= (OP_CHI | OP_GUO);
			}
		}
	}

	bool pickingOperSeatNumber(MainLogic *logic, qp_int8 &operSeatNumber)
	{
		for (qp_int8 i = 0; i < 4; i++)
		{
			operSeatNumber = i;
			if (logic->seats_[i].operFlag_ & OP_GANG)
			{
				//可以杠
				return true;
			}
		}

		for (qp_int8 i = 0; i < 4; i++)
		{
			operSeatNumber = i;
			if (logic->seats_[i].operFlag_ & OP_PENG)
			{
				//可以碰
				return true;
			}
		}

		for (qp_int8 i = 0; i < 4; i++)
		{
			operSeatNumber = i;
			if (logic->seats_[i].operFlag_ & OP_CHI)
			{
				//可以吃
				return true;
			}
		}

		return false;
	}


	bool isEnd(MainLogic *logic)
	{
		return (logic->poolHeadReadIndex_ == 0 && logic->poolTailReadIndex_ == 0);
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

	void Oper(MainLogic *logic, qp_int8 operSeatNumber, qp_uint8 operType, qp_uint8 v1, qp_uint8 v2)
	{
		if (logic->currentSeatNumber_ != operSeatNumber)
		{
			//座位号不对
			return;
		}

		Seat &currentSeat = logic->seats_[operSeatNumber];
		if (!(operType & currentSeat.operFlag_))
		{
			//操作类型不对
			return;
		}

		logic->currentValue1_ = v1;
		logic->currentValue2_ = v2;

		switch (operType)
		{
			case OP_CHI:
			{

			}
				break;
			case OP_PENG:
				break;
			case OP_GANG:
				break;
			case OP_HU:
				break;
			case OP_CHU:
			{
				assert(v1 > 0);
				//更新其他玩家的操作
				updateOper(logic, operSeatNumber, v1);
				//选取一个能够做操作的玩家
				qp_int8 newOperSeat = -1;
				if (pickingOperSeatNumber(logic, newOperSeat))
				{
					//有人可以做操作,设置下一个操作的相关信息
					logic->nextSeatNumber_ = newOperSeat;
					logic->nextValue1_ = 0;
					logic->nextValue2_ = 0;
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
					logic->nextSeatNumber_ = ((operSeatNumber + 1) % 4);
					Seat &nextSeat = logic->seats_[logic->nextSeatNumber_];
					qp_uint8 newPai = getNewPaiByHead(logic);
					logic->nextValue1_ = newPai;
					logic->nextValue2_ = 0;
					//检测下一个玩家能够做的操作
					//杠？胡？
				}
			}
				break;
			case OP_GUO:
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



