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
            logic->bankerSeatNumber_ = bankerSeatNumber;
        }

        logic->currentSeatNumber_ = logic->bankerSeatNumber_;

		printf("bankerSeatNumber_=%d\n", logic->bankerSeatNumber_);
		printf("currentSeatNumber_=%d\n", logic->currentSeatNumber_);
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
		printf( "seat_number[%d]:", i );
		for (int j = 0; j < seat.writeIndex_; j++)
		{
			std::string v = common::getPaiString(seat.pai_[j]);
			printf("%s ", v.c_str());
		}
		printf( "\n" );
	}
    return 0;
}
#endif



