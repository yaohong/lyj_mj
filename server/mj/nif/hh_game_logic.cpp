#include "hh_game_logic.h"

namespace hh
{


    void RandomPool( MainLogic *logic )
    {
        //初始化池里的牌 (万子 条子 筒子)
        uint8 writeOffset = 0;
        for (uint8 i = 0; i < 3; i++)
        {
            for (uint8 j = 0; j < 9; j++)
            {
                for (uint8 k = 0; k < 4; k++)
                {
                    logic->pool_[writeOffset++] = PAI( i + 1, j + 1 );
                }
            }
        }

        //插入中发白
        for (uint8 i = 0; i < 3; i++)
        {
            for (uint8 j = 0; j < 4; j++)
            {
                logic->pool_[writeOffset++] = PAI( FA, i + 1 );
            }
        }

        assert( HH_MAX_COUNT == writeOffset );

        //将牌乱序
        uint8 tmpRandRange = writeOffset;
		for (uint8 i = 0; i < writeOffset; ++i)
        {
            uint8 tmpIndex = rand() % tmpRandRange--;
            //和最大索引交换值
            uint8 tmpValue = logic->pool_[tmpIndex];
            logic->pool_[tmpIndex] = logic->pool_[tmpRandRange];
            logic->pool_[tmpRandRange] = tmpValue;
        }
    }

    void CrcPool( MainLogic *logic )
    {
        std::map<uint8, int> verifyMap;
        for (int i = 0; i < HH_MAX_COUNT; ++i)
        {
            std::map<uint8, int>::iterator p_it = verifyMap.find( logic->pool_[i] );
            if (p_it == verifyMap.end())
            {
                verifyMap.insert( std::pair<uint8, int>( logic->pool_[i], 1 ) );
            }
            else
            {
                p_it->second++;
            }
        }

        for (int i = 0; i < common::MAX_TITLE_INDEX; ++i)
        {
			std::map<uint8, int>::iterator p_it = verifyMap.find(logic->pool_[common::PAI_ARRAY[i]]);
			if (p_it != verifyMap.end())
			{
				assert(p_it->second == 4);
			}
        }
    }

    void Init( MainLogic *logic, int8 bankerSeatNumber )
    {
        memset( logic, 0, sizeof(MainLogic) );
        srand( (uint32)time( NULL ) );
        RandomPool( logic );
        CrcPool( logic );
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
        //发三轮四张
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 4; j++)
            {
                int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + j) % 4;
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
            int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + i) % 4;
            assert( tmpOperSeatNumer >= 0 && tmpOperSeatNumer < 4 );
            Seat &seat = logic->seats_[tmpOperSeatNumer];
            seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];
        }

        //给庄家在发一张
        Seat &seat = logic->seats_[logic->bankerSeatNumber_];
        seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];
        printf( "banker_seat_number=%d\n", logic->bankerSeatNumber_ );

        //给牌排序
        for (int i = 0; i < 4; i++)
        {
            Seat &seat = logic->seats_[i];
			common::Sort(seat.pai_, seat.writeIndex_);
            printf( "seat_number[%d]:", i );
            for (int j = 0; j < seat.writeIndex_; j++)
            {
                std::string v = common::getPaiString( seat.pai_[j] );
                printf( "%s ", v.c_str() );
            }
            printf( "\n" );
        }
    }
}



#ifdef HH_EXE
int main( int argc, const char * argv[] )
{
	//insert code here...
	hh::MainLogic logic;
	hh::Init(&logic, -1);
	//uint8 source[14] = {1,2,3,4,3,3,7,8,9,10,11,12,13,14};
	//uint8 remove[4] = { 3, 7, 1, 13 };
	//common::RemovePai(source, 14, remove, 3);
	//for (int8 i = 0; i < 14; i++)
	//{
	//	printf("%d ", source[i]);
	//}
    return 0;
}
#endif