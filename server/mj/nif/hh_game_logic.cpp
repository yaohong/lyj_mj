#include "hh_game_logic.h"

namespace hh
{
    int MAX_TITLE_INDEX = 30;
    uint8 PAI_ARRAY[30] = {
        PAI( 1, 1 ), PAI( 1, 2 ), PAI( 1, 3 ), PAI( 1, 4 ), PAI( 1, 5 ), PAI( 1, 6 ), PAI( 1, 7 ), PAI( 1, 8 ), PAI( 1, 9 ),        //万
        PAI( 2, 1 ), PAI( 2, 2 ), PAI( 2, 3 ), PAI( 2, 4 ), PAI( 2, 5 ), PAI( 2, 6 ), PAI( 2, 7 ), PAI( 2, 8 ), PAI( 2, 9 ),        //条
        PAI( 3, 1 ), PAI( 3, 2 ), PAI( 3, 3 ), PAI( 3, 4 ), PAI( 3, 5 ), PAI( 3, 6 ), PAI( 3, 7 ), PAI( 3, 8 ), PAI( 3, 9 ),        //筒
        PAI( 5, 1 ), PAI( 5, 1 ), PAI( 5, 1 )  //中发白         
    };

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
        for (int i = 0; i < writeOffset; ++i)
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

        for (int i = 0; i < MAX_TITLE_INDEX; ++i)
        {
            std::map<uint8, int>::iterator p_it = verifyMap.find( logic->pool_[PAI_ARRAY[i]] );
            assert( p_it != verifyMap.end() );
            assert( p_it->second == 4 );
        }
    }

    void Sort( uint8 pai[], int8 count )
    {
        //万 条 筒 东 西 南 北 中 发 白

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
            printf( "seat_number[%d]:", i );
            for (int j = 0; j < seat.writeIndex_; j++)
            {
                std::string v = getPaiString( seat.pai_[i] );
                printf( "%s ", v.c_str() );
            }
            printf( "\n" );

            Sort( seat.pai_, seat.writeIndex_ );
        }
    }
}



#ifdef HH_EXE
int main( int argc, const char * argv[] )
{
    // insert code here...
    std::cout << "Hello, World!\n";
    hh::MainLogic logic;
    hh::Init( &logic, -1 );

    return 0;
}
#endif