#include "game_logic.h"
#include <string.h>
#include <map>
#include <time.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <iostream>
void RandomPool(MainLogic *logic)
{
    //初始化池里的牌
    uint8 writeOffset = 0;
    for (uint8 i=0;i<3; i++)
    {
        for (uint8 j=0; j<9; j++)
        {
            for (uint8 k=0; k<4; k++)
            {
                logic->pool_[writeOffset++] = PAI(i + 1, j + 1);
            }
        }
    }
    assert(MAX_COUNT == writeOffset);
    
    //将牌乱序
    uint8 tmpRandRange = writeOffset;
    for (int i=0; i<writeOffset; ++i)
    {
        uint8 tmpIndex = rand() % tmpRandRange--;
        //和最大索引交换值
        uint8 tmpValue = logic->pool_[tmpIndex];
        logic->pool_[tmpIndex] = logic->pool_[tmpRandRange];
        logic->pool_[tmpRandRange] = tmpValue;
    }
}

void CrcPool(MainLogic *logic)
{
    for (int i=0; i<MAX_COUNT; i++)
    {
        std::string v = getPaiString(logic->pool_[i]);
        printf("%s ", v.c_str());
    }
    
    std::map<uint8, int> verifyMap;
    for (int i=0; i<MAX_COUNT; ++i)
    {
        std::map<uint8, int>::iterator p_it = verifyMap.find(logic->pool_[i]);
        if (p_it == verifyMap.end())
        {
            verifyMap.insert(std::pair<uint8, int>(logic->pool_[i], 1));
        }
        else
        {
            p_it->second++;
        }
    }
    
    for (int i=0; i<MAX_COUNT; ++i)
    {
        std::map<uint8, int>::iterator p_it = verifyMap.find(logic->pool_[i]);
        assert(p_it != verifyMap.end());
        assert(p_it->second == 4);
    }
}

void Sort( uint8 pai[], int8 count )
{
    //万 条 筒 东 西 南 北 中 发 白

}

void Init(MainLogic *logic, int8 bankerSeatNumber)
{
	memset(logic, 0, sizeof(MainLogic));
    srand((uint32)time(NULL));
    RandomPool(logic);
    CrcPool(logic);
    //随即一个庄家
    logic->poolHeadReadIndex_ = 0;
    logic->poolTailReadIndex_ = MAX_COUNT -  1;
    if ( -1 == bankerSeatNumber )
    {
        logic->bankerSeatNumber_ = rand() % 4;
    } 
    else 
    {
        logic->bankerSeatNumber_ = bankerSeatNumber;
    }
    
    logic->currentSeatNumber_ = logic->bankerSeatNumber_;
    //发三轮四张
    for (int i=0; i<3; i++)
    {
        for (int j=0; j<4; j++)
        {
            int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + j) % 4;
            assert(tmpOperSeatNumer >= 0 && tmpOperSeatNumer < 4);
            Seat &seat = logic->seats_[tmpOperSeatNumer];
            for (int k=0; k<4; k++)
            {
                seat.pai_[seat.writeIndex_++] = logic->pool_[logic->poolHeadReadIndex_++];
            }
        }
    }
    
    //发一轮一张
    for (int i=0; i<4; i++)
    {
        int8 tmpOperSeatNumer = (logic->bankerSeatNumber_ + i) % 4;
        assert(tmpOperSeatNumer >= 0 && tmpOperSeatNumer < 4);
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
        Sort(seat.pai_, seat.writeIndex_);
    }
}

std::string getPaiString(uint8 p)
{
    uint8 type = p >> 4;
    uint8 value = p & 15;
    std::string strType;
    if (type == 1) {
        strType = "wan";
    } else if (type == 2){
        strType = "tiao";
    } else if (type == 3) {
        strType = "tong";
    } else {
        assert(false);
    }
    char buff[16] = {0};
    sprintf(buff, "%d-%s", value, strType.c_str());
    return buff;
}








#ifdef EXE
int main(int argc, const char * argv[]) {
    // insert code here...
    std::cout << "Hello, World!\n";
    MainLogic logic;
    Init(&logic, -1);
    
    return 0;
}
#endif