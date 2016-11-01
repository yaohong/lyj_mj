#include "common.h"
#include <stdlib.h>
#include <stdio.h>

std::string getPaiString( uint8 p )
{
    uint8 type = p >> 4;
    uint8 value = p & 15;
    char buff[16] = { 0 };
    if (type == 1)
    {
        sprintf( buff, "%d-万.", value );
    }
    else if (type == 2)
    {
        sprintf( buff, "%d-条.", value );
    }
    else if (type == 3)
    {
        sprintf( buff, "%d-筒.", value );
    }
    else if (type == 5)
    {
        //中发白
        if (1 == value)
        {
            sprintf( buff, "红中." );
        }
        else if (2 == value)
        {
            sprintf( buff, "发财." );
        }
        else if (3 == value)
        {
            sprintf( buff, "白板." );
        }
        else
        {
            assert( false );
        }
    }
    else
    {
        assert( false );
    }
    
    
    return buff;
}