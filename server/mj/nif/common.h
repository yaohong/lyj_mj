#ifndef _common_h__
#define _common_h__
#include <string>
#include <assert.h>
#include <string.h>
#include <map>
#include <time.h>
#include <iostream>


//麻将类型
#define WAN 1 //(1-9)
#define TIAO 2  //(1-9)
#define TONG 3 //(1-9)
#define FENG 4  //(东(1) 南(2) 西(3) 北(4))
#define FA 5 //(中(1) 发(2) 白(3))

#define TYPE(p) (p >> 4)
#define VALUE(p) (p & 0x0f)
#define PAI(t, v) ((t) << 4 | v)

#define uint8 unsigned char 
#define int8 char
#define uint32 unsigned int


//能够进行的操作
#define OP_NONE 0				
#define OP_CHI 1 				//1 << 0
#define OP_PENG 2 				//1 << 1
#define OP_GANG 4				//1 << 2
#define OP_HU 8					//1 << 3
#define OP_CHU 16				//1 << 4

std::string getPaiString( uint8 p );

#endif