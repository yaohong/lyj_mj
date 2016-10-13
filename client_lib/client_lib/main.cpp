// client_lib.cpp : 定义控制台应用程序的入口点。
//
#include "client_lib.h"

int main( int argc, char* argv[] )
{
    client_lib::Socket socket_;
    bool nRet = socket_.Connect("192.168.1.181", 4576, 1000);
    return 0;
}

