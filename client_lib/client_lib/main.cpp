// client_lib.cpp : 定义控制台应用程序的入口点。
//
#include "client_lib.h"

int main( int argc, char* argv[] )
{
    client_lib::Socket socket_;
    bool nRet = socket_.Connect("115.239.211.112", 4576, 3000);
    return 0;
}

