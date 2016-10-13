#ifndef _sysdefine_h__
#define _sysdefine_h__

#ifdef WIN32 
#include <WinSock2.h>
#include <WS2tcpip.h>
#include <windows.h>
#include <process.h>
#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "user32.lib")
#endif
#include <assert.h>






#endif