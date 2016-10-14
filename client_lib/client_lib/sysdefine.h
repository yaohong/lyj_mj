#ifndef _sysdefine_h__
#define _sysdefine_h__

#ifdef WIN32 
#include <WinSock2.h>
#include <WS2tcpip.h>
#include <windows.h>
#include <process.h>
#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "user32.lib")
#else 
#include <sys/socket.h>    // socket     connect  write
#include <arpa/inet.h>      // inet_pton
#include <unistd.h>
#include <pthread.h>
#include <dirent.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/eventfd.h>
#endif
#include <vector>
#include <string>
#include <memory>
#include <string.h>
#include <assert.h>
#include <fcntl.h>

#endif