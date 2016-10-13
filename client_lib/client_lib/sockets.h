#ifndef _sockets_h__
#define _sockets_h__
#include "sysdefine.h"
namespace client_lib
{
    class sockets
    {
    public:
        static int createNonblockingOrDie();
        static void setNonBlockAndCloseOnExec( int sockfd );
        static void close( int sockfd );
        static void setSendBuffSize( int sockfd, int size );
        static void setRecvBuffSize( int sockfd, int size );
    };

#ifndef WIN32
    class IgnoreSigPipe
    {
    public:
        IgnoreSigPipe();
    };

#else 
    class SocketInit
    {
    public:
        SocketInit();
    };
#endif

#ifdef WIN32
#define NETWORK_ERROR WSAGetLastError()
#else 
#define NETWORK_ERROR errno
#endif
}







#endif