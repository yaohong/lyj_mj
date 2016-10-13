#include "sockets.h"

namespace client_lib
{
#ifdef WIN32 
    SocketInit::SocketInit()
    {
        WORD wVer = MAKEWORD( 2, 2 );
        WSADATA wsaData;
        WSAStartup( wVer, &wsaData );
    }
    
#else 
    IgnoreSigPipe::IgnoreSigPipe()
    {
        ::signal( SIGPIPE, SIG_IGN );
    }
#endif


    int sockets::createNonblockingOrDie()
    {
#ifndef WIN32 
        int sockfd = ::socket( PF_INET, SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0 );
        assert( !(sockfd < 0) );
        return sockfd;
#else
        int sockfd = socket( PF_INET, SOCK_STREAM, 0 );
        assert( !(sockfd < 0) );
        sockets::setNonBlockAndCloseOnExec( sockfd );
        return sockfd;
#endif
    }

    void sockets::setNonBlockAndCloseOnExec( int sockfd )
    {
#ifndef WIN32
        // non-block
        int flags = ::fcntl( sockfd, F_GETFL, 0 );
        flags |= O_NONBLOCK;
        int ret = ::fcntl( sockfd, F_SETFL, flags );
        // FIXME check

        // close-on-exec
        flags = ::fcntl( sockfd, F_GETFD, 0 );
        flags |= FD_CLOEXEC;
        ret = ::fcntl( sockfd, F_SETFD, flags );
        // FIXME check

        (void)ret;
#else
        int ul = 1;           //1·Ç×èÈû:0×èÈû
        if (SOCKET_ERROR == ioctlsocket( sockfd, FIONBIO, (unsigned long *)&ul ))
        {
            assert( false );
        }
#endif
    }

    void sockets::close( int sockfd )
    {
#ifndef WIN32
        ::close( sockfd );
#else 
        closesocket( sockfd );
#endif
    }

    void sockets::setSendBuffSize( int sockfd, int size )
    {
        int ret = ::setsockopt( sockfd, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(size) );
        if (ret < 0)
        {
            assert( false );
        }
    }

    void sockets::setRecvBuffSize( int sockfd, int size )
    {
        int ret = ::setsockopt( sockfd, SOL_SOCKET, SO_RCVBUF, (char *)&size, sizeof(size) );
        if (ret < 0)
        {
            assert( false );
        }
    }
}