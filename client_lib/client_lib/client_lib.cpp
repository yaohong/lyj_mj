#include "client_lib.h"
#include "sockets.h"
#include "endian.h"
namespace client_lib
{
#ifdef WIN32 
    SocketInit initObj;
#else 
    IgnoreSigPipe initObj;
#endif

    Socket::Socket()
    {
        socket_ = -1;
        socket_ = sockets::createNonblockingOrDie();
        sockets::setSendBuffSize( socket_, 8 * 1024 * 1024 );
        sockets::setRecvBuffSize( socket_, 8 * 1024 * 1024 );
    }

    Socket::~Socket()
    {
        Close();
    }

    void Socket::SetCompletePackageCallback( const CompletePackageCallback &cb )
    {
        cb_ = cb;
    }

    bool Socket::Connect( const std::string &addr, unsigned short port, int timeoutValue )
    {
        assert(socket_ > 0);
        struct sockaddr_in server_addr;
        memset( &server_addr, 0, sizeof(server_addr) );
        server_addr.sin_family = AF_INET;
#ifdef WIN32
        server_addr.sin_addr.S_un.S_addr = inet_addr( addr.c_str() );
#else
        server_addr.sin_addr.s_addr = inet_addr( addr.c_str() );
#endif
        server_addr.sin_port = htons( port );
        int ret = ::connect( socket_, (const sockaddr *)&server_addr, sizeof(server_addr) );
        if (ret >= 0)
        {
            return true;
        }
        else
        {
            int errorCode = NETWORK_ERROR;
#ifdef WIN32
            if (errorCode == WSAEWOULDBLOCK)
            {
#else
            if (errorCode == EINPROGRESS)
            {
#endif
                //正在进行中，检查
                fd_set write_set;
                timeval timeout;
                timeout.tv_sec = 0;
                timeout.tv_usec = timeoutValue * 1000;
                while (true)
                {
                    FD_ZERO( &write_set );
                    FD_SET( socket_, &write_set );
                    int ret = ::select( socket_ + 1, NULL, &write_set, NULL, &timeout );
                    if (ret == -1 && (errno == EAGAIN || errno == EINTR))
                    {
                        continue;
                    }
                    break;
                }

                //判断是否连接成功
                if (FD_ISSET( socket_, &write_set ))
                {
#ifndef WIN32
                    int error = 0;
                    socklen_t len = sizeof(error);
                    if (getsockopt( socket_, SOL_SOCKET, SO_ERROR, (char *)&error, &len ) == 0)
                    {
                        if (error != 0)
                        {
                            printf( "connect failed, getsockopt[error=%d]\n", error );
                            return false;
                        }
                    } 
                    else 
                    {
                        printf( "connect failed, getsockopt failed, errno=%d\n", errno );
                        return false;
                    }
#endif 
                }
                else
                {
                    //链接失败
                    printf( "connect failed, timeout\n" );
                    return false;
                }

            }
            else
            {
                printf( "connect failed, errno=%d\n", errorCode );
                return false;
            }
        } 

        return true;
    }

    bool Socket::Send( const char *data, int dataLen )
    {
        assert( socket_ > 0 );
        int sendLen = ::send( socket_, (const char *)data, dataLen, 0 );
        if (sendLen == dataLen)
        {
            return true;
        }
        return false;
    }

    bool Socket::Recv()
    {
        int n = buffer_.readFd( socket_ );
        if (n > 0)
        {
			while (true)
			{
				int readableBbyte = buffer_.readableBytes();
                if (readableBbyte < packet_head_size)
				{
					//不够一个包头
					return true;
				}
				const char *bufOffset = buffer_.peek();
				unsigned short packetSize = endian::networkToHost32(*(unsigned short *)bufOffset);

                int totalLen = packetSize + packet_head_size;
				if (readableBbyte < totalLen)
				{
					return true;
				}

                cb_(*this, bufOffset + packet_head_size, packetSize );
				buffer_.retrieve(totalLen);
			}
            return true;
        }
        
#ifdef WIN32 
        if (NETWORK_ERROR == WSAEWOULDBLOCK)
#else
        if (NETWORK_ERROR == EINTR || NETWORK_ERROR == EAGAIN)
#endif
        {
            return true;
        }

        return false;
    }

    void Socket::Close()
    {
        if (-1 != socket_)
        {
            sockets::close( socket_ );
            socket_ = -1;
        }
        
    }
}


