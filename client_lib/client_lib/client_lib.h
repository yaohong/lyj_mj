#ifndef _client_lib_h__
#define _client_lib_h__

#include <string>
#include <functional>
#include "buffer.h"


namespace client_lib
{

    typedef std::function<void( const char*, unsigned int )> CompletePackageCallback;
    const unsigned int buffer_size = 8 * 1024 * 1024;
    const unsigned short packet_head_size = 2;
    class Socket
    {
    public:
        Socket();
        ~Socket();
    public:
        void SetCompletePackageCallback( const CompletePackageCallback &cb);
        bool Connect(const std::string &addr, unsigned short port, int timeout);
        bool Send(const char *data, int dataLen);
        bool Recv();
        void Close();
    private:
        Buffer buffer_;
        CompletePackageCallback cb_;
        int socket_;
    };
}


#endif