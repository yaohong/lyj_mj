#include "buffer.h"
namespace client_lib
{
        Buffer::Buffer( size_t initialSize )
            : buffer_( kCheapPrepend + initialSize )
            , readerIndex_( kCheapPrepend )
            , writerIndex_( kCheapPrepend )
        {

        }

        Buffer::~Buffer()
        {

        }

        int Buffer::readFd( int fd )
        {
            size_t writable = writableBytes();
            if (writable == 0)
            {
                if (readerIndex_ == kCheapPrepend && writerIndex_ == buffer_.size())
                {
                    //缓冲区满了,还没有读取数据,这个时候不接受数据
                    assert( false );
                    return 1;
                }

                move(); //移动位置
                writable = writableBytes();
                assert( writable > 0 );
            }
            int n = ::recv( fd, beginWrite(), writable, 0 );
            if (n >= 0)
            {
                hasWritten( n );
            }

            return n;
        }
}