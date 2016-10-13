
#include "buffer.h"
#include "sysdefine.h"
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
                //两种原因会导致以下情况
                //1:一个完整包BUFFER装不下（这个不应该发生）设计问题，正式环境不应该出现
                //2:当前剩余空间不够读完一个整包(有可能发生) 正式环境可能出现（几率很低）
                //3:用户层不读取数据（BUG）设计问题，正式环境不应该出现
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