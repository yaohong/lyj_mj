#ifndef _Buffer_h__
#define _Buffer_h__
#include <assert.h>
#include <vector>
namespace client_lib
{
        static const size_t kCheapPrepend = 8;
        static const size_t kInitialSize = 4 * 1024 * 1024;     //初始化4M的缓存
        class Buffer
        {
            friend class Socket;
        public:
            Buffer( size_t initialSize = kInitialSize );
        private:
            ~Buffer();
        public:
            //可以读取的字节数
            size_t readableBytes() const
            {
                return writerIndex_ - readerIndex_;
            }

            //可以写入的字节数
            size_t writableBytes() const
            {
                return buffer_.size() - writerIndex_;
            }

            //当前的读取偏移
            size_t prependableBytes() const
            {
                return readerIndex_;
            }

            //当前读取的起始地址
            const char* peek() const
            {
                return begin() + readerIndex_;
            }

            //修改读取偏移
            void retrieve( size_t len )
            {
                assert( len <= readableBytes() );
                if (len < readableBytes())
                {
                    readerIndex_ += len;
                }
                else
                {
                    //读取偏移等于写入偏移，重置
                    retrieveAll();
                }
            }

            //重置
            void retrieveAll()
            {
                readerIndex_ = kCheapPrepend;
                writerIndex_ = kCheapPrepend;
            }

            //读取指定长度的字符串
            std::string retrieveAsString( size_t len )
            {
                assert( len <= readableBytes() );
                std::string result( peek(), len );
                retrieve( len );
                return result;
            }

            //当前写的偏移地址
            char* beginWrite()
            {
                return begin() + writerIndex_;
            }

            void testWrite( const char *test )
            {
                int len = strlen( test );
                memcpy( beginWrite(), test, len );
                hasWritten( len );
            }

            int size()
            {
                return buffer_.size();
            }

            int capacity()
            {
                return buffer_.capacity();
            }

            int readFd( int fd );
        private:

            char* begin()
            {
                return &*buffer_.begin();
            }

            const char* begin() const
            {
                return &*buffer_.begin();
            }

            const char* beginWrite() const
            {
                return begin() + writerIndex_;
            }

            void hasWritten( size_t len )
            {
                assert( len <= writableBytes() );
                writerIndex_ += len;
            }
            void move()
            {
                assert( kCheapPrepend < readerIndex_ );
                size_t readable = readableBytes();
                std::copy(
                    begin() + readerIndex_,
                    begin() + writerIndex_,
                    begin() + kCheapPrepend );
                readerIndex_ = kCheapPrepend;
                writerIndex_ = readerIndex_ + readable;
                assert( readable == readableBytes() );
            }
        private:
            std::vector<char> buffer_;
            size_t readerIndex_;
            size_t writerIndex_;
        };
}













#endif