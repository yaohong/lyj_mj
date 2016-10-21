// client_lib.cpp : 定义控制台应用程序的入口点。
//
#include "client_lib.h"
#include "mj.pb.h"
#include "endian.h"

using namespace client_lib;
#ifdef WIN32

#ifdef _DEBUG
#pragma comment(lib, "./lib/protobuf/lib/libprotobuf_d.lib")
#else
#pragma comment(lib, "./lib/protobuf/lib/libprotobuf.lib")
#endif

#pragma comment(lib, "ws2_32.lib")
#pragma comment(lib, "user32.lib")

#endif

void packetHandle( client_lib::Socket &socket_, const char *data, int len)
{
    qp_server::qp_packet root;
    root.ParseFromArray( data, len );
    switch ((qp_server::ws_cmd)root.cmd())
    {
        case qp_server::CMD_QP_LOGIN_RSP:
        {
            qp_server::qp_login_rsp rsp;
            rsp.ParseFromString(root.serialized());

        }
            break;
        case qp_server::CMD_QP_CREATE_ROOM_RSP:
        {
            qp_server::qp_create_room_rsp rsp;
            rsp.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_JOIN_ROOM_RSP:
        {
            qp_server::qp_join_room_rsp rsp;
            rsp.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_JOIN_ROOM_PUSH:
        {
            qp_server::qp_join_room_push push;
            push.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_READY_RSP:
        {
            qp_server::qp_ready_rsp rsp;
            rsp.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_READY_PUSH:
        {
            qp_server::qp_ready_push push;
            push.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_EXIT_ROOM_RSP:
        {
            qp_server::qp_exit_room_rsp rsp;
            rsp.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_EXIT_ROOM_PUSH:
        {
            qp_server::qp_exit_room_push push;
            push.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_GAME_DATA:
        {
            qp_server::qp_game_data data;
            data.ParseFromString( root.serialized() );
        }
            break;
        case qp_server::CMD_QP_PING_RSP:
        {
            qp_server::qp_ping_rsp rsp;
            rsp.ParseFromString( root.serialized() );
        }
            break;
        default:
            printf( "unkown cmd:%d\n", root.cmd());
            socket_.Close();
    }
}

void sendPacket( client_lib::Socket &socket, const google::protobuf::Message* msg, unsigned int cmdId );

int main( int argc, char* argv[] )
{
    client_lib::Socket socket_;
    socket_.SetCompletePackageCallback( std::bind( packetHandle, std::placeholders::_1, std::placeholders::_2, std::placeholders::_3 ) );
    bool nRet = socket_.Connect( "106.75.30.194", 18751, 3000 );
    if (!nRet)
    {
        return 0;
    }
    qp_server::qp_login_req req;
    req.set_account("yaohong");
    sendPacket( socket_, &req, qp_server::CMD_QP_LOGIN_REQ );
    
    while (true)
    {
        Sleep( 100 );
        if (!socket_.Recv())
        {
            break;
        }
    }

    return 0;
}

#define DEFAULT_BUFF_SIZE   (1*1024*1024)
void sendPacket( client_lib::Socket &socket, const google::protobuf::Message* msg, unsigned int cmdId )
{
    static  char msgBuf[DEFAULT_BUFF_SIZE];
    int needSize = msg->ByteSize();
    msg->SerializeToArray( msgBuf, DEFAULT_BUFF_SIZE );

    qp_server::qp_packet root;
    root.set_cmd( cmdId );
    root.set_serialized( msgBuf, needSize );
    root.set_seq_id( 0 );

    int rootSize = root.ByteSize();
    root.SerializeToArray( msgBuf + 2, DEFAULT_BUFF_SIZE );
    *((unsigned short *)msgBuf) = endian::hostToNetwork16( rootSize );
    
    socket.Send( msgBuf, rootSize + 2 );
}

