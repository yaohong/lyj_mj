#ifndef _endian_h__
#define _endian_h__



namespace client_lib
{
	namespace endian {
		inline unsigned short hostToNetwork16(unsigned short host16)
		{
#ifndef WIN32
			return htobe16(host16);
#else 
			return htons(host16);
#endif
		}


		inline unsigned short networkToHost16(unsigned short net16)
		{
#ifndef WIN32
			return be16toh(net16);
#else 
			return ntohs(net16);
#endif
		}
	}



}





#endif