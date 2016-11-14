#include "common.h"
#include <stdlib.h>
#include <stdio.h>

namespace common {
	std::string getPaiString(qp_uint8 p)
	{
		qp_uint8 type = p >> 4;
		qp_uint8 value = p & 15;
		char buff[16] = { 0 };
		if (type == 1)
		{
			sprintf(buff, "%d-万.", value);
		}
		else if (type == 2)
		{
			sprintf(buff, "%d-条.", value);
		}
		else if (type == 3)
		{
			sprintf(buff, "%d-筒.", value);
		}
		else if (type == 5)
		{
			//中发白
			if (1 == value)
			{
				sprintf(buff, "红中.");
			}
			else if (2 == value)
			{
				sprintf(buff, "发财.");
			}
			else if (3 == value)
			{
				sprintf(buff, "白板.");
			}
			else
			{
				assert(false);
			}
		}
		else
		{
			assert(false);
		}


		return buff;
	}

	void Sort(qp_uint8 pai[], qp_uint8 count)
	{
		//万 条 筒 风 发
		for (qp_uint8 i = count; i>0; i--)
		{
			for (qp_int8 j = 0; j < i - 1; j++)
			{
				if (pai[j] > pai[j + 1])
				{
					qp_uint8 t = pai[j];
					pai[j] = pai[j + 1];
					pai[j + 1] = t;
				}
			}

		}
	}

	void Random(qp_uint8 pai[], qp_uint8 count)
	{
		qp_uint8 tmpRandRange = count;
		for (qp_uint8 i = 0; i < count; ++i)
		{
			qp_uint8 tmpIndex = rand() % tmpRandRange--;
			//和最大索引交换值
			qp_uint8 tmpValue = pai[tmpIndex];
			pai[tmpIndex] = pai[tmpRandRange];
			pai[tmpRandRange] = tmpValue;
		}
	}

	void Crc(qp_uint8 pai[], qp_uint8 count)
	{
		std::map<qp_uint8, int> verifyMap;
		for (int i = 0; i < count; ++i)
		{
			std::map<qp_uint8, int>::iterator p_it = verifyMap.find(pai[i]);
			if (p_it == verifyMap.end())
			{
				verifyMap.insert(std::pair<qp_uint8, int>(pai[i], 1));
			}
			else
			{
				p_it->second++;
			}
		}

		for (int i = 0; i < MAX_TITLE_INDEX; ++i)
		{
			std::map<qp_uint8, int>::iterator p_it = verifyMap.find(pai[common::PAI_ARRAY[i]]);
			if (p_it != verifyMap.end())
			{
				assert(p_it->second == 4);
			}
		}
	}

	void RemovePai(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 dest[], qp_uint8 destLen)
	{
		assert(sourceLen <= 14);
		assert(destLen <= 14);
		qp_uint8 tmpSource[14];
		qp_uint8 tmpDest[14];
		memset(tmpSource, 0, 14 * sizeof(qp_uint8));
		memset(tmpDest, 0, 14 * sizeof(qp_uint8));
		memcpy(tmpSource, source, sourceLen * sizeof(qp_uint8));
		memcpy(tmpDest, dest, destLen * sizeof(qp_uint8));

		memset(source, 0, sourceLen * sizeof(qp_uint8));
		qp_uint8 writeIndex = 0;
		for (qp_uint8 i = 0; i < sourceLen; i++)
		{
			if (tmpSource[i] != 0)
			{
				bool isRemove = false;
				for (qp_uint8 j = 0; j < destLen; j++)
				{
					if (tmpSource[i] == tmpDest[j])
					{
						tmpDest[j] = 0;
						isRemove = true;
						break;
					}
				}

				if (!isRemove)
				{
					source[writeIndex++] = tmpSource[i];
				}
			}
		}
	}

	bool CheckPai(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 dest[], qp_uint8 destLen)
	{
		assert(destLen <= 14);
		qp_uint8 tmpDest[14];
		memset(tmpDest, 0, 14 * sizeof(qp_uint8));
		memcpy(tmpDest, dest, destLen * sizeof(qp_uint8));

		qp_uint8 count = 0;
		for (qp_uint8 i = 0; i < sourceLen; i++)
		{
			if (source[i] != 0)
			{
				bool isRemove = false;
				for (qp_uint8 j = 0; j < destLen; j++)
				{
					if (source[i] == tmpDest[j])
					{
						tmpDest[j] = 0;
						count++;
						break;
					}
				}
			}
		}
		return count == sourceLen;
	}

	qp_uint8 GetPaiCount(qp_uint8 source[], qp_uint8 sourceLen, qp_uint8 p)
	{
		assert(sourceLen <= 14);
		qp_uint8 count = 0;
		for (qp_uint8 i = 0; i < sourceLen; i++)
		{
			if (source[i] == p)
			{
				count++;
			}
		}

		return count;
	}

	void GetChi(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p, qp_uint8 chi[3][2], qp_uint8 &chiCount)
	{
		
	}

	void CheckBasicHuPai(qp_uint8 source[], qp_uint8 sourceLen, std::vector<HuBasicResult> &result)
	{
		//看总数是否为3n + 2
		if (sourceLen == 2)
		{
			
		}
		if (sourceLen % 3 != 2)
		{
			return;
		}
		return;
	}
}