#include "common.h"
#include <stdlib.h>
#include <stdio.h>

namespace common {
	std::string getPaiString(uint8 p)
	{
		uint8 type = p >> 4;
		uint8 value = p & 15;
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

	void Sort(uint8 pai[], uint8 count)
	{
		//万 条 筒 风 发
		for (uint8 i = count; i>0; i--)
		{
			for (int8 j = 0; j < i - 1; j++)
			{
				if (pai[j] > pai[j + 1])
				{
					uint8 t = pai[j];
					pai[j] = pai[j + 1];
					pai[j + 1] = t;
				}
			}

		}
	}

	void Random(uint8 pai[], uint8 count)
	{
		uint8 tmpRandRange = count;
		for (uint8 i = 0; i < count; ++i)
		{
			uint8 tmpIndex = rand() % tmpRandRange--;
			//和最大索引交换值
			uint8 tmpValue = pai[tmpIndex];
			pai[tmpIndex] = pai[tmpRandRange];
			pai[tmpRandRange] = tmpValue;
		}
	}

	void Crc(uint8 pai[], uint8 count)
	{
		std::map<uint8, int> verifyMap;
		for (int i = 0; i < count; ++i)
		{
			std::map<uint8, int>::iterator p_it = verifyMap.find(pai[i]);
			if (p_it == verifyMap.end())
			{
				verifyMap.insert(std::pair<uint8, int>(pai[i], 1));
			}
			else
			{
				p_it->second++;
			}
		}

		for (int i = 0; i < MAX_TITLE_INDEX; ++i)
		{
			std::map<uint8, int>::iterator p_it = verifyMap.find(pai[common::PAI_ARRAY[i]]);
			if (p_it != verifyMap.end())
			{
				assert(p_it->second == 4);
			}
		}
	}

	void RemovePai(uint8 source[], uint8 sourceLen, uint8 dest[], uint8 destLen)
	{
		assert(sourceLen <= 14);
		assert(destLen <= 14);
		uint8 tmpSource[14];
		uint8 tmpDest[14];
		memset(tmpSource, 0, 14 * sizeof(uint8));
		memset(tmpDest, 0, 14 * sizeof(uint8));
		memcpy(tmpSource, source, sourceLen * sizeof(uint8));
		memcpy(tmpDest, dest, destLen * sizeof(uint8));

		memset(source, 0, sourceLen * sizeof(uint8));
		uint8 writeIndex = 0;
		for (uint8 i = 0; i < sourceLen; i++)
		{
			if (tmpSource[i] != 0)
			{
				bool isRemove = false;
				for (uint8 j = 0; j < destLen; j++)
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

	void CheckBasicHuPai(uint8 source[], uint8 sourceLen, std::vector<HuBasicResult> &result)
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