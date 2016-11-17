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

	void RemoveSinglePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 removePai)
	{
		qp_uint8 removePool[1] = { removePai };
		RemovePai(source, sourceLen, removePool, 1);
	}

	void RemovePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 dest[], qp_uint8 destLen)
	{
		assert(sourceLen <= 14);
		assert(destLen <= 14);
		qp_uint8 tmpSource[14];
		qp_uint8 tmpDest[14];
		qp_uint8 tmpSourceLen = sourceLen;
		memset(tmpSource, 0, 14 * sizeof(qp_uint8));
		memset(tmpDest, 0, 14 * sizeof(qp_uint8));
		memcpy(tmpSource, source, tmpSourceLen * sizeof(qp_uint8));
		memcpy(tmpDest, dest, destLen * sizeof(qp_uint8));

		memset(source, 0, tmpSourceLen * sizeof(qp_uint8));
		qp_uint8 writeIndex = 0;
		for (qp_uint8 i = 0; i < tmpSourceLen; i++)
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
						sourceLen--;
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

	void AddSinglePai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 addPai)
	{
		qp_uint8 paiPool[1] = { addPai };
		AddPai(source, sourceLen, paiPool, 1);
	}

	void AddPai(qp_uint8 source[], qp_uint8 &sourceLen, qp_uint8 add_pai[], qp_uint8 addLen)
	{
		assert(sourceLen + addLen <= 14);
		for (qp_uint8 i = 0; i < addLen; i++)
		{
			source[sourceLen + i] = add_pai[i];
		}
		sourceLen += addLen;

		common::Sort(source, sourceLen);

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
		assert(poolLen <= 14);
		qp_uint8 type = TYPE(p);
		chiCount = 0;
		if (FENG == type || FA == type)
		{
			//风和中发白不能吃
			return;
		}
		qp_uint8 value = VALUE(p);

		//左边看右边两张是否存在
		if (value + 1 < 9 && value + 2 <= 9)
		{
			qp_uint8 p1 = PAI(type, value + 1);
			qp_uint8 p2 = PAI(type, value + 2);
			if (GetPaiCount(pool, poolLen, p1) > 0 && GetPaiCount(pool, poolLen, p2) > 0)
			{
				chi[chiCount][0] = p1;
				chi[chiCount][1] = p2;
				chiCount++;
			}
		}

		if (value - 1 >= 1 && value + 1 <= 9)
		{
			//中间看两边是否存在
			qp_uint8 p1 = PAI(type, value - 1);
			qp_uint8 p2 = PAI(type, value + 1);
			if (GetPaiCount(pool, poolLen, p1) > 0 && GetPaiCount(pool, poolLen, p2) > 0)
			{
				chi[chiCount][0] = p1;
				chi[chiCount][1] = p2;
				chiCount++;
			}
		}



		//右边看左边两张是否存在
		if (value - 2 >= 1 && value - 1 > 1)
		{
			qp_uint8 p1 = PAI(type, value - 2);
			qp_uint8 p2 = PAI(type, value - 1);
			if (GetPaiCount(pool, poolLen, p1) > 0 && GetPaiCount(pool, poolLen, p2) > 0)
			{
				chi[chiCount][0] = p1;
				chi[chiCount][1] = p2;
				chiCount++;
			}
		}
	}

	bool IsChi(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p)
	{
		assert(poolLen <= 14);
		qp_uint8 chiCount = 0;
		qp_uint8 chiData[3][2] = { {0,0}, {0,0}, {0,0} };
		GetChi(pool, poolLen, p, chiData, chiCount);
		return chiCount > 0;
	}

	bool IsPeng(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p)
	{
		assert(poolLen <= 14);
		return GetPaiCount(pool, poolLen, p) >= 2;
	}

	bool IsGang1(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 p)
	{
		assert(poolLen <= 14);
		return GetPaiCount(pool, poolLen, p) >= 3;
	}


	void GetGang(qp_uint8 pool[], qp_uint8 poolLen, qp_uint8 gang[3], qp_uint8 &gangCount)
	{
		assert(poolLen <= 14);
		gangCount = 0;
		for (qp_uint8 i = 0; i < MAX_TITLE_INDEX; i++)
		{
			if (GetPaiCount(pool, poolLen, PAI_ARRAY[i]) == 4)
			{
				gang[gangCount++] = PAI_ARRAY[i];
			}
		}
	}

	bool IsGang(qp_uint8 pool[], qp_uint8 poolLen)
	{
		assert(poolLen <= 14);
		qp_uint8 gang[3] = {0,0,0};
		qp_uint8 gangCount = 0;
		GetGang(pool, poolLen, gang, gangCount);
		return gangCount > 0;
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