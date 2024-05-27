#include <iostream>
#include <vector>
#include <unordered_set>

template <class T = int>
using matrix = std::vector<std::vector<T>>;
using u64 = u_int64_t;

matrix<int> parse(std::istream &input)
{
	matrix<int> m;
	std::string line;
	while (std::getline(input, line))
	{
		m.emplace_back();
		for (char &c : line)
		{
			m.back().push_back(c - '0');
		}
	}
	return m;
}

void part1(matrix<int> &m)
{
	matrix<bool> seen;
	for (int i = 0; i < m.size(); i++)
		seen.emplace_back(m[i].size());

	auto f = [&seen](matrix<int> m, int i, int j, int &h)
	{
		int curr = m[i][j];
		if (curr > h)
		{
			h = curr;
			seen[i][j] = true;
		}
	};

	// left
	for (int i = 0; i < m.size(); i++)
	{
		int h = -1;
		for (int j = 0; j < m.size(); j++)
		{
			f(m, i, j, h);
		}
	}
	// up
	for (int j = 0; j < m[0].size(); j++)
	{
		int h = -1;
		for (int i = 0; i < m.size(); i++)
		{
			f(m, i, j, h);
		}
	}

	// right
	for (int i = 0; i < m.size(); i++)
	{
		int h = -1;
		for (int j = m.size() - 1; j >= 0; j--)
		{
			f(m, i, j, h);
		}
	}

	// down
	for (int j = 0; j < m[0].size(); j++)
	{
		int h = -1;
		for (int i = m.size() - 1; i >= 0; i--)
		{
			f(m, i, j, h);
		}
	}

	int seenc = 0;

	for (int i = 0; i < m.size(); i++)
		for (int j = 0; j < m.size(); j++)
			if (seen[i][j])
				seenc++;

	std::cout << "Part 1: " << seenc << std::endl;
}

void part2(matrix<int> &m)
{
	u64 max_scenery = 0;
	int imax, jmax;

	for (int i = 0; i < m.size(); i++)
	{
		for (int j = 0; j < m.size(); j++)
		{
			int curr = m[i][j];
			u64 left = 0;
			for (int k = j - 1; k >= 0; k--)
			{
				left++;
				if (!(curr >= m[i][k]))
					break;
			}
			u64 up = 0;
			for (int k = i - 1; k >= 0; k--)
			{
				up++;
				if (!(curr > m[k][j]))
					break;
			}
			u64 right = 0;
			for (int k = j + 1; k < m.size(); k++)
			{
				right++;
				if (!(curr > m[i][k]))
					break;
			}
			u64 down = 0;
			for (int k = i + 1; k < m.size(); k++)
			{
				down++;
				if (!(curr > m[k][j]))
					break;
			}
			u64 scenery = left * up * right * down;
			if (scenery > max_scenery)
			{
				max_scenery = scenery;
				imax = i;
				jmax = j;
			}
		}
	}

	std::cout << "Part 2: " << max_scenery << std::endl;
	std::cout << "Found at " << imax + 1 << ", " << jmax + 1 << std::endl;
}

int main()
{
	matrix<int> m = parse(std::cin);
	part1(m);
	part2(m);
}