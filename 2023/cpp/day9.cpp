#include <iostream>
#include <vector>

#include "util.hpp"

struct series
{
	std::vector<int> xs;

	std::pair<int, int> predict()
	{
		std::vector<std::vector<int>> dxs;
		dxs.push_back(xs);
		while (true)
		{
			bool all_zeroes = true;
			auto &dx = dxs.emplace_back(dxs.back().size() - 1);
			std::vector<int> &lastxs = dxs[dxs.size() - 2];
			for (int i = 0; i < lastxs.size() - 1; i++)
			{
				int diff = lastxs[i + 1] - lastxs[i];
				dx[i] = diff;
				if (diff != 0)
					all_zeroes = false;
			}
			if (all_zeroes)
				break;
		}

		int acc_b = 0;
		int acc_f = 0;
		for (int i = dxs.size() - 1; i >= 0; i--)
		{
			acc_f += dxs[i].back();
			acc_b = dxs[i].front() - acc_b;
		}

		return {acc_f, acc_b};
	}
};

std::vector<series> parse(std::istream &input)
{
	std::vector<series> res;
	std::string line;
	while (std::getline(input, line))
	{
		res.emplace_back();
		series &s = res.back();
		std::string_view sv(line);
		while (sv.length() > 0)
		{
			int x = take_int(sv);
			s.xs.push_back(x);
			while (sv.length() > 0 && sv[0] == ' ')
				sv.remove_prefix(1);
		}
	}
	return res;
}

void parts(std::vector<series> &ss)
{
	int sum_forward = 0;
	int sum_backward = 0;
	for (auto &s : ss)
	{
		auto [forward, backward] = s.predict();
		sum_forward += forward;
		sum_backward += backward;
	}
	std::cout << "Part 1: " << sum_forward << std::endl;
	std::cout << "Part 2: " << sum_backward << std::endl;
}

int main()
{
	std::vector<series> ss = parse(std::cin);
	parts(ss);
}