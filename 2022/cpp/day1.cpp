#include <iostream>

void part2()
{
	int top1 = 0, top2 = 0, top3 = 0;
	int curr = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		if (line.length() == 0)
		{
			if (curr >= top1)
			{
				top3 = top2;
				top2 = top1;
				top1 = curr;
			}
			else if (curr >= top2)
			{
				top3 = top2;
				top2 = curr;
			}
			else if (curr >= top3)
			{
				top3 = curr;
			}
			curr = 0;
			continue;
		}
		curr += std::stoi(line);
	}
	std::cout << "Part 2: " << top1 + top2 + top3 << std::endl;
}

void part1()
{
	int max = 0;
	int curr = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		if (line.length() == 0)
		{
			max = max < curr ? curr : max;
			curr = 0;
			continue;
		}
		curr += std::stoi(line);
	}
	std::cout << "Part 1: " << max << std::endl;
}

int main()
{
	part2();
}