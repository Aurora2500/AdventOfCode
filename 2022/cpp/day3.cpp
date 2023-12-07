#include <iostream>
#include <string_view>
#include <unordered_set>

int char_to_priority(char c)
{
	if ('a' <= c && c <= 'z')
		return c - 'a' + 1;
	return c - 'A' + 27;
}

void part2()
{
	int sum = 0;
	std::string line1, line2, line3;

	while (std::getline(std::cin, line1), std::getline(std::cin, line2), std::getline(std::cin, line3))
	{
		std::unordered_set<char> set1, set2;
		for (char c : line1)
		{
			set1.insert(c);
		}
		for (char c : line2)
		{
			set2.insert(c);
		}
		for (char c : line3)
		{
			if (set1.contains(c) && set2.contains(c))
			{
				sum += char_to_priority(c);
				break;
			}
		}
	}

	std::cout << "Part 2: " << sum << std::endl;
}

void part1()
{
	int sum = 0;
	std::string line;

	while (std::getline(std::cin, line))
	{
		bool first_half[57] = {false};
		size_t half = line.length() / 2;
		for (int i = 0; i < half; i++)
		{
			first_half[char_to_priority(line.at(i))] = true;
		}
		for (int i = half; i < line.length(); i++)
		{
			int p = char_to_priority(line.at(i));
			if (first_half[p])
			{
				sum += p;
				break;
			}
		}
	}
	std::cout << "Part 1: " << sum << std::endl;
}

int main()
{
	part2();
}