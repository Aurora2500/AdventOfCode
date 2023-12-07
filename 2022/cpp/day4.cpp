#include <iostream>
#include <string_view>
#include <cctype>
#include <charconv>

int take_int(std::string_view &sv)
{
	int res;
	for (int i = 0; i < sv.length(); i++)
	{
		if (!std::isdigit(sv.at(i)))
		{
			std::from_chars(sv.data(), sv.data() + i, res);
			sv.remove_prefix(i);
			return res;
		}
	}
	std::from_chars(sv.data(), sv.data() + sv.length(), res);
	return res;
}

void part2()
{
	int count = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		std::string_view sv(line);
		int start1 = take_int(sv);
		sv.remove_prefix(1);
		int end1 = take_int(sv);
		sv.remove_prefix(1);
		int start2 = take_int(sv);
		sv.remove_prefix(1);
		int end2 = take_int(sv);
		if ((start1 <= start2 && start2 <= end1) || (start1 <= end2 && end2 <= end1) || (start2 <= start1 && start1 <= end2) || (start2 <= end1 && end1 <= end2))
		{
			count += 1;
		}
	}
	std::cout << "Part 1: " << count << std::endl;
}

void part1()
{
	int count = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		std::string_view sv(line);
		int start1 = take_int(sv);
		sv.remove_prefix(1);
		int end1 = take_int(sv);
		sv.remove_prefix(1);
		int start2 = take_int(sv);
		sv.remove_prefix(1);
		int end2 = take_int(sv);
		if ((start1 <= start2 && end1 >= end2) || (start1 >= start2 && end1 <= end2))
		{
			count += 1;
		}
	}
	std::cout << "Part 1: " << count << std::endl;
}

int main()
{
	part2();
}