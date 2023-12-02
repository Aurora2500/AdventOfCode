#include <iostream>
#include <charconv>

#define RED_MAX 12
#define GREEN_MAX 13
#define BLUE_MAX 14

struct set
{
	int r, g, b;
	set(int r, int g, int b) : r(r), g(g), b(b) {}
	int power() const { return r * g * b; }
};

void part2()
{
	int acc = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		set curr(0, 0, 0);
		std::string_view sv(line);
		size_t start = sv.find(":");
		sv.remove_prefix(start + 2);
		while (sv.length() > 0)
		{
			size_t n_len = sv.find(" ");
			int num;
			std::from_chars(sv.data(), sv.data() + n_len, num);
			sv.remove_prefix(n_len + 1);
			if (sv.at(0) == 'r')
			{
				curr.r = num < curr.r ? curr.r : num;
			}
			else if (sv.at(0) == 'g')
			{
				curr.g = num < curr.g ? curr.g : num;
			}
			else if (sv.at(0) == 'b')
			{
				curr.b = num < curr.b ? curr.b : num;
			}
			size_t next = sv.find(" ");
			if (next == std::string_view::npos)
				break;
			sv.remove_prefix(next + 1);
		}
		acc += curr.power();
	}
	std::cout << "Part 2: " << acc << std::endl;
}

void part1()
{
	int acc = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		std::string_view sv(line);
		sv.remove_prefix(5); // remove "Game "
		size_t n_len = sv.find(":");
		int curr_id;
		std::from_chars(sv.data(), sv.data() + n_len, curr_id);
		sv.remove_prefix(n_len + 2); // remove " numbrer:space"
		while (sv.length() > 0)
		{
			size_t n_len = sv.find(" ");
			int num;
			std::from_chars(sv.data(), sv.data() + n_len, num);
			sv.remove_prefix(n_len + 1);
			if (sv.at(0) == 'r')
			{
				if (num > RED_MAX)
					goto outer;
			}
			else if (sv.at(0) == 'g')
			{
				if (num > GREEN_MAX)
					goto outer;
			}
			else if (sv.at(0) == 'b')
			{
				if (num > BLUE_MAX)
					goto outer;
			}
			size_t next = sv.find(" ");
			if (next == std::string_view::npos)
				break;
			sv.remove_prefix(next + 1);
		}
		acc += curr_id;
	outer:;
	}
	std::cout << "Part 1: " << acc << std::endl;
}

int main()
{
	part2();
}