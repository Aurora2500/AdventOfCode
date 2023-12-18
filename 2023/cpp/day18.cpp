#include <vector>
#include <string>
#include <iostream>
#include <string_view>
#include <format>

#include "util.hpp"

template <class T>
using two = std::pair<T, T>;

enum class Direction
{
	Up,
	Down,
	Left,
	Right,
};

Direction char_to_dir(char c)
{
	switch (c)
	{
	case 'U':
		return Direction::Up;
	case 'D':
		return Direction::Down;
	case 'L':
		return Direction::Left;
	case 'R':
		return Direction::Right;
	}
	throw std::runtime_error("invalid direction");
}

struct instruction
{
	Direction dir;
	int steps;

	instruction(Direction d, int s) : dir(d), steps(s) {}
};

two<std::vector<instruction>> parse(std::istream &is)
{
	two<std::vector<instruction>> instructions;
	std::string line;
	while (std::getline(is, line))
	{
		std::string_view sv(line);
		Direction d1 = char_to_dir(sv[0]);
		sv.remove_prefix(2);
		int s1 = take_int(sv);
		sv.remove_prefix(3);
		int s2 = take_int(sv, 5, 16);
		int d2i = take_int(sv);
		Direction d2;
		switch (d2i)
		{
		case 0:
			d2 = Direction::Right;
			break;
		case 1:
			d2 = Direction::Down;
			break;
		case 2:
			d2 = Direction::Left;
			break;
		case 3:
			d2 = Direction::Up;
			break;
		}
		instructions.first.emplace_back(d1, s1);
		instructions.second.emplace_back(d2, s2);
	}
	return instructions;
}

u64 area(const std::vector<instruction> &instructions)
{
	i64 sum = 0;
	u64 length = 0;
	i64 y = 0;
	for (auto &i : instructions)
	{
		length += i.steps;
		switch (i.dir)
		{
		case Direction::Up:
			y -= i.steps;
			break;
		case Direction::Down:
			y += i.steps;
			break;
		case Direction::Left:
			sum += y * i.steps;
			break;
		case Direction::Right:
			sum -= y * i.steps;
			break;
		}
	}
	return std::abs(sum) + length / 2 + 1;
}

int main()
{
	auto [ins1, ins2] = parse(std::cin);
	std::cout << "Part 1: " << area(ins1) << std::endl;
	std::cout << "Part 2: " << area(ins2) << std::endl;
}
