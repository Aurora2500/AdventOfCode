#include <vector>
#include <string>
#include <iostream>
#include <string_view>
#include <format>

#include "util.hpp"

enum class Direction
{
	Up = 1 << 0,
	Down = 1 << 1,
	Left = 1 << 2,
	Right = 1 << 3,
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

std::ostream &operator<<(std::ostream &os, const Direction &d)
{
	switch (d)
	{
	case Direction::Up:
		return os << "Up";
	case Direction::Down:
		return os << "Down";
	case Direction::Left:
		return os << "Left";
	case Direction::Right:
		return os << "Right";
	}
}

struct instruction
{
	Direction dir;
	int steps;
	Direction dir2;
	int steps2;

	instruction(Direction d, int s, Direction d2, int s2) : dir(d), steps(s), dir2(d2), steps2(s2) {}
};

std::vector<instruction> parse(std::istream &is)
{
	std::vector<instruction> instructions;
	std::string line;
	while (std::getline(is, line))
	{
		std::string_view sv(line);
		Direction dir = char_to_dir(sv[0]);
		sv.remove_prefix(2);
		int steps = take_int(sv);
		sv.remove_prefix(3);
		int steps2 = take_int(sv, 5, 16);
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
		instructions.emplace_back(dir, steps, d2, steps2);
	}
	return instructions;
}

void part1(const std::vector<instruction> &instructions)
{
	int x = 0, y = 0;
	int max_x = 0, max_y = 0;
	int min_x = 0, min_y = 0;

	for (const auto &i : instructions)
	{
		switch (i.dir)
		{
		case Direction::Up:
			y -= i.steps;
			break;
		case Direction::Down:
			y += i.steps;
			break;
		case Direction::Left:
			x -= i.steps;
			break;
		case Direction::Right:
			x += i.steps;
			break;
		}
		max_x = std::max(max_x, x);
		max_y = std::max(max_y, y);
		min_x = std::min(min_x, x);
		min_y = std::min(min_y, y);
	}

	max_x++;
	max_y++;

	int width = max_x - min_x + 1;
	int height = max_y - min_y + 1;

	std::vector<char> grid(width * height, 0);

	x = -min_x, y = -min_y;

	for (auto &i : instructions)
	{
		switch (i.dir)
		{
		case Direction::Up:
			grid[y * width + x] |= (int)Direction::Up;
			for (int j = 0; j < i.steps; j++)
			{
				y--;
				grid[y * width + x] |= (int)Direction::Down | ((j == i.steps - 1) ? 0 : (int)Direction::Up);
			}
			break;
		case Direction::Down:
			grid[y * width + x] |= (int)Direction::Down;
			for (int j = 0; j < i.steps; j++)
			{
				y++;
				grid[y * width + x] |= (int)Direction::Up | ((j == i.steps - 1) ? 0 : (int)Direction::Down);
			}
			break;
		case Direction::Left:
			grid[y * width + x] |= (int)Direction::Left;
			for (int j = 0; j < i.steps; j++)
			{
				x--;
				grid[y * width + x] |= (int)Direction::Right | ((j == i.steps - 1) ? 0 : (int)Direction::Left);
			}
			break;
		case Direction::Right:
			grid[y * width + x] |= (int)Direction::Right;
			for (int j = 0; j < i.steps; j++)
			{
				x++;
				grid[y * width + x] |= (int)Direction::Left | ((j == i.steps - 1) ? 0 : (int)Direction::Right);
			}
			break;
		}
	}

	// for (int i = 0; i < max_y; i++)
	// {
	// 	for (int j = 0; j < width; j++)
	// 	{
	// 		std::cout << std::format(" {:02}", (int)grid[i * width + j]);
	// 	}
	// 	std::cout << "\n";
	// }
	// std::cout << std::endl;

	int space = 0;
	for (int i = 0; i < height; i++)
	{
		bool outside = false;
		for (int j = 0; j < width; j++)
		{
			if (grid[i * width + j] != 0)
			{
				space++;
				// std::cout << "#";
				if (grid[i * width + j] & (int)Direction::Up)
					outside = !outside;
				continue;
			}
			if (outside)
			{
				space++;
				// std::cout << "*";
				continue;
			}
			// std::cout << ".";
		}
		// std::cout << "\n";
	}

	std::cout << "Part 1: " << space << std::endl;
}

struct pos
{
	i64 x, y;
	Direction d;
};

void part2(const std::vector<instruction> &instructions)
{
	i64 x = 0, y = 0;
	std::vector<pos> vertices;
	for (auto &i : instructions)
	{
		switch (i.dir2)
		{
		case Direction::Up:
			vertices.push_back({x, y, i.dir2});
			y -= i.steps2;
			break;
		case Direction::Down:
			vertices.push_back({x, y, i.dir2});
			y += i.steps2;
			break;
		case Direction::Left:
			vertices.push_back({x, y, i.dir2});
			x -= i.steps2;
			break;
		case Direction::Right:
			vertices.push_back({x, y, i.dir2});
			x += i.steps2;
			break;
		}
	}

	i64 sum = 0;
	for (int i = 0; i < vertices.size(); i++)
	{
		auto &v1 = vertices[i];
		auto &v2 = vertices[(i + 1) % vertices.size()];
		auto &v3 = vertices[(i + 2) % vertices.size()];
		if ((int)v2.d & ((int)Direction::Up | (int)Direction::Down))
			continue;
		bool p1 = v1.d == Direction::Down;
		bool p3 = v3.d == Direction::Up;
		if (v2.d == Direction::Right)
		{
			i64 h = v2.y;
			i64 w = v3.x - v2.x - (p1 && p3) + !(p1 || p3);
			sum -= h * w;
		}
		else
		{
			i64 h = v2.y + 1;
			i64 w = v2.x - v3.x + (p1 && p3) - !(p1 || p3);
			sum += h * w;
		}
	}
	std::cout << "Part 2: " << sum << std::endl;
}

int main()
{
	auto instructions = parse(std::cin);
	part1(instructions);
	part2(instructions);
}
