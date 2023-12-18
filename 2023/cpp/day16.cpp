#include <vector>
#include <list>
#include <set>
#include <iostream>
#include <algorithm>

enum class Tile
{
	ReflectorDexter,
	ReflectorSinister,
	SplitterVertical,
	SplitterHorizontal,
	Empty,
};

Tile char_to_tile(char c)
{
	switch (c)
	{
	case '\\':
		return Tile::ReflectorDexter;
	case '/':
		return Tile::ReflectorSinister;
	case '|':
		return Tile::SplitterVertical;
	case '-':
		return Tile::SplitterHorizontal;
	case '.':
		return Tile::Empty;
	}
	// error
	throw std::runtime_error("Invalid tile");
}

enum class Direction
{
	Up,
	Down,
	Left,
	Right,
};

std::pair<int, int> direction_to_delta(Direction d)
{
	switch (d)
	{
	case Direction::Up:
		return {0, -1};
	case Direction::Down:
		return {0, 1};
	case Direction::Left:
		return {-1, 0};
	case Direction::Right:
		return {1, 0};
	}
}

Direction reflect_dexter(Direction d)
{
	switch (d)
	{
	case Direction::Up:
		return Direction::Left;
	case Direction::Down:
		return Direction::Right;
	case Direction::Left:
		return Direction::Up;
	case Direction::Right:
		return Direction::Down;
	}
}

Direction reflect_sinister(Direction d)
{
	switch (d)
	{
	case Direction::Up:
		return Direction::Right;
	case Direction::Down:
		return Direction::Left;
	case Direction::Left:
		return Direction::Down;
	case Direction::Right:
		return Direction::Up;
	}
}

std::ostream &operator<<(std::ostream &os, Direction d)
{
	switch (d)
	{
	case Direction::Up:
		os << "Up";
		break;
	case Direction::Down:
		os << "Down";
		break;
	case Direction::Left:
		os << "Left";
		break;
	case Direction::Right:
		os << "Right";
		break;
	}
	return os;
}

struct Point
{
	int x;
	int y;

	bool operator<(const Point &other) const
	{
		return x < other.x || (x == other.x && y < other.y);
	}
};

struct Beam
{
	Point position;
	Direction direction;

	bool operator<(const Beam &other) const
	{
		return (
				position.x < other.position.x ||
				(position.x == other.position.x && position.y < other.position.y) ||
				(position.x == other.position.x && position.y == other.position.y && direction < other.direction));
	}
};

struct Grid
{
	std::vector<Tile> tiles;
	int width, height;

	Tile get(int x, int y) const
	{
		return tiles[y * width + x];
	}

	bool contains(int x, int y) const
	{
		return x >= 0 && x < width && y >= 0 && y < height;
	}
};

Grid parse(std::istream &is)
{
	Grid grid;
	grid.width = 0;
	grid.height = 0;

	std::string line;
	while (std::getline(is, line))
	{
		grid.width = line.size();
		grid.height++;
		for (char c : line)
		{
			grid.tiles.push_back(char_to_tile(c));
		}
	}

	return grid;
}

int energize(const Grid &g, Beam starting)
{
	std::set<Beam> closed;
	std::list<Beam> open;
	std::set<Point> passed;
	open.push_back(starting);
	while (!open.empty())
	{
		Beam beam = open.front();
		open.pop_front();
		if (!g.contains(beam.position.x, beam.position.y))
			continue;
		closed.insert(beam);
		passed.insert(beam.position);
		Tile tile = g.get(beam.position.x, beam.position.y);

		int dx, dy;
		switch (tile)
		{
		case Tile::Empty:
			std::tie(dx, dy) = direction_to_delta(beam.direction);
			beam.position.x += dx;
			beam.position.y += dy;
			if (!closed.contains(beam))
				open.push_back(beam);
			break;
		case Tile::ReflectorDexter:
			beam.direction = reflect_dexter(beam.direction);
			std::tie(dx, dy) = direction_to_delta(beam.direction);
			beam.position.x += dx;
			beam.position.y += dy;
			if (!closed.contains(beam))
				open.push_back(beam);
			break;
		case Tile::ReflectorSinister:
			beam.direction = reflect_sinister(beam.direction);
			std::tie(dx, dy) = direction_to_delta(beam.direction);
			beam.position.x += dx;
			beam.position.y += dy;
			if (!closed.contains(beam))
				open.push_back(beam);
			break;
		case Tile::SplitterHorizontal:
		{
			Beam beam1 = beam;
			Beam beam2 = beam;
			beam1.direction = Direction::Left;
			beam2.direction = Direction::Right;
			beam1.position.x--;
			beam2.position.x++;
			if (!closed.contains(beam1))
				open.push_back(beam1);
			if (!closed.contains(beam2))
				open.push_back(beam2);
		}
		break;
		case Tile::SplitterVertical:
		{
			Beam beam1 = beam;
			Beam beam2 = beam;
			beam1.direction = Direction::Up;
			beam2.direction = Direction::Down;
			beam1.position.y--;
			beam2.position.y++;
			if (!closed.contains(beam1))
				open.push_back(beam1);
			if (!closed.contains(beam2))
				open.push_back(beam2);
		}
		}
	}
	return passed.size();
}

void part1(const Grid &g)
{
	int energized = energize(g, Beam{{0, 0}, Direction::Right});
	std::cout << "Part 1: " << energized << std::endl;
}

void part2(const Grid &g)
{
	int max = 0;
	for (int i = 0; i < g.width; i++)
	{
		int curr = energize(g, Beam{{i, 0}, Direction::Down});
		if (curr > max)
			max = curr;
		curr = energize(g, Beam{{i, g.height - 1}, Direction::Up});
		if (curr > max)
			max = curr;
	}
	for (int j = 0; j < g.height; j++)
	{
		int curr = energize(g, Beam{{0, j}, Direction::Right});
		if (curr > max)
			max = curr;
		curr = energize(g, Beam{{g.width - 1, j}, Direction::Left});
		if (curr > max)
			max = curr;
	}
	std::cout << "Part 2: " << max << std::endl;
}

int main()
{
	const auto grid = parse(std::cin);
	part1(grid);
	part2(grid);
}