#include <iostream>
#include <vector>
#include <string>
#include <queue>
#include <array>
#include <unordered_set>
#include <variant>

enum class Tile
{
	Plot,
	Rock,
};

enum class Direction
{
	Up,
	Down,
	Left,
	Right,
};

constexpr std::array<Direction, 4> all_dirs = {Direction::Up, Direction::Down, Direction::Left, Direction::Right};

enum class Corner
{
	TopLeft,
	TopRight,
	BottomLeft,
	BottomRight,
};

std::array<Direction, 2> directions(Corner c)
{
	switch (c)
	{
	case Corner::TopLeft:
		return {Direction::Right, Direction::Down};
	case Corner::TopRight:
		return {Direction::Left, Direction::Down};
	case Corner::BottomLeft:
		return {Direction::Right, Direction::Up};
	case Corner::BottomRight:
		return {Direction::Left, Direction::Up};
	}
}

struct Point
{
	int x;
	int y;

	bool operator==(const Point &other) const
	{
		return x == other.x && y == other.y;
	}

	bool parity() const
	{
		return (x + y) % 2;
	}

	Point step(Direction d) const
	{
		switch (d)
		{
		case Direction::Up:
			return {x, y + 1};
		case Direction::Down:
			return {x, y - 1};
		case Direction::Left:
			return {x - 1, y};
		case Direction::Right:
			return {x + 1, y};
		}
	}
};

template <>
struct std::hash<Point>
{
	std::size_t operator()(const Point &p) const
	{
		return 773 * p.x + 797 * p.y;
	}
};

struct Node
{
	Point p;
	int steps;
};

struct MetaNode
{
	Point pos;
	std::variant<Point, Corner> start;
	int steps;
};

struct Map
{
	std::vector<Tile> tiles;
	int width;
	int height;
	Point start;

	const Tile &operator[](const Point &p) const
	{
		return tiles[p.y * width + p.x];
	}

	bool bounds(const Point &p) const
	{
		return p.x >= 0 && p.x < width && p.y >= 0 && p.y < height;
	}

	int parity(bool even) const
	{
		int count = 0;
		for (int y = 0; y < height; y++)
		{
			for (int x = even ? 0 : 1; x < width; x += 2)
			{
				if ((*this)[{x, y}] == Tile::Plot)
				{
					Point p = {x, y};
					if (p.parity() == even)
						count++;
				}
			}
		}
		return count;
	}
};

Map parse(std::istream &is)
{
	Map m;
	std::string line;
	while (std::getline(is, line))
	{
		m.width = line.length();
		int i = 0;
		for (char c : line)
		{
			if (c == '.' || c == 'S')
			{
				if (c == 'S')
				{
					m.start = {i, m.height};
				}
				m.tiles.push_back(Tile::Plot);
			}
			else
			{
				m.tiles.push_back(Tile::Rock);
			}
			i++;
		}
		m.height++;
	}
	return m;
}

void part1(const Map &map, int max_steps)
{
	std::unordered_set<Point> visited;
	std::queue<Node> q;
	q.push(Node{map.start, 0});

	while (!q.empty())
	{
		Node n = q.front();
		q.pop();
		if (visited.contains(n.p))
			continue;
		visited.insert(n.p);
		if (n.steps >= max_steps)
			continue;
		std::array<Direction, 4> dirs = {Direction::Up, Direction::Down, Direction::Left, Direction::Right};
		for (auto &dir : dirs)
		{
			auto next = n.p.step(dir);
			if (!map.bounds(next) || map[next] == Tile::Rock)
				continue;
			q.push(Node{next, n.steps + 1});
		}
	}

	int count = 0;
	bool parity = map.start.parity() != (max_steps % 2);

	for (auto &p : visited)
	{
		if (p.parity() == parity)
			count++;
	}
	std::cout << "Part 1: " << count << std::endl;
}

void part2(const Map &map, int max_steps)
{
	int rule_of_thumb = 2 * map.width + 2 * map.height;
	u_int64_t count = 0;
	std::array<int, 2> parities = {map.parity(true), map.parity(false)};

	std::unordered_set<Point> v_meta;
	std::queue<MetaNode> q_meta;

	q_meta.push(MetaNode{{0, 0}, map.start, 0});

	while (!q_meta.empty())
	{
		auto mcurr = q_meta.front();
		q_meta.pop();
		if (v_meta.contains(mcurr.pos))
			continue;
		v_meta.insert(mcurr.pos);
		if (std::holds_alternative<Corner>(mcurr.start))
		{
			for (auto dir : all_dirs)
			{
				auto next = mcurr.pos.step(dir);
				int len = (dir == Direction::Up || dir == Direction::Down) ? map.height : map.width;
				if (mcurr.steps + len < max_steps)
				{
					q_meta.push(MetaNode{next, mcurr.start, mcurr.steps + len});
				}
			}
		}
		else
		{
			std::unordered_set<Point> v;
			std::queue<Node> q;
			q.push(Node{std::get<Point>(mcurr.start), mcurr.steps});
			while (!q.empty())
			{
				Node curr = q.front();
				q.pop();
				if (v.contains(curr.p))
					continue;
				v.insert(curr.p);
			}
		}
	}

	std::cout << "Part 2: " << count << std::endl;
}

int main()
{
	auto map = parse(std::cin);
	part1(map, 64);

	std::vector<int> steps = {6, 10, 50, 100, 500, 1000, 5000};
	for (auto s : steps)
	{
		part2(map, s);
	}
}