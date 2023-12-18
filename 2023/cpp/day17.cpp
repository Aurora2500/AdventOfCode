#include <queue>
#include <vector>
#include <iostream>
#include <set>
#include <map>

enum class Direction
{
	Up,
	Down,
	Left,
	Right
};

struct pos
{
	int x;
	int y;

	bool operator==(const pos &other) const
	{
		return x == other.x && y == other.y;
	}

	bool operator<(const pos &other) const
	{
		return x < other.x || (x == other.x && y < other.y);
	}

	int manhattan(const pos &other) const
	{
		return std::abs(x - other.x) + std::abs(y - other.y);
	}

	pos step(Direction d)
	{
		switch (d)
		{
		case Direction::Up:
			return {x, y - 1};
		case Direction::Down:
			return {x, y + 1};
		case Direction::Left:
			return {x - 1, y};
		case Direction::Right:
			return {x + 1, y};
		}
	}

	friend std::ostream &operator<<(std::ostream &os, const pos &p)
	{
		return os << "(" << p.x << ", " << p.y << ")";
	}
};

struct map
{
	std::vector<int> loss;
	int width;

	bool in_bounds(int x, int y) const
	{
		return x >= 0 && x < width && y >= 0 && y < loss.size() / width;
	}

	bool in_bounds(pos p) const
	{
		return in_bounds(p.x, p.y);
	}

	int &at(int x, int y)
	{
		return loss[y * width + x];
	}

	const int &at(int x, int y) const
	{
		return loss[y * width + x];
	}

	int &at(pos p)
	{
		return at(p.x, p.y);
	}

	const int &at(pos p) const
	{
		return at(p.x, p.y);
	}
};

Direction turn_cw(Direction d)
{
	switch (d)
	{
	case Direction::Up:
		return Direction::Right;
	case Direction::Down:
		return Direction::Left;
	case Direction::Left:
		return Direction::Up;
	case Direction::Right:
		return Direction::Down;
	}
}

Direction turn_ccw(Direction d)
{
	switch (d)
	{
	case Direction::Up:
		return Direction::Left;
	case Direction::Down:
		return Direction::Right;
	case Direction::Left:
		return Direction::Down;
	case Direction::Right:
		return Direction::Up;
	}
}

struct state_lossless
{
	pos p;
	Direction d;
	int straight;

	bool operator<(const state_lossless &other) const
	{
		return p < other.p || (p == other.p && d < other.d) || (p == other.p && d == other.d && straight < other.straight);
	}
};

struct state
{
	pos p;
	Direction d;
	int straight;
	int loss;
	int heuristic;

	bool operator<(const state &other) const
	{
		return (loss + heuristic) > (other.loss + other.heuristic);
	}

	state_lossless lossless() const
	{
		return {p, d, straight};
	}

	state(pos p, Direction d, int straight, int loss, int heuristic) : p(p), d(d), straight(straight), loss(loss), heuristic(heuristic) {}
	state(state_lossless s, int loss, int heuristic) : p(s.p), d(s.d), straight(s.straight), loss(loss), heuristic(heuristic) {}
};

map parse(std::istream &is)
{
	map m;
	std::string line;
	while (std::getline(is, line))
	{
		m.width = line.size();
		for (auto c : line)
			m.loss.push_back(c - '0');
	}
	return m;
}

void part1(const map &map)
{
	std::priority_queue<state> q;
	std::set<state_lossless> visited;
	std::map<state_lossless, state_lossless> came_from;
	pos end{map.width - 1, (static_cast<int>(map.loss.size()) / map.width) - 1};
	q.emplace(pos{1, 0}, Direction::Right, 0, map.at(1, 0), end.manhattan(pos{1, 0}));
	q.emplace(pos{0, 1}, Direction::Down, 0, map.at(0, 1), end.manhattan(pos{0, 1}));
	int final_loss = 0;
	state_lossless final_state;

	while (!q.empty())
	{
		auto s = q.top();
		q.pop();

		if (s.p == end)
		{
			final_loss = s.loss;
			final_state = s.lossless();
			break;
		}

		if (visited.contains(s.lossless()))
			continue;
		visited.insert(s.lossless());

		auto cw = turn_cw(s.d);
		auto step_cw = s.p.step(cw);
		auto state_cw = state_lossless{step_cw, cw, 0};
		if (map.in_bounds(step_cw) && !visited.contains(state_cw))
		{
			q.emplace(state_cw, s.loss + map.at(step_cw), end.manhattan(step_cw));
			came_from[state_cw] = s.lossless();
		}
		auto ccw = turn_ccw(s.d);
		auto step_ccw = s.p.step(ccw);
		auto state_ccw = state_lossless{step_ccw, ccw, 0};
		if (map.in_bounds(step_ccw) && !visited.contains(state_ccw))
		{
			q.emplace(state_ccw, s.loss + map.at(step_ccw), end.manhattan(step_ccw));
			came_from[state_ccw] = s.lossless();
		}

		auto step_forward = s.p.step(s.d);
		auto state_forward = state_lossless{step_forward, s.d, s.straight + 1};
		if (s.straight < 2 && map.in_bounds(step_forward) && !visited.contains(state_forward))
		{
			q.emplace(state_forward, s.loss + map.at(step_forward), end.manhattan(step_forward));
			came_from[state_forward] = s.lossless();
		}
	}
	std::cout << "Part 1: " << final_loss << std::endl;
	// int l = final_loss;
	// while (came_from.contains(final_state) && final_state.p != pos{0, 0})
	// {
	// std::cout << final_state.p.x << " " << final_state.p.y << " (" << l << ")" << std::endl;
	// l -= map.at(final_state.p);
	// final_state = came_from.at(final_state);
	// }
}

void part2(const map &map)
{
	std::priority_queue<state> q;
	std::set<state_lossless> visited;
	std::map<state_lossless, state_lossless> came_from;
	pos end{map.width - 1, (static_cast<int>(map.loss.size()) / map.width) - 1};
	q.emplace(pos{1, 0}, Direction::Right, 0, map.at(1, 0), end.manhattan(pos{1, 0}));
	q.emplace(pos{0, 1}, Direction::Down, 0, map.at(0, 1), end.manhattan(pos{0, 1}));
	int final_loss = 0;
	state_lossless final_state;

	while (!q.empty())
	{
		auto s = q.top();
		q.pop();

		if (s.p == end && s.straight > 2)
		{
			final_loss = s.loss;
			final_state = s.lossless();
			break;
		}

		if (visited.contains(s.lossless()))
			continue;
		visited.insert(s.lossless());

		auto cw = turn_cw(s.d);
		auto step_cw = s.p.step(cw);
		auto state_cw = state_lossless{step_cw, cw, 0};
		if (map.in_bounds(step_cw) && s.straight > 2 && !visited.contains(state_cw))
		{
			q.emplace(state_cw, s.loss + map.at(step_cw), end.manhattan(step_cw));
			came_from[state_cw] = s.lossless();
		}
		auto ccw = turn_ccw(s.d);
		auto step_ccw = s.p.step(ccw);
		auto state_ccw = state_lossless{step_ccw, ccw, 0};
		if (map.in_bounds(step_ccw) && s.straight > 2 && !visited.contains(state_ccw))
		{
			q.emplace(state_ccw, s.loss + map.at(step_ccw), end.manhattan(step_ccw));
			came_from[state_ccw] = s.lossless();
		}

		auto step_forward = s.p.step(s.d);
		auto state_forward = state_lossless{step_forward, s.d, s.straight + 1};
		if (s.straight < 9 && map.in_bounds(step_forward) && !visited.contains(state_forward))
		{
			q.emplace(state_forward, s.loss + map.at(step_forward), end.manhattan(step_forward));
			came_from[state_forward] = s.lossless();
		}
	}
	std::cout << "Part 2: " << final_loss << std::endl;

	// int l = final_loss;
	// while (came_from.contains(final_state) && final_state.p != pos{0, 0})
	// {
	// std::cout << final_state.p << " (" << l << ")" << std::endl;
	// l -= map.at(final_state.p);
	// final_state = came_from.at(final_state);
	// }
	// std::cout << final_state.p << " (" << l << ")" << std::endl;
}

int main()
{
	auto map = parse(std::cin);
	part1(map);
	part2(map);
}