#include <iostream>
#include <vector>
#include <list>

enum class direction
{
	Up,
	Down,
	Left,
	Right,
	NumDirs,
};

direction opposite(direction dir)
{
	switch (dir)
	{
	case direction::Up:
		return direction::Down;
	case direction::Down:
		return direction::Up;
	case direction::Left:
		return direction::Right;
	case direction::Right:
		return direction::Left;
	}
}

struct pos
{
	int x, y;
};

struct pipe
{
	bool dirs[(int)direction::NumDirs];

	pipe(char c)
	{
		for (int i = 0; i < (int)direction::NumDirs; i++)
			dirs[i] = false;
		switch (c)
		{
		case '|':
			dirs[(int)direction::Up] = true;
			dirs[(int)direction::Down] = true;
			break;
		case '-':
			dirs[(int)direction::Left] = true;
			dirs[(int)direction::Right] = true;
			break;
		case '7':
			dirs[(int)direction::Down] = true;
			dirs[(int)direction::Left] = true;
			break;
		case 'J':
			dirs[(int)direction::Up] = true;
			dirs[(int)direction::Left] = true;
			break;
		case 'L':
			dirs[(int)direction::Up] = true;
			dirs[(int)direction::Right] = true;
			break;
		case 'F':
			dirs[(int)direction::Down] = true;
			dirs[(int)direction::Right] = true;
			break;
		case 'S':
			for (int i = 0; i < (int)direction::NumDirs; i++)
				dirs[i] = true;
			break;
		}
	}
};

std::pair<std::vector<std::vector<pipe>>, pos> parse(std::istream &input)
{
	std::vector<std::vector<pipe>> res;
	pos start;
	std::string line;
	int i = 0;
	while (std::getline(input, line))
	{
		res.emplace_back();
		int j = 0;
		for (char &c : line)
		{
			res.back().emplace_back(c);
			if (c == 'S')
			{
				start.y = i;
				start.x = j;
			}
			j++;
		}
		i++;
	}
	return {res, start};
}

void part1(const std::vector<std::vector<pipe>> &pipes, const pos &start)
{
	std::vector<std::vector<int>> dists;
	for (int i = 0; i < pipes.size(); i++)
	{
		dists.emplace_back(pipes[i].size(), -1);
	}
	std::list<pos> q;
	int max_dist = 0;
	q.push_back(start);
	dists[start.y][start.x] = 0;
	while (q.size() > 0)
	{
		pos curr = q.front();
		q.pop_front();
		int dist = dists[curr.y][curr.x];
		for (int i = 0; i < (int)direction::NumDirs; i++)
		{
			if (pipes[curr.y][curr.x].dirs[i])
			{
				pos next = curr;
				direction curr_dir = (direction)i;
				direction opposite_dir = opposite(curr_dir);
				switch (curr_dir)
				{
				case direction::Up:
					next.y--;
					break;
				case direction::Down:
					next.y++;
					break;
				case direction::Left:
					next.x--;
					break;
				case direction::Right:
					next.x++;
					break;
				}
				if (next.y >= 0 && next.y < pipes.size() && next.x >= 0 && next.x < pipes[next.y].size() && pipes[next.y][next.x].dirs[(int)opposite_dir] && dists[next.y][next.x] == -1)
				{
					int d = dist + 1;
					dists[next.y][next.x] = d;
					q.push_back(next);
					if (d > max_dist)
						max_dist = d;
				}
			}
		}
	}

	std::cout << "Part 1: " << max_dist << std::endl;
}

void part2(std::vector<std::vector<pipe>> &pipes, pos &start)
{
	std::vector<std::vector<bool>> visited;
	for (int i = 0; i < pipes.size(); i++)
	{
		visited.emplace_back(pipes[i].size(), false);
	}
	std::list<pos> q;
	q.push_back(start);
	while (q.size() > 0)
	{
		pos curr = q.front();
		q.pop_front();
		visited[curr.y][curr.x] = true;
		for (int i = 0; i < (int)direction::NumDirs; i++)
		{
			if (pipes[curr.y][curr.x].dirs[i])
			{
				pos next = curr;
				direction curr_dir = (direction)i;
				direction opposite_dir = opposite(curr_dir);
				switch (curr_dir)
				{
				case direction::Up:
					next.y--;
					break;
				case direction::Down:
					next.y++;
					break;
				case direction::Left:
					next.x--;
					break;
				case direction::Right:
					next.x++;
					break;
				}
				if (next.y >= 0 && next.y < pipes.size() && next.x >= 0 && next.x < pipes[next.y].size() && pipes[next.y][next.x].dirs[(int)opposite_dir] && !visited[next.y][next.x])
				{
					q.push_back(next);
				}
			}
		}
	}

	int inside = 0;
	for (int i = 0; i < pipes.size(); i++)
	{
		bool parity = false;
		for (int j = 0; j < pipes[i].size(); j++)
		{
			if (visited[i][j] && pipes[i][j].dirs[(int)direction::Up])
			{
				if (start.x == j && start.y == i)
				{
					// the start has an up pipe, so we need to check if it actually connects to anything to update the parity
					if (i > 0 && pipes[i - 1][j].dirs[(int)direction::Down])
						parity = !parity;
				}
				else
					parity = !parity;
			}
			if (!visited[i][j] && parity)
			{
				inside++;
				std::cout << "Inside at " << i + 1 << ", " << j + 1 << std::endl;
			}
		}
	}
	std::cout << "Part 2: " << inside << std::endl;
}

int main()
{
	auto [pipes, start] = parse(std::cin);
	part1(pipes, start);
	part2(pipes, start);
}