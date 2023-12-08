#include <iostream>
#include <memory>
#include <unordered_map>
#include <vector>
#include <numeric>

class node;
enum class direction;

using noderef = std::shared_ptr<node>;
using nodemap = std::unordered_map<std::string, noderef>;
using directions = std::vector<direction>;

enum class direction
{
	Left,
	Right
};

class node
{
	std::string name;
	noderef left, right;

public:
	node(std::string name) : name(name) {}
	node(std::string name, noderef left, noderef right) : name(name), left(left), right(right) {}
	noderef go(direction dir)
	{
		switch (dir)
		{
		case direction::Left:
			return left;
		case direction::Right:
			return right;
		}
	}

	std::string &get_name() { return name; }

	friend std::pair<nodemap, directions> parse(std::istream &);
};

std::pair<nodemap, directions> parse(std::istream &input)
{
	std::string line;
	directions dirs;

	std::getline(input, line);
	for (char &c : line)
	{
		if (c == 'L')
			dirs.push_back(direction::Left);
		else if (c == 'R')
			dirs.push_back(direction::Right);
	}

	std::getline(input, line);

	nodemap map;

	while (std::getline(input, line))
	{
		std::string src = line.substr(0, 3);
		std::string leftname = line.substr(7, 3);
		std::string rightname = line.substr(12, 3);

		noderef curr, left, right;

		auto leftit = map.find(leftname);
		if (leftit == map.end())
		{
			left = std::make_shared<node>(leftname);
			map.insert({leftname, left});
		}
		else
		{
			left = leftit->second;
		}

		auto rightit = map.find(rightname);
		if (rightit == map.end())
		{
			right = std::make_shared<node>(rightname);
			map.insert({rightname, right});
		}
		else
		{
			right = rightit->second;
		}

		auto currit = map.find(src);
		if (currit == map.end())
		{
			curr = std::make_shared<node>(src, left, right);
			map.insert({src, curr});
		}
		else
		{
			curr = currit->second;
		}
		curr->left = left;
		curr->right = right;
	}

	return {map, dirs};
}

void part1(nodemap &map, directions &dirs)
{
	const std::string goal("ZZZ");
	const std::string start("AAA");
	noderef curr = map[start];
	int i = 0;
	while (true)
	{
		direction dir = dirs[i % dirs.size()];
		curr = curr->go(dir);
		i++;
		if (curr->get_name() == goal)
		{
			break;
		}
	}

	std::cout << "Part 1: " << i << std::endl;
}

void part2(nodemap &map, directions &dirs)
{
	std::vector<noderef> currs;
	for (auto kv : map)
	{
		if (kv.first.ends_with("A"))
		{
			currs.push_back(kv.second);
		}
	}
	std::vector<int> reached_at(currs.size());
	int i = 0;
	while (true)
	{
		direction dir = dirs[i % dirs.size()];
		int numz = 0;
		for (int j = 0; j < currs.size(); j++)
		{
			currs[j] = currs[j]->go(dir);
			if (currs[j]->get_name().ends_with("Z") && reached_at[j] == 0)
			{
				reached_at[j] = i + 1;
			}
		}
		i++;
		bool should_break = true;
		for (auto &n : reached_at)
			if (n == 0)
				should_break = false;
		if (should_break)
			break;
	}
	u_int64_t lcm = 1;
	for (auto &n : reached_at)
		lcm = std::lcm(lcm, n);
	std::cout << "Part 2: " << lcm << std::endl;
}

int main()
{
	auto [map, dirs] = parse(std::cin);
	part1(map, dirs);
	part2(map, dirs);
}