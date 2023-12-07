#include <iostream>
#include <vector>
#include <string_view>

#include "util.hpp"

struct command
{
	int count;
	int from;
	int to;

	command(int count, int from, int to) : count(count), from(from), to(to) {}
};

class state
{
	std::vector<std::vector<char>> stacks;

public:
	state(std::vector<std::vector<char>> stacks) : stacks(stacks) {}

	void apply_stack_1(command cmd)
	{
		std::vector<char> &sourceStack = stacks.at(cmd.from);
		std::vector<char> &destStack = stacks.at(cmd.to);
		for (int i = 0; i < cmd.count; i++)
		{
			char crate = sourceStack.back();
			sourceStack.pop_back();
			destStack.push_back(crate);
		}
	}

	void apply_stack_2(command cmd)
	{
		std::vector<char> &sourceStack = stacks.at(cmd.from);
		std::vector<char> &destStack = stacks.at(cmd.to);
		for (int i = 0; i < cmd.count; i++)
		{
			char crate = sourceStack[sourceStack.size() - cmd.count + i];
			destStack.push_back(crate);
		}
		sourceStack.resize(sourceStack.size() - cmd.count);
	}

	std::string view_top()
	{
		std::string s;
		for (std::vector<char> &stack : stacks)
		{
			s.push_back(stack.empty() ? ' ' : stack.back());
		}
		return s;
	}
};

std::pair<state, std::vector<command>> parse(std::istream &s)
{
	std::vector<std::vector<char>> matrix;
	std::string line;
	while (std::getline(s, line))
	{
		if (std::isdigit(line[1]))
		{
			break;
		}
		matrix.emplace_back();
		std::vector<char> &mline = matrix.back();
		for (int j = 0; j < line.length(); j++)
		{
			mline.push_back(line[j]);
		}
	}

	int num_cols = (matrix[0].size() + 1) / 4;
	std::vector<std::vector<char>> stacks(num_cols);
	for (int j = 0; j < num_cols; j++)
	{
		for (int i = matrix.size() - 1; i >= 0; i--)
		{
			char c = matrix[i][j * 4 + 1];
			if (c == ' ')
			{
				break;
			}
			stacks[j].push_back(c);
		}
	}

	std::vector<command>
			cmds;

	std::getline(s, line);

	while (std::getline(s, line))
	{
		std::string_view sv(line);
		sv.remove_prefix(5);
		int count = take_int(sv);
		sv.remove_prefix(6);
		int from = take_int(sv);
		sv.remove_prefix(4);
		int to = take_int(sv);
		cmds.emplace_back(count, from - 1, to - 1);
	}

	return {state(stacks), cmds};
}

void part1(state &s, std::vector<command> &cmds)
{
	for (command &cmd : cmds)
	{
		s.apply_stack_1(cmd);
	}
	std::cout << "Part 1: " << s.view_top() << std::endl;
}

void part2(state &s, std::vector<command> &cmds)
{
	for (command &cmd : cmds)
	{
		s.apply_stack_2(cmd);
	}
	std::cout << "Part 2: " << s.view_top() << std::endl;
}

int main()
{
	auto [s1, cmds] = parse(std::cin);
	state s2 = s1;
	part1(s1, cmds);
	part2(s2, cmds);
}