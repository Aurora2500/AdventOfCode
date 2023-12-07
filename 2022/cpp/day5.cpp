#include <iostream>
#include <vector>
#include <string_view>

struct command
{
	int count;
	int from;
	int to;
};

class state
{
	std::vector<std::vector<char>> stacks;

public:
	state(std::vector<std::vector<char>> stacks) : stacks(stacks) {}

	void apply_stack(command cmd)
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

	std::string view_top()
	{
		std::string s;
		for (std::vector<char> &stack : stacks)
		{
			if (stack.empty())
			{
				s.push_back(' ');
			}
			else
			{
				s.push_back(stack.back());
			}
		}
		return s;
	}
};

state build_state(std::istream s)
{
	std::vector<std::vector<char>> matrix;
	std::string line;
	while (std::getline(s, line))
	{
		matrix.emplace_back();
		std::vector<char> &mline = matrix.back();
		std::string_view sv(line);
		if (std::isdigit(sv.at(1)))
		{
			break;
		}
	}

	std::vector<std::vector<char>> stacks;

	return state(stacks);
}

void part1();

int main()
{
	part1();
}