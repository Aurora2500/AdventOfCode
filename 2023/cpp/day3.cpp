#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <map>
#include <charconv>
#include <memory>

struct part_number
{
	int number;
	int row;
	int start, end;
};

struct gear
{
	int numbers[2];
	int num_parts;
};

class schematic
{
public:
	std::vector<part_number> parts;
	std::vector<std::unordered_map<int, gear *>> symbols;
	std::vector<std::unique_ptr<gear>> gears;
};

schematic parse(std::istream &input)
{
	schematic s;
	s.gears.reserve(1000000);
	std::string line;
	int i = 0;
	while (std::getline(input, line))
	{
		s.symbols.emplace_back();
		std::unordered_map<int, gear *> &symbols = s.symbols.back();
		int j = 0;
		while (j < line.length())
		{
			if (line.at(j) == '.')
			{
				j++;
				continue;
			}
			if (std::isdigit(line.at(j)))
			{
				// collect whole number
				int num_len = 1;
				while (num_len + j < line.length() && std::isdigit(line.at(j + num_len)))
				{
					num_len++;
				}
				int num;
				std::from_chars(line.data() + j, line.data() + j + num_len, num);
				part_number pn = {.number = num, .row = i, .start = j, .end = j + num_len};
				s.parts.push_back(pn);
				j += num_len;
				continue;
			}
			// is symbol
			if (line.at(j) == '*')
			{
				gear g = {.numbers = {0}, .num_parts = 0};
				s.gears.emplace_back(std::make_unique<gear>(g));
				symbols.emplace(j, &*s.gears.back());
			}
			else
			{
				symbols.emplace(j, nullptr);
			}
			j++;
		}
		i++;
	}

	for (const part_number &pn : s.parts)
	{
		for (int i = std::max(0, pn.row - 1); i <= std::min((int)s.symbols.size() - 1, pn.row + 1); i++)
		{
			for (int j = std::max(0, pn.start - 1); j <= pn.end; j++)
			{
				if (s.symbols.at(i).contains(j))
				{
					gear *g = s.symbols.at(i).at(j);
					if (g == nullptr)
						continue;
					if (g->num_parts < 2)
					{
						g->numbers[g->num_parts] = pn.number;
					}
					g->num_parts += 1;
				}
			}
		}
	}

	return s;
}

void part2(const schematic &schem)
{
	int acc = 0;

	for (auto &g : schem.gears)
	{
		if (g->num_parts == 2)
		{
			acc += g->numbers[0] * g->numbers[1];
		}
	}
	std::cout << "Part 2: " << acc << std::endl;
}

void part1(schematic &schem)
{
	int sum = 0;

	for (const part_number &pn : schem.parts)
	{
		for (int i = std::max(0, pn.row - 1); i <= std::min((int)schem.symbols.size() - 1, pn.row + 1); i++)
		{
			for (int j = std::max(0, pn.start - 1); j <= pn.end; j++)
			{
				if (schem.symbols.at(i).contains(j))
				{
					sum += pn.number;
					goto end;
				}
			}
		}
	end:;
	}

	std::cout << "Part 1: " << sum << std::endl;
}

int main()
{
	schematic schem = parse(std::cin);
	part1(schem);
	part2(schem);
}