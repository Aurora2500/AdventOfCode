#include <iostream>
#include <unordered_set>
#include <vector>
#include <string>
#include <cctype>
#include <charconv>

struct card
{
	std::unordered_set<int> winning, playing;

	card(std::unordered_set<int> w, std::unordered_set<int> p) : winning(w), playing(p) {}

	int matching()
	{
		int matching = 0;
		for (int p : playing)
		{
			if (winning.contains(p))
				matching++;
		}
		return matching;
	}

	int points()
	{
		int n = matching();
		return n > 0 ? (1 << (n - 1)) : 0;
	}
};

std::vector<card> parse(std::istream &s)
{
	std::vector<card> acc;
	std::string line;
	while (std::getline(s, line))
	{
		std::string_view sv(line);
		size_t start_len = sv.find(':');
		sv.remove_prefix(start_len + 1);
		std::unordered_set<int> winning, playing;
		bool first_section = true;
		while (sv.length() > 0)
		{
			if (sv.at(0) == ' ')
			{
				sv.remove_prefix(1);
				continue;
			}
			if (sv.at(0) == '|')
			{
				sv.remove_prefix(1);
				first_section = false;
				continue;
			}
			size_t num_len = sv.find(' ');
			if (num_len == std::string_view::npos)
			{
				num_len = sv.length();
			}
			int num;
			std::from_chars(sv.data(), sv.data() + num_len, num);
			(first_section ? winning : playing).insert(num);
			sv.remove_prefix(num_len);
		}
		acc.emplace_back(winning, playing);
	}
	return acc;
}

void part2(std::vector<card> &cards)
{
	size_t total = cards.size();
	int *winnings = new int[total];
	for (int i = 0; i < total; i++)
		winnings[i] = 1;

	int acc = 0;
	for (int i = 0; i < total; i++)
	{
		int m = cards.at(i).matching();
		for (int j = 1; j <= m; j++)
		{
			winnings[i + j] += winnings[i];
		}
		acc += winnings[i];
	}

	delete[] winnings;

	std::cout << "Part 2: " << acc << std::endl;
}

void part1(std::vector<card> &cards)
{
	int acc = 0;
	for (card &c : cards)
	{
		acc += c.points();
	}
	std::cout << "Part 1: " << acc << std::endl;
}

int main()
{
	std::vector<card> cards = parse(std::cin);
	part1(cards);
	part2(cards);
}