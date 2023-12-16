#include <iostream>
#include <string_view>
#include <string>
#include <vector>
#include <array>

struct Input
{
	std::string data;
	std::vector<std::string_view> pieces;
};

Input parse(std::istream &is)
{
	Input input;
	std::getline(is, input.data);
	std::string_view view(input.data);
	while (view.size() > 0)
	{
		auto pos = view.find_first_of(',');
		if (pos == std::string_view::npos)
		{
			input.pieces.push_back(view);
			break;
		}
		input.pieces.push_back(view.substr(0, pos));
		view = view.substr(pos + 1);
	}
	return input;
}

int hash(std::string_view view)
{
	int hash = 0;
	for (auto c : view)
	{
		hash += c;
		hash *= 17;
		hash %= 256;
	}
	return hash;
}

struct Lens
{
	std::string_view label;
	int focal_length;
};

struct hashmap
{
	std::array<std::vector<Lens>, 256> data;
};

void part1(const Input &input)
{
	int sum = 0;
	for (auto piece : input.pieces)
	{
		sum += hash(piece);
	}
	std::cout << "Part 1: " << sum << std::endl;
}

void part2(const Input &input)
{
	hashmap map;
	for (auto piece : input.pieces)
	{
		if (piece.back() == '-')
		{
			// remove
			auto label = piece.substr(0, piece.size() - 1);
			auto &bucket = map.data[hash(label)];
			for (int i = 0; i < bucket.size(); ++i)
			{
				if (bucket[i].label == label)
				{
					bucket.erase(bucket.begin() + i);
					break;
				}
			}
		}
		else
		{
			// add
			auto label = piece.substr(0, piece.size() - 2);
			auto focal_length = piece.back() - '0';
			auto &bucket = map.data[hash(label)];
			bool added = false;
			for (int i = 0; i < bucket.size(); ++i)
			{
				if (bucket[i].label == label)
				{
					bucket[i].focal_length = focal_length;
					added = true;
					break;
				}
			}
			if (!added)
				bucket.push_back({label, focal_length});
		}
	}

	int sum = 0;

	for (int b = 0; b < map.data.size(); b++)
	{
		auto &bucket = map.data[b];
		for (int s = 0; s < bucket.size(); s++)
		{
			auto &lens = bucket[s];
			sum += (b + 1) * (s + 1) * lens.focal_length;
		}
	}

	std::cout << "part 2: " << sum << std::endl;
}

int main()
{
	auto input = parse(std::cin);
	part1(input);
	part2(input);
}