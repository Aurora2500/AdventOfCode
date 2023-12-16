#include <iostream>
#include <vector>

enum class Floor
{
	Ash,
	Rock,
};

struct image
{
	std::vector<Floor> floor;
	int width;

	int height() const
	{
		return floor.size() / width;
	}

	const Floor &at(int x, int y) const
	{
		return floor[y * width + x];
	}
};

std::vector<image> parse(std::istream &is)
{
	std::vector<image> images;
	std::string line;
	images.emplace_back();
	while (std::getline(is, line))
	{
		if (line.empty())
		{
			images.emplace_back();
			continue;
		}

		if (images.back().width == 0)
			images.back().width = line.size();

		for (auto c : line)
		{
			switch (c)
			{
			case '.':
				images.back().floor.push_back(Floor::Ash);
				break;
			case '#':
				images.back().floor.push_back(Floor::Rock);
				break;
			default:
				throw std::runtime_error("invalid character");
			}
		}
	}
	return images;
}

void part1(const std::vector<image> &images)
{
	int sum = 0;
	for (auto &image : images)
	{
		// check for horizontal symmetry
		for (int x = 0; x < image.width - 1; x++)
		{
			bool hsym = true;
			// sym is between x & x+1
			int max_x = std::min(x + 1, image.width - x - 1);

			for (int j = 0; j < max_x; j++)
			{
				for (int i = 0; i < image.height(); i++)
				{
					if (image.at(x + 1 + j, i) != image.at(x - j, i))
					{
						hsym = false;
						break;
					}
				}
				if (!hsym)
					break;
			}
			if (hsym)
			{
				sum += x + 1;
				break;
			}
		}

		// check for vertical symmetry
		for (int y = 0; y < image.height() - 1; y++)
		{
			bool vsym = true;
			// sym is between x & x+1
			int max_y = std::min(y + 1, image.height() - y - 1);

			for (int i = 0; i < max_y; i++)
			{
				for (int j = 0; j < image.width; j++)
				{
					if (image.at(j, y - i) != image.at(j, y + 1 + i))
					{
						vsym = false;
						break;
					}
				}
				if (!vsym)
					break;
			}
			if (vsym)
			{
				sum += (y + 1) * 100;
				break;
			}
		}
	}

	std::cout << "Part 1: " << sum << std::endl;
}

void part2(const std::vector<image> &images)
{
	int sum = 0;
	for (auto &image : images)
	{
		// check for horizontal symmetry
		for (int x = 0; x < image.width - 1; x++)
		{
			int hsym = 0;
			// sym is between x & x+1
			int max_x = std::min(x + 1, image.width - x - 1);

			for (int j = 0; j < max_x; j++)
			{
				for (int i = 0; i < image.height(); i++)
				{
					if (image.at(x + 1 + j, i) != image.at(x - j, i))
					{
						hsym++;
					}
				}
				if (hsym >= 2)
					break;
			}
			if (hsym == 1)
			{
				sum += x + 1;
				break;
			}
		}

		// check for vertical symmetry
		for (int y = 0; y < image.height() - 1; y++)
		{
			int vsym = 0;
			// sym is between x & x+1
			int max_y = std::min(y + 1, image.height() - y - 1);

			for (int i = 0; i < max_y; i++)
			{
				for (int j = 0; j < image.width; j++)
				{
					if (image.at(j, y - i) != image.at(j, y + 1 + i))
					{
						vsym++;
					}
				}
				if (vsym >= 2)
					break;
			}
			if (vsym == 1)
			{
				sum += (y + 1) * 100;
				break;
			}
		}
	}

	std::cout << "Part 2: " << sum << std::endl;
}

int main()
{
	auto images = parse(std::cin);
	part1(images);
	part2(images);
}