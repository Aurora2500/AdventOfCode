#include <iostream>
#include <vector>

using u64 = u_int64_t;
using i64 = int64_t;

struct pos
{
	u64 i, j;
};

struct image
{
	std::vector<pos> galaxies;
	u64 width, height;
};

image parse(std::istream &input)
{
	image img;
	std::string line;

	std::getline(input, line);
	img.width = line.size();

	u64 i = 0;
	do
	{
		for (u64 j = 0; j < line.size(); j++)
			if (line[j] == '#')
				img.galaxies.push_back({i, j});
		i++;
	} while (std::getline(input, line));
	img.height = i;
	return std::move(img);
}

u64 manhattan(const pos &a, const pos &b)
{
	return std::abs((i64)a.i - (i64)b.i) + std::abs((i64)a.j - (i64)b.j);
}

void expand_count(image &img, u64 expansion, int part)
{
	std::vector<bool> emptyx(img.width, true), emptyy(img.height, true);
	for (auto &g : img.galaxies)
	{
		emptyx[g.j] = false;
		emptyy[g.i] = false;
	}
	std::vector<int> expandx(img.width, 0), expandy(img.height, 0);

	u64 xaccum = 0, yaccum = 0;
	for (int i = 0; i < img.width; i++)
	{
		if (emptyx[i])
			xaccum += expansion;
		expandx[i] = xaccum;
	}
	for (int i = 0; i < img.height; i++)
	{
		if (emptyy[i])
			yaccum += expansion;
		expandy[i] = yaccum;
	}

	u64 sum = 0;
	for (int i = 0; i < img.galaxies.size(); i++)
		for (int j = i + 1; j < img.galaxies.size(); j++)
		{
			pos a = img.galaxies[i];
			pos b = img.galaxies[j];
			a.i += expandy[a.i];
			a.j += expandx[a.j];
			b.i += expandy[b.i];
			b.j += expandx[b.j];
			sum += manhattan(a, b);
		}

	std::cout << "Part " << part << ": " << sum << std::endl;
}

int main()
{
	image img = parse(std::cin);
	expand_count(img, 1, 1);
	expand_count(img, 1000000 - 1, 2);
}