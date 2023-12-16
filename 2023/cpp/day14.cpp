#include <vector>
#include <iostream>

enum class Direction : int
{
	North = 0,
	West = 1,
	South = 2,
	East = 3,
};

enum class Space
{
	Empty,
	Square,
	Round,
};

struct Plate
{
	std::vector<Space> spaces;
	int width, height;

	Space &operator()(int x, int y)
	{
		return spaces[y * width + x];
	}

	const Space &operator()(int x, int y) const
	{
		return spaces[y * width + x];
	}

	void print() const
	{
		for (int i = 0; i < spaces.size(); i++)
		{
			switch (spaces[i])
			{
			case Space::Empty:
				std::cout << ' ';
				break;
			case Space::Square:
				std::cout << '#';
				break;
			case Space::Round:
				std::cout << 'O';
				break;
			}
			if ((i + 1) % width == 0)
				std::cout << '\n';
		}
		std::cout << std::endl;
	}

	int major_axis(Direction dir)
	{
		switch (dir)
		{
		case Direction::North:
		case Direction::South:
			return width;
		case Direction::West:
		case Direction::East:
			return height;
		}
	}

	int minor_axis(Direction dir)
	{
		switch (dir)
		{
		case Direction::North:
		case Direction::South:
			return height;
		case Direction::West:
		case Direction::East:
			return width;
		}
	}

	Space &from(int major, int minor, Direction dir)
	{
		switch (dir)
		{
		case Direction::North:
			return (*this)(major, minor);
		case Direction::West:
			return (*this)(minor, major);
		case Direction::South:
			return (*this)(major, height - minor - 1);
		case Direction::East:
			return (*this)(width - minor - 1, major);
		}
	}

	int calc_load() const
	{
		int sum = 0;
		for (int x = 0; x < width; x++)
		{
			for (int y = 0; y < height; y++)
			{
				if ((*this)(x, y) == Space::Round)
					sum += height - y;
			}
		}
		return sum;
	}
};

Plate parse(std::istream &is)
{
	Plate plate;

	std::string line;
	int height = 0;
	while (std::getline(is, line))
	{

		for (char c : line)
		{
			plate.width = line.size();
			switch (c)
			{
			case '.':
				plate.spaces.push_back(Space::Empty);
				break;
			case '#':
				plate.spaces.push_back(Space::Square);
				break;
			case 'O':
				plate.spaces.push_back(Space::Round);
				break;
			}
		}
		height++;
	}
	plate.height = height;

	return plate;
}

void part1(const Plate &plate)
{
	Plate copy = plate;

	int sum = 0;

	for (int j = 0; j < plate.width; j++)
	{
		int last_empty = 0;
		for (int i = 0; i < plate.height; i++)
		{
			switch (copy(j, i))
			{
			case Space::Round:
				sum += copy.height - last_empty;
				copy(j, i) = Space::Empty;
				copy(j, last_empty) = Space::Round;
				last_empty++;
				break;
			case Space::Square:
				last_empty = i + 1;
				break;
			case Space::Empty:
				break;
			}
		}
	}

	std::cout << "Part 1: " << sum << std::endl;
}

void part2(const Plate &plate)
{
	Plate copy = plate;

	constexpr int max_cycles = 1000000000;
	constexpr int min_repetition_check = 10;
	constexpr int min_period = 2;
	constexpr int period_check = 2;
	std::vector<int> loads;
	int result = 0;

	for (int cycle = 0; cycle < max_cycles; cycle++)
	{
		for (int d = 0; d < 4; d++)
		{
			Direction dir = static_cast<Direction>(d);
			int major = copy.major_axis(dir);
			int minor = copy.minor_axis(dir);
			for (int i = 0; i < major; i++)
			{
				int last_empty = 0;
				for (int j = 0; j < minor; j++)
				{
					switch (copy.from(i, j, dir))
					{
					case Space::Round:
						copy.from(i, j, dir) = Space::Empty;
						copy.from(i, last_empty, dir) = Space::Round;
						last_empty++;
						break;
					case Space::Square:
						last_empty = j + 1;
						break;
					case Space::Empty:
						break;
					}
				}
			}
		}
		int load = copy.calc_load();
		loads.push_back(load);
		if (cycle >= min_repetition_check)
		{
			for (int possible_period = min_period; possible_period < std::min(cycle / 10, min_period * 10); possible_period++)
			{
				bool matches = true;
				for (int j = 0; j < period_check; j++)
				{
					if (loads[cycle - j * possible_period] != loads[cycle - (j + 1) * possible_period])
					{
						matches = false;
						break;
					}
				}
				if (matches)
				{
					// we found the period, extrapolate to the end
					int cycles_left = max_cycles - cycle - 1;
					int period_offset = cycles_left % possible_period;
					result = loads[cycle + period_offset - possible_period];
					goto end;
				}
			}
		}
	}
end:
	std::cout << "Part 2: " << result << std::endl;
}

int main()
{
	auto plate = parse(std::cin);
	part1(plate);
	part2(plate);
}