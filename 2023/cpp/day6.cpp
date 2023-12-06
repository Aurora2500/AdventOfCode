#include <vector>
#include <iostream>
#include <charconv>
#include <cmath>

using u64 = u_int64_t;

struct result
{
	u64 time;
	u64 distance;
};

u64 take_int(std::string_view &sv)
{
	size_t prefix_len = 0;
	while (!std::isdigit(sv[prefix_len]))
	{
		prefix_len++;
	}
	sv.remove_prefix(prefix_len);

	size_t end = 0;
	while (end < sv.length() && std::isdigit(sv[end]))
	{
		end++;
	}

	u64 x;
	std::from_chars(sv.data(), sv.data() + end, x);
	sv.remove_prefix(end);
	return x;
}

std::vector<result> parse(std::istream &input)
{
	std::vector<result> results;
	std::string line1, line2;
	std::getline(input, line1);
	std::getline(input, line2);

	std::string_view sv1(line1), sv2(line2);

	while (sv1.length() > 0)
	{
		u64 time = take_int(sv1);
		u64 distance = take_int(sv2);
		results.push_back({time, distance});
	}
	return results;
}

u64 faster(u64 t, u64 d)
{
	double tf = t;
	double df = d;
	double xmin = 0.5 * (tf - std::sqrt(tf * tf - 4 * df));
	u64 xlo = std::floor(xmin) + 1;
	u64 xhi = t - xlo;
	return xhi - xlo + 1;
}

void part1(std::vector<result> &results)
{
	u64 prod = 1;
	for (auto &r : results)
	{
		prod *= faster(r.time, r.distance);
	}

	std::cout << "Part 1: " << prod << std::endl;
}

void part2(std::vector<result> &results)
{
	u64 real_time = 0;
	u64 real_dist = 0;
	for (auto &r : results)
	{
		u64 t_digits = std::floor(std::log10(r.time));
		u64 d_digits = std::floor(std::log10(r.distance));
		real_time *= std::pow(10, t_digits + 1);
		real_dist *= std::pow(10, d_digits + 1);
		real_time += r.time;
		real_dist += r.distance;
	}

	u64 r = faster(real_time, real_dist);
	std::cout << "Part 2: " << r << std::endl;
}

int main()
{
	std::vector<result> results = parse(std::cin);
	part1(results);
	part2(results);
}