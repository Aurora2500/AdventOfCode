#include <iostream>
#include <vector>
#include <charconv>
#include <algorithm>

class sequence;

class range
{
	u_int64_t start, length;

	friend class remap;
	friend class range_container;

public:
	range(u_int64_t start, u_int64_t length) : start(start), length(length) {}
	u_int64_t lowest() { return start; }
};

class range_container
{
	std::vector<range> ranges;

	friend class remap;
	friend range_container parse_ranges(std::vector<u_int64_t> &xs);
	friend void part2(sequence &seq, range_container &rc);

public:
	void extend(range_container other)
	{
		for (auto r : other.ranges)
			ranges.push_back(r);
	}

	int size()
	{
		return ranges.size();
	}

	void merge()
	{
		// sort ranges
		std::sort(ranges.begin(), ranges.end(), [](range &a, range &b)
							{ return a.start < b.start; });
		// merge ranges
		// for current range, check if it overlaps with the next range
		for (size_t i = 0; i < ranges.size() - 1; i++)
		{
			for (size_t j = i + 1; j < ranges.size(); j++)
			{
				if (ranges[i].start + ranges[i].length >= ranges[j].start)
				{
					// merge ranges
					ranges[i].length = std::max(ranges[i].start + ranges[i].length, ranges[j].start + ranges[j].length) - ranges[i].start;
					// remove range
					ranges.erase(ranges.begin() + j);
					j--;
				}
				else
					break;
			}
		}
	}
};

class remap
{
	u_int64_t src, dest, length;

public:
	remap(u_int64_t source, u_int64_t dest, u_int64_t length) : src(source), dest(dest), length(length) {}

	bool contains(u_int64_t x)
	{
		return x >= src && x < src + length;
	}

	u_int64_t forward(u_int64_t x)
	{
		if (contains(x))
			return dest + x - src;
		return x;
	}

	range_container forward(range_container &rc)
	{
		range_container result;
		range_container extra_range;
		for (int i = 0; i < rc.ranges.size(); i++)
		{
			auto &r = rc.ranges[i];
			// check for intersection with the remap
			if (r.start + r.length > src && r.start < src + length)
			{
				u_int64_t start = std::max(src, r.start);
				u_int64_t len = std::min(src + length - start, r.length);
				result.ranges.push_back({dest + start - src, len});
				if (r.start < src)
				{
					// has a bit before
					extra_range.ranges.push_back({r.start, src - 1});
				}
				if (r.start + r.length > src + length)
				{
					// has a bit after
					u_int64_t ns = src + length;
					extra_range.ranges.push_back({ns, r.start + r.length - ns});
				}
				rc.ranges.erase(rc.ranges.begin() + i);
				i--;
			}
		}
		rc.extend(extra_range);
		return result;
	}

	friend class remap_layer;
};

class remap_layer
{
	std::vector<remap> remaps;

public:
	u_int64_t forward(u_int64_t x)
	{
		for (auto &r : remaps)
		{
			if (r.contains(x))
				return r.forward(x);
		}
		return x;
	}

	range_container forward(const range_container &rc)
	{
		range_container rcc = rc;
		rcc.merge();
		range_container result;
		for (auto &r : remaps)
		{
			result.extend(r.forward(rcc));
		}
		result.extend(rcc);
		result.merge();
		return result;
	}

	void index()
	{
		// sort remaps
		std::sort(remaps.begin(), remaps.end(), [](remap &a, remap &b)
							{ return a.src < b.src; });
	}

	void add_remap(remap &r)
	{
		remaps.push_back(r);
	}
};

class sequence
{
	std::vector<remap_layer> layers;

public:
	u_int64_t forward(u_int64_t x)
	{
		for (auto &l : layers)
		{
			x = l.forward(x);
		}
		return x;
	}

	range_container forward(const range_container &rc)
	{
		range_container result = rc;
		for (auto &l : layers)
		{
			result = l.forward(result);
		}
		return result;
	}

	void add_layer(remap_layer &l)
	{
		layers.push_back(l);
	}

	void index()
	{
		for (auto &l : layers)
		{
			l.index();
		}
	}

	int size()
	{
		return layers.size();
	}
};

u_int64_t take_int(std::string_view &sv)
{
	size_t end = sv.find(' ');
	if (end == std::string_view::npos)
		end = sv.length();
	u_int64_t x;
	std::from_chars(sv.data(), sv.data() + end, x);
	sv.remove_prefix(end);
	return x;
}

std::pair<sequence, std::vector<u_int64_t>> parse(std::istream &input)
{
	sequence s;
	std::vector<u_int64_t> xs;
	std::string line;
	// get inputs
	{
		std::getline(input, line);
		std::string_view sv(line);
		size_t init_len = sv.find(' ');
		sv.remove_prefix(init_len + 1);
		while (sv.length() > 0)
		{
			xs.push_back(take_int(sv));
			if (sv.length() > 0)
				sv.remove_prefix(1);
		}
	}

	std::getline(input, line);
	while (std::getline(input, line))
	{
		remap_layer curr_layer;
		// loop trough all transforms
		while (std::getline(input, line) && line.length() > 0)
		{
			std::string_view sv(line);
			auto dest = take_int(sv);
			sv.remove_prefix(1);
			auto src = take_int(sv);
			sv.remove_prefix(1);
			auto length = take_int(sv);
			remap r(src, dest, length);
			curr_layer.add_remap(r);
		}
		s.add_layer(curr_layer);
	}

	return {s, xs};
}

range_container parse_ranges(std::vector<u_int64_t> &xs)
{
	range_container rc;
	for (size_t i = 0; i < xs.size(); i += 2)
	{
		rc.ranges.push_back({xs[i], xs[i + 1]});
	}
	return rc;
}

void part1(sequence &seq, std::vector<u_int64_t> &xs)
{
	u_int64_t min = ~0;
	for (auto x : xs)
	{
		auto y = seq.forward(x);
		if (y < min)
			min = y;
	}
	std::cout << "Part 1: " << min << std::endl;
}

void part2(sequence &seq, range_container &rc)
{
	auto result = seq.forward(rc);
	u_int64_t min = ~0;
	std::cout << "there are " << result.ranges.size() << " ranges" << std::endl;
	for (auto r : result.ranges)
	{
		if (r.lowest() < min)
			min = r.lowest();
	}
	std::cout << "Part 2: " << min << std::endl;
}

int main()
{
	auto [seq, xs] = parse(std::cin);
	part1(seq, xs);
	range_container rc = parse_ranges(xs);
	seq.index();
	part2(seq, rc);
}