#include <iostream>
#include <vector>
#include <numeric>

#include "util.hpp"

using u64 = u_int64_t;
using i64 = int64_t;

enum class Tile : char
{
	Empty,
	Full,
	Unknown,
};

Tile char_to_tile(char c)
{
	switch (c)
	{
	case '.':
		return Tile::Empty;
	case '#':
		return Tile::Full;
	default:
		return Tile::Unknown;
	}
}

struct row
{
	std::vector<Tile> tiles;
	std::vector<int> hint;

	friend std::ostream &operator<<(std::ostream &os, const row &r)
	{
		for (auto t : r.tiles)
		{
			switch (t)
			{
			case Tile::Empty:
				os << '.';
				break;
			case Tile::Full:
				os << '#';
				break;
			default:
				os << '?';
				break;
			}
		}
		os << " ";
		for (auto h : r.hint)
		{
			os << h << ",";
		}
		return os;
	}
};

struct table
{
	std::vector<i64> memo;
	int n_hints;

	table(int hints, int len) : memo(hints * len, -1), n_hints(hints) {}
	bool contains(int hint, int len) const
	{
		if (hint < 0 || len < 0 || hint >= n_hints || len >= (memo.size() / n_hints))
			return false;
		return memo[hint + len * n_hints] != -1;
	}

	i64 get(int hint, int len)
	{
		return memo[hint + len * n_hints];
	}

	void set(int hint, int len, i64 val)
	{
		if (hint < 0 || len < 0 || hint >= n_hints || len >= (memo.size() / n_hints))
			return;
		memo[hint + len * n_hints] = val;
	}
};

row unfold(const row &r)
{
	row res;
	constexpr int reps = 5;
	for (int i = 0; i < reps; ++i)
		for (auto h : r.hint)
			res.hint.push_back(h);
	bool first = true;
	for (int i = 0; i < reps; ++i)
	{
		if (!first)
		{
			res.tiles.push_back(Tile::Unknown);
		}
		for (auto t : r.tiles)
		{
			res.tiles.push_back(t);
		}
		first = false;
	}
	return res;
}

std::vector<row> parse(std::istream &is)
{
	std::vector<row> rows;
	std::string line;
	while (std::getline(is, line))
	{
		std::string_view sv(line);
		row r;
		while (sv.length() > 0 && sv.front() != ' ')
		{
			r.tiles.push_back(char_to_tile(sv.front()));
			sv.remove_prefix(1);
		}
		sv.remove_prefix(1);
		std::vector<int> hints;
		while (sv.length() > 0)
		{
			int hint = take_int(sv);
			hints.push_back(hint);
			if (sv.length() > 0)
				sv.remove_prefix(1);
		}
		r.hint = hints;
		rows.push_back(r);
	}
	return rows;
}

bool fits(const std::vector<Tile> &tiles, int idx, int len)
{
	if (len == 0)
		return tiles[idx] != Tile::Full;
	if (idx + len > tiles.size())
		return false;
	for (int i = 0; i < len; ++i)
	{
		if (tiles[idx + i] == Tile::Empty)
			return false;
	}
	if (idx + len < tiles.size() && tiles[idx + len] == Tile::Full)
		return false;
	return true;
}

u64 pos2(table &t, const std::vector<int> &hints, const std::vector<Tile> &tiles, int hi, int ti)
{
	if (t.contains(hi, ti))
		return t.get(hi, ti);
	if (hi == hints.size() && (ti == tiles.size() || ti == tiles.size() + 1))
	{
		return 1;
	}
	if (ti > tiles.size() || hi > hints.size())
		return 0;
	i64 n1 = 0, n2 = 0;
	if (fits(tiles, ti, hints[hi]))
		n1 = pos2(t, hints, tiles, hi + 1, ti + hints[hi] + 1);
	if (fits(tiles, ti, 0))
		n2 = pos2(t, hints, tiles, hi, ti + 1);
	t.set(hi, ti, n1 + n2);
	return n1 + n2;
}

void part1(const std::vector<row> &rows)
{
	int sum = 0;
	for (const auto &r : rows)
	{
		table t(r.hint.size(), r.tiles.size());
		int p = pos2(t, r.hint, r.tiles, 0, 0);
		sum += p;
	}
	std::cout << "Part 1: " << sum << std::endl;
}

void part2(const std::vector<row> &rows)
{
	u_int64_t sum = 0;
	for (const auto &r : rows)
	{
		row ur = unfold(r);
		table t(ur.hint.size(), ur.tiles.size());
		u64 p = pos2(t, ur.hint, ur.tiles, 0, 0);
		sum += p;
	}
	std::cout << "Part 2: " << sum << std::endl;
}

int main()
{
	auto rows = parse(std::cin);
	part1(rows);
	part2(rows);
}