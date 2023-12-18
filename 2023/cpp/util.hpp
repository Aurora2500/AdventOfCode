#include <string>
#include <string_view>
#include <charconv>

using u64 = u_int64_t;
using i64 = int64_t;

int take_int(std::string_view &sv, size_t len, int base = 10)
{
	int x;
	std::from_chars(sv.data(), sv.data() + len, x, base);
	sv.remove_prefix(len);
	return x;
}

int take_int(std::string_view &sv)
{
	size_t len = 0;
	while (len < sv.length() && (std::isdigit(sv[len]) || sv[len] == '-'))
		len++;
	return take_int(sv, len);
}

template <class T>
struct cycle_iter
{
	std::vector<T> &xs;
	size_t i;

	T &operator*()
	{
		return xs[i];
	}

	T &operator->()
	{
		return xs[i];
	}

	cycle_iter &operator++()
	{
		i = (i + 1) % xs.size();
		return *this;
	}

	cycle_iter operator++(int)
	{
		cycle_iter c = *this;
		i = (i + 1) % xs.size();
		return c;
	}

	bool operator==(const cycle_iter &other)
	{
		return xs == other.xs && i == other.i;
	}

	bool operator!=(const cycle_iter &other)
	{
		return !(*this == other);
	}

	cycle_iter begin()
	{
		return *this;
	}

	cycle_iter end()
	{
		return {xs, xs.size()};
	}
};

template <class T>
cycle_iter<T> cycle(std::vector<T> &xs)
{
	return {xs, 0};
}
