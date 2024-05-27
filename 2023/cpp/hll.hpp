#include <array>
#include <cmath>
#include <unordered_set>

template <typename T, size_t bits = 10, typename Hash = std::hash<T>>
class hll
{
	friend int main();
	static constexpr size_t m = static_cast<size_t>(1) << bits;
	static constexpr size_t s = sizeof(size_t) * static_cast<size_t>(8);
	static constexpr size_t value_bits = s - bits;
	static constexpr size_t lower_mask = (static_cast<size_t>(1) << value_bits) - 1;
	static constexpr size_t upper_mask = (~0) ^ lower_mask;

private:
	std::array<unsigned int, m> data;

public:
	hll() : data({0}) {}

	void add(T x)
	{
		size_t hash = Hash()(x);
		std::cout << "Hash  " << hash << std::endl;
		size_t index = (hash & upper_mask) >> value_bits;
		size_t value = hash & lower_mask;
		std::cout << "Value " << value << std::endl;
		size_t new_leading = __builtin_clz(value | 1);
		std::cout << "Index " << index << std::endl;
		std::cout << "New leading " << new_leading << std::endl;
		if (new_leading > data[index])
			data[index] = new_leading;
	}

	size_t size()
	{
		double sum = 0;
		for (auto x : data)
			sum += 1.0 / (1 << x);
		double alpha = 0.7213 / (1 + 1.079 / m);
		double estimate = alpha * m * m / sum;
		if (estimate <= 2.5 * m)
		{
			size_t zeros = 0;
			for (auto x : data)
				if (x == 0)
					zeros++;
			if (zeros != 0)
				estimate = m * std::log(m / zeros);
		}
		else if (estimate > 1.0 / 30.0 * pow(2, 32))
			estimate = -pow(2, 32) * std::log(1 - estimate / pow(2, 32));
		return estimate;
	}

	void merge(hll<T, bits, Hash> other)
	{
		for (size_t i = 0; i < m; i++)
			if (other.data[i] > data[i])
				data[i] = other.data[i];
	}
};