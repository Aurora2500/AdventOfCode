#include <iostream>
#include <cstring>

#define N 14

bool has_duplicates(char w[], size_t size)
{
	for (int i = 0; i < size; i++)
		for (int j = i + 1; j < size; j++)
			if (w[i] == w[j])
				return true;
	return false;
}

int main()
{
	std::istream &input = std::cin;
	char window[N];
	input.read(window, N);
	int i = N;
	while (has_duplicates(window, N))
	{
		std::memmove(window, window + 1, N - 1);
		input.get(window[N - 1]);
		i++;
	}
	std::cout << "Part 1: " << i << " (";
	std::cout.write(window, N);
	std::cout << ")" << std::endl;
}