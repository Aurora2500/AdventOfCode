#include <iostream>
#include "hll.hpp"

int main()
{
	hll<std::string, 3> h;

	auto print_h = [&h]()
	{
		std::cout << "Size " << h.size() << std::endl;
		for (auto x : h.data)
			std::cout << x << " ";
		std::cout << std::endl;
	};

	print_h();
	h.add("woah");
	print_h();
	h.add("yesss");
	print_h();
	h.add("awoo");
	print_h();
	h.add("zzzzzzzz");
	print_h();
}