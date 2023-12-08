#pragma once
#include <string_view>
#include <charconv>
#include <cctype>

int take_int(std::string_view &src, size_t len)
{
	int x;
	std::from_chars(src.data(), src.data() + len, x);
	src.remove_prefix(len);
	return x;
}

int take_int(std::string_view &src)
{
	size_t len = 0;
	while (len < src.length() && std::isdigit(src[len]))
		len++;
	return take_int(src, len);
}
