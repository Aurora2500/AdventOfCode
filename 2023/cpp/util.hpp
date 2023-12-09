#include <string>
#include <string_view>
#include <charconv>

int take_int(std::string_view &sv, size_t len)
{
	int x;
	std::from_chars(sv.data(), sv.data() + len, x);
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