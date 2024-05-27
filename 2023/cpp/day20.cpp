#include <vector>
#include <iostream>
#include <string>
#include <string_view>
#include <unordered_map>

enum class Pulse
{
	Low,
	High,
};

struct Signal
{
	Pulse pulse;
	std::string dest;
};

enum class ModuleType
{
	Flipflop,
	Conjunction,
	Broadcast,
	Void,
};

struct Module
{
	ModuleType type;
	std::vector<std::string> outputs;
};

std::unordered_map<std::string, Module> parse(std::istream &is)
{
	std::unordered_map<std::string, Module> modules;
	std::string line;
	while (std::getline(is, line))
	{
		Module m;
		std::string_view sv(line);
		int len = sv.find(' ');
		std::string_view ty = sv.substr(0, len);
		if (ty[0] == '%')
		{
			m.type = ModuleType::Flipflop;
		}
		else if (ty[0] == '&')
		{
			m.type = ModuleType::Conjunction;
		}
		else
		{
			m.type = ModuleType::Broadcast;
		}
		std::string name = std::string(ty.substr(m.type == ModuleType::Broadcast ? 0 : 1));
		sv.remove_prefix(len + 4);
		while (sv.length())
		{
			len = sv.find(',');
			if (len == std::string_view::npos)
			{
				m.outputs.push_back(std::string(sv));
				break;
			}
			else
			{
				m.outputs.push_back(std::string(sv.substr(0, len)));
				sv.remove_prefix(len + 1);
			}
		}
		modules.insert({name, m});
	}

	return modules;
}

int main()
{
	auto modules = parse(std::cin);
	for (auto &it : modules)
	{
		std::cout << it.first << " -> ";
		for (auto &out : it.second.outputs)
		{
			std::cout << out << ' ';
		}
		std::cout << std::endl;
	}
}