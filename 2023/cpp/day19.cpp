#include <vector>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <array>
#include <variant>
#include <optional>
#include <iostream>
#include <string_view>

#include "util.hpp"

enum class Category
{
	Extreme,
	Musical,
	Aerodynamic,
	Shiny,
};

Category char_to_cat(char c)
{
	switch (c)
	{
	case 'x':
		return Category::Extreme;
	case 'm':
		return Category::Musical;
	case 'a':
		return Category::Aerodynamic;
	case 's':
		return Category::Shiny;
	}
	throw std::runtime_error("invalid category");
}

char char_from_cat(Category cat)
{
	switch (cat)
	{
	case Category::Extreme:
		return 'x';
	case Category::Musical:
		return 'm';
	case Category::Aerodynamic:
		return 'a';
	case Category::Shiny:
		return 's';
	}
}

enum class End
{
	Accepted,
	Rejected,
};

enum class Relation
{
	Less,
	Greater,
};

using Outcome = std::variant<End, std::string>;

struct Part
{
	std::array<int, 4> ratings;

	int operator[](Category c) const
	{
		return ratings[static_cast<int>(c)];
	}

	int sum() const
	{
		int sum = 0;
		for (int r : ratings)
		{
			sum += r;
		}
		return sum;
	}
};

struct Condition
{
	Category cat;
	Relation rel;
	int treshold;
};

struct Rule
{
	std::optional<Condition> cond;
	std::variant<End, std::string> next;
};

struct Workflow
{
	std::vector<Rule> rules;

	Outcome forward(const Part &part) const
	{
		for (const auto &rule : rules)
		{
			if (rule.cond)
			{
				const auto &cond = *rule.cond;
				int rating = part[cond.cat];
				switch (cond.rel)
				{
				case Relation::Less:
					if (rating < cond.treshold)
						return rule.next;
					break;
				case Relation::Greater:
					if (rating > cond.treshold)
						return rule.next;
					break;
				}
			}
			else
			{
				return rule.next;
			}
		}
		throw std::runtime_error("no matching rule");
	}

	friend std::ostream &operator<<(std::ostream &os, const Workflow &workflow)
	{
		os << '{';
		bool first_rule = true;
		for (const auto &rule : workflow.rules)
		{
			if (!first_rule)
			{
				os << ',';
			}
			if (rule.cond)
			{
				const auto &cond = *rule.cond;
				os << char_from_cat(cond.cat) << (cond.rel == Relation::Less ? '<' : '>') << cond.treshold << ':';
			}
			if (std::holds_alternative<End>(rule.next))
			{
				os << (std::get<End>(rule.next) == End::Accepted ? "A" : "R");
			}
			else
			{
				os << std::get<std::string>(rule.next);
			}
			first_rule = false;
		}
		os << '}';
		return os;
	}
};

struct edge
{
	std::string from, to;

	edge() = delete;
	edge(const std::string &from, const std::string &to) : from(from), to(to) {}
};

template <>
struct std::hash<edge>
{
	size_t operator()(const edge &e) const
	{
		return std::hash<std::string>()(e.from) ^ std::hash<std::string>()(e.to);
	}
};

using WorkflowMap = std::unordered_map<std::string, Workflow>;

std::vector<Workflow> topological_sort(const WorkflowMap &map, std::string &start)
{
	std::vector<Workflow> result;
	std::queue<std::string> free;
	free.push(start);
	std::unordered_set<edge> edges;
	for (const auto &[name, workflow] : map)
	{
		for (const auto &rule : workflow.rules)
		{
			if (std::holds_alternative<std::string>(rule.next))
			{
				edges.emplace(name, std::get<std::string>(rule.next));
			}
		}
	}
	while (!free.empty())
	{
		auto &n = free.front();
		free.pop();
		auto &w = map.at(n);
	}
	if (!edges.empty())
		throw std::runtime_error("graph is not a DAG");
	return result;
}

std::pair<WorkflowMap, std::vector<Part>> parse(std::istream &is)
{
	std::pair<WorkflowMap, std::vector<Part>> result;
	auto &map = result.first;
	auto &parts = result.second;

	bool in_workflow = true;

	std::string line;
	while (std::getline(is, line))
	{
		std::string_view sv(line);
		if (sv.length() == 0)
		{
			in_workflow = false;
			continue;
		}
		if (in_workflow)
		{
			int name_len = sv.find('{');
			std::string name(sv.substr(0, name_len));
			sv.remove_prefix(name_len + 1);
			Workflow workflow;
			while (sv.length() > 0)
			{
				Rule rule;
				if (sv[1] == '<' || sv[1] == '>')
				{
					Category cat = char_to_cat(sv[0]);
					Relation rel = sv[1] == '<' ? Relation::Less : Relation::Greater;
					sv.remove_prefix(2);
					int treshold = take_int(sv);
					sv.remove_prefix(1);
					rule.cond = Condition{cat, rel, treshold};
				}
				if (sv[0] == 'A' || sv[0] == 'R')
				{
					rule.next = sv[0] == 'A' ? End::Accepted : End::Rejected;
					sv.remove_prefix(2);
				}
				else
				{
					int name_len = sv.find_first_of(",}");
					rule.next = std::string(sv.substr(0, name_len));
					sv.remove_prefix(name_len + 1);
				}
				workflow.rules.push_back(rule);
			}
			map.emplace(name, workflow);
		}
		else
		{
			Part part;
			for (int i = 0; i < 4; i++)
			{
				sv.remove_prefix(3);
				part.ratings[i] = take_int(sv);
			}
			parts.push_back(part);
		}
	}
	return result;
}

void part1(const WorkflowMap &map, const std::vector<Part> &parts)
{
	int sum = 0;
	std::string start = "in";
	for (const auto &part : parts)
	{
		std::cout << "{x=" << part[Category::Extreme] << ",m=" << part[Category::Musical]
							<< ",a=" << part[Category::Aerodynamic] << ",s=" << part[Category::Shiny] << "}: ";
		std::string current = start;
		while (true)
		{
			std::cout << current << " -> ";
			auto &workflow = map.at(current);
			auto outcome = workflow.forward(part);
			if (std::holds_alternative<End>(outcome))
			{
				if (std::get<End>(outcome) == End::Accepted)
				{
					sum += part.sum();
					std::cout << "A " << part.sum() << std::endl;
				}
				else
				{
					std::cout << "R" << std::endl;
				}
				break;
			}
			else
			{
				current = std::get<std::string>(outcome);
			}
		}
	}
	std::cout << "Part 1: " << sum << std::endl;
}

int main()
{
	auto [map, parts] = parse(std::cin);
	part1(map, parts);
}