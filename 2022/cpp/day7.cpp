#include <iostream>
#include <vector>
#include <memory>
#include <unordered_map>
#include <optional>
#include <list>
#include <limits>

#include "util.hpp"

class node
{
public:
	virtual size_t size() = 0;
};

class file : public node
{
	size_t s;

public:
	file(size_t size) : s(size) {}
	size_t size() override { return s; }
};

class directory : public node
{
	std::unordered_map<std::string, std::shared_ptr<node>> children;
	std::weak_ptr<directory> parent;

public:
	directory() {}
	size_t size() override
	{
		size_t size = 0;
		for (auto child : children)
			size += child.second->size();
		return size;
	}

	void add_child(std::string name, std::shared_ptr<node> child)
	{
		children.insert({name, child});
	}

	void set_parent(std::weak_ptr<directory> parent)
	{
		this->parent = parent;
	}

	class iterator
	{
		using valtype = directory;
		using ptrtype = valtype *;
		using reftype = valtype &;

		std::list<directory *> l;

	public:
		iterator(directory *root)
		{
			if (root)
				l.push_back(root);
		}

		iterator &operator++()
		{
			for (auto &kv : l.front()->children)
			{
				directory *child = dynamic_cast<directory *>(kv.second.get());
				if (child)
					l.push_back(child);
			}
			l.pop_front();
			return *this;
		}

		iterator operator++(int)
		{
			iterator tmp = *this;
			++(*this);
			return tmp;
		}

		reftype operator*()
		{
			return *l.front();
		}

		ptrtype operator->()
		{
			return l.front();
		}

		bool operator==(const iterator &other) const
		{
			if (l.empty() && other.l.empty())
				return true;
			if (l.empty() || other.l.empty())
				return false;
			return l.front() == other.l.front();
		}

		bool operator!=(const iterator &other) const
		{
			return !(*this == other);
		}
	};

	iterator begin()
	{
		return iterator(this);
	}

	iterator end()
	{
		return iterator(nullptr);
	}

	friend std::shared_ptr<directory>
	parse(std::istream &input);
};

std::shared_ptr<directory> parse(std::istream &input)
{
	std::shared_ptr<directory> root = std::make_shared<directory>();
	std::shared_ptr<directory> current = root;

	std::string line;
	// ignore first line
	std::getline(input, line);
	while (std::getline(input, line))
	{
		std::string_view sv(line);
		if (sv[0] == '$')
		{
			sv.remove_prefix(2);
			int cmd_len = sv.find(' ');
			std::string cmd(sv.substr(0, cmd_len));
			sv.remove_prefix(cmd_len + 1);
			if (cmd == "cd")
			{
				if (sv == "..")
				{
					current = current->parent.lock();
				}
				else
				{
					current = std::static_pointer_cast<directory>(current->children[std::string(sv)]);
				}
			}
			else
			{
				// lists
				while (!input.eof() && input.peek() != '$')
				{
					std::getline(input, line);
					sv = line;
					int size_len = sv.find(' ');
					std::string name = std::string(sv.substr(size_len + 1));
					std::shared_ptr<node> child;
					if (std::isdigit(sv[0]))
					{
						int size = take_int(sv, size_len);
						child = std::make_shared<file>(size);
					}
					else
					{
						std::shared_ptr<directory> d = std::make_shared<directory>();
						d->set_parent(current);
						child = d;
					}
					current->add_child(name, child);
				}
			}
		}
	}

	return std::move(root);
}

void part1(std::shared_ptr<directory> root)
{
	int max_size = 100000;
	int size_sum = 0;
	for (auto &d : *root)
		if (d.size() <= max_size)
			size_sum += d.size();
	std::cout << "Part 1: " << size_sum << std::endl;
}

void part2(std::shared_ptr<directory> root)
{
	const int total_space = 70000000;
	const int needed_space = 30000000;
	int used_space = root->size();
	int free_space = total_space - used_space;
	int remaining_needed = needed_space - free_space;

	int lowest_to_delete = std::numeric_limits<int>::max();
	for (auto &d : *root)
	{
		int size = d.size();
		if (size >= remaining_needed && size < lowest_to_delete)
			lowest_to_delete = size;
	}
	std::cout << "Part 2: " << lowest_to_delete << std::endl;
}

int main()
{
	std::shared_ptr<directory> root = parse(std::cin);
	part1(root);
	part2(root);
}