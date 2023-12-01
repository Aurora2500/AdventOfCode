#include <iostream>
#include <cctype>
#include <string>
#include <sstream>
#include <vector>

std::vector<std::string> digits {
	"zero",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
};

bool is_digit(std::string& line, int offset, int& result) {
	char c = line.at(offset);
	if (std::isdigit(c)) {
		result = c - '0';
		return true;
	}
	int rest_length = line.length() - offset;
	if (rest_length < 3) return false;
	for (int i = 0; i <= 9; i++) {
		std::string& name = digits[i];
		int length = name.length();
		if (rest_length < length) continue;
		std::string word = line.substr(offset, length);
		if (word == name) {
			result = i;
			return true;
		}
	}
	return false;
}

void part_two() {

	std::string line;
	int acc = 0;

	while (std::getline(std::cin, line)) {
		int first_digit, last_digit;
		bool read_first = false;
		for (int i = 0; i < line.length(); i++) {
			if (is_digit(line, i, last_digit)) {
				if (!read_first) {
					first_digit = last_digit;
					read_first = true;
				}
			}
		}
		int num = 10*first_digit + last_digit;
		// std::cout << "digit: " << num << std::endl;
		acc += num;
	}

	std::cout << "solution: " << acc << std::endl;
}

void part_one() {
	std::string line;
	int acc = 0;

	while (std::getline(std::cin, line)) {
		int first_digit, last_digit;
		bool read_first = false;
		for (char c : line) {
			if (std::isdigit(c)) {
				if (!read_first) {
					first_digit = c - '0';
					read_first = true;
				}
				last_digit = c - '0';
			}
		}
		int num = 10*first_digit + last_digit;
		std::cout << "digit: " << num << std::endl;
		acc += num;
	}

	std::cout << "solution: " << acc << std::endl;
}

int main() {
	part_two();
}