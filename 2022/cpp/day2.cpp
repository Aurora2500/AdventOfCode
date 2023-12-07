#include <iostream>

enum class RPS
{
	Rock = 1,
	Paper = 2,
	Scissors = 3,
};

enum class Outcome
{
	Loss = 0,
	Tie = 3,
	Win = 6,
};
RPS own_play(RPS opponent, Outcome outcome)
{
	switch (outcome)
	{
	case Outcome::Tie:
		return opponent;
	case Outcome::Win:
		return (RPS)(((int)opponent) % 3 + 1);
	case Outcome::Loss:
	{
		int a = (int)opponent;
		a += 1;
		a %= 3;
		a += 1;
		RPS w = (RPS)a;
		return w;
	}
	}
}

void part2()
{
	int points = 0;
	std::string line;
	while (std::getline(std::cin, line))
	{
		RPS opponent, own;
		Outcome outcome;
		switch (line.at(0))
		{
		case 'A':
			opponent = RPS::Rock;
			break;
		case 'B':
			opponent = RPS::Paper;
			break;
		case 'C':
			opponent = RPS::Scissors;
			break;
		}
		switch (line.at(2))
		{
		case 'X':
			outcome = Outcome::Loss;
			break;
		case 'Y':
			outcome = Outcome::Tie;
			break;
		case 'Z':
			outcome = Outcome::Win;
			break;
		}
		own = own_play(opponent, outcome);
		points += ((int)own + (int)outcome);
	}

	std::cout << "Part 2: " << points << std::endl;
}

Outcome play(RPS player, RPS opponent)
{
	if (player == opponent)
		return Outcome::Tie;
	if (player == RPS::Rock && opponent == RPS::Scissors)
		return Outcome::Win;
	if (player == RPS::Paper && opponent == RPS::Rock)
		return Outcome::Win;
	if (player == RPS::Scissors && opponent == RPS::Paper)
		return Outcome::Win;
	return Outcome::Loss;
}

void part1()
{
	int points = 0;

	std::string line;
	while (std::getline(std::cin, line))
	{
		RPS opponent, own;
		switch (line.at(0))
		{
		case 'A':
			opponent = RPS::Rock;
			break;
		case 'B':
			opponent = RPS::Paper;
			break;
		case 'C':
			opponent = RPS::Scissors;
			break;
		}
		switch (line.at(2))
		{
		case 'X':
			own = RPS::Rock;
			break;
		case 'Y':
			own = RPS::Paper;
			break;
		case 'Z':
			own = RPS::Scissors;
			break;
		}
		points += ((int)own + (int)play(own, opponent));
	}

	std::cout << "Part 1: " << points << std::endl;
}

int main()
{
	RPS a = own_play(RPS::Rock, Outcome::Loss);
	part2();
}