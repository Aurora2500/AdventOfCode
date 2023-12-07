#include <iostream>
#include <vector>
#include <charconv>
#include <algorithm>

enum class CardRank
{
	Two,
	Three,
	Four,
	Five,
	Six,
	Seven,
	Eight,
	Nine,
	Trickster,
	Joker,
	Queen,
	King,
	Ace,
	NumRanks
};

enum class PlayType
{
	High,
	Pair,
	TwoPair,
	ThreeKind,
	House,
	FourKind,
	FiveKind,
};

class Play
{
	int bid;
	std::vector<CardRank> play;
	PlayType type;

	PlayType compute_type()
	{
		int counts[(int)CardRank::NumRanks] = {0};
		for (int i = 0; i < 5; i++)
			counts[(int)play[i]]++;

		// check for five of a kind
		for (int i = 0; i < (int)CardRank::NumRanks; i++)
		{
			if (counts[i] == 5)
				return PlayType::FiveKind;
			if (counts[i] == 4)
				return PlayType::FourKind;
		}

		// check for house
		bool has_three = false;
		bool has_two = false;
		for (int i = 0; i < (int)CardRank::NumRanks; i++)
		{
			if (counts[i] == 3)
				has_three = true;
			if (counts[i] == 2)
				has_two = true;
		}
		if (has_three && has_two)
			return PlayType::House;
		if (has_three)
			return PlayType::ThreeKind;

		int num_pairs = 0;
		for (int i = 0; i < (int)CardRank::NumRanks; i++)
		{
			if (counts[i] == 2)
				num_pairs++;
		}
		if (num_pairs == 2)
			return PlayType::TwoPair;
		if (num_pairs == 1)
			return PlayType::Pair;
		return PlayType::High;
	}

public:
	Play(int bid, std::vector<CardRank> play) : bid(bid), play(play), type(compute_type()) {}

	bool operator<(const Play &other) const
	{
		if (type != other.type)
			return type < other.type;
		for (int i = 0; i < 5; i++)
		{
			if (play[i] != other.play[i])
				return play[i] < other.play[i];
		}
		return false;
	}

	int get_bid() const
	{
		return bid;
	}

	// to string
	friend std::ostream &operator<<(std::ostream &os, const Play &play)
	{
		os << "Bid: " << play.bid << " Play: ";
		for (int i = 0; i < 5; i++)
		{
			if (play.play[i] == CardRank::Two)
				os << "2";
			else if (play.play[i] == CardRank::Three)
				os << "3";
			else if (play.play[i] == CardRank::Four)
				os << "4";
			else if (play.play[i] == CardRank::Five)
				os << "5";
			else if (play.play[i] == CardRank::Six)
				os << "6";
			else if (play.play[i] == CardRank::Seven)
				os << "7";
			else if (play.play[i] == CardRank::Eight)
				os << "8";
			else if (play.play[i] == CardRank::Nine)
				os << "9";
			else if (play.play[i] == CardRank::Trickster)
				os << "T";
			else if (play.play[i] == CardRank::Joker)
				os << "J";
			else if (play.play[i] == CardRank::Queen)
				os << "Q";
			else if (play.play[i] == CardRank::King)
				os << "K";
			else if (play.play[i] == CardRank::Ace)
				os << "A";
		}

		os << " Type: ";
		if (play.type == PlayType::High)
			os << "High";
		else if (play.type == PlayType::Pair)
			os << "Pair";
		else if (play.type == PlayType::TwoPair)
			os << "TwoPair";
		else if (play.type == PlayType::ThreeKind)
			os << "ThreeKind";
		else if (play.type == PlayType::House)
			os << "House";
		else if (play.type == PlayType::FourKind)
			os << "FourKind";
		else if (play.type == PlayType::FiveKind)
			os << "FiveKind";
		return os;
	}
};

std::vector<Play> parse(std::istream &in)
{
	std::vector<Play> plays;
	std::string line;
	while (std::getline(in, line))
	{
		std::string_view cards(line), bid_sv(line);
		cards = cards.substr(0, 5);
		bid_sv.remove_prefix(6);

		std::vector<CardRank> play;
		while (cards.length() > 0)
		{
			char card = cards[0];
			cards.remove_prefix(1);
			if (card == '2')
				play.push_back(CardRank::Two);
			else if (card == '3')
				play.push_back(CardRank::Three);
			else if (card == '4')
				play.push_back(CardRank::Four);
			else if (card == '5')
				play.push_back(CardRank::Five);
			else if (card == '6')
				play.push_back(CardRank::Six);
			else if (card == '7')
				play.push_back(CardRank::Seven);
			else if (card == '8')
				play.push_back(CardRank::Eight);
			else if (card == '9')
				play.push_back(CardRank::Nine);
			else if (card == 'T')
				play.push_back(CardRank::Trickster);
			else if (card == 'J')
				play.push_back(CardRank::Joker);
			else if (card == 'Q')
				play.push_back(CardRank::Queen);
			else if (card == 'K')
				play.push_back(CardRank::King);
			else if (card == 'A')
				play.push_back(CardRank::Ace);
		}

		int bid;
		std::from_chars(bid_sv.data(), bid_sv.data() + bid_sv.length(), bid);

		plays.push_back(Play(bid, play));
	}
	return plays;
}

void part1(std::vector<Play> &plays)
{
	int sum = 0;
	int rank = 1;
	for (auto &p : plays)
		sum += p.get_bid() * rank++;

	std::cout << "Part 1: " << sum << std::endl;
}

int main()
{
	std::vector<Play> plays = parse(std::cin);
	std::sort(plays.begin(), plays.end(), [](const Play &a, const Play &b)
						{ return a < b; });

	part1(plays);
}