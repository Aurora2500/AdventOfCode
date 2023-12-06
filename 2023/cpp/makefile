days=day1 day2 day3 day4 day5 day6

CC=clang++
CFLAGS= -std=c++20 -ggdb

all: $(days)

clean:
	rm -f $(days)

$(days): %: %.cpp
	${CC} -o $@ $< ${CFLAGS}