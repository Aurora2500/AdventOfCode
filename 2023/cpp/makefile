max_day = 16
days=$(shell seq ${max_day} | sed 's/^/day/')

CC=clang++
CFLAGS= -std=c++20 -ggdb -O0

default: day${max_day}

all: $(days)

clean:
	rm -f $(days)

$(days): %: %.cpp
	${CC} -o $@ $< ${CFLAGS}