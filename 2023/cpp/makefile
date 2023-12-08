max_day = 8
days=$(shell seq ${max_day} | sed 's/^/day/')

CC=clang++
CFLAGS= -std=c++20 -ggdb

default: day${max_day}

all: $(days)

clean:
	rm -f $(days)

$(days): %: %.cpp
	${CC} -o $@ $< ${CFLAGS}