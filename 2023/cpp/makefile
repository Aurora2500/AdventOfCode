max_day = 7
days=$(shell seq ${max_day} | sed 's/^/day/')

CC=clang++
CFLAGS= -std=c++20 -ggdb

all: $(days)

clean:
	rm -f $(days)

$(days): %: %.cpp
	${CC} -o $@ $< ${CFLAGS}