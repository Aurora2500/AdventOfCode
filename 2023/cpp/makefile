max_day = 22
days=$(shell seq ${max_day} | sed 's/^/day/')

CC=clang++
CFLAGS= -std=c++20 -ggdb -O0 -Wall

default: day${max_day}

all: $(days)

clean:
	rm -f $(days)

%: %.cpp
	${CC} -o $@ $< ${CFLAGS}