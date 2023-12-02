days=day1 day2

CC=clang++

all: $(days)

clean:
	rm -f $(days)

$(days): %: %.cpp
	${CC} -o $@ $<