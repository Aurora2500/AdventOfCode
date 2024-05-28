package main

import (
	"io"
	"os"
	"strconv"
	"strings"
)

func execute(program []int, verb, noun int, out chan int) {
	c := make([]int, len(program))
	copy(c, program)
	c[1] = noun
	c[2] = verb
	i := 0
	for {
		if i >= len(c) {
			break
		}
		switch c[i] {
		case 1:
			c[c[i+3]] = c[c[i+1]] + c[c[i+2]]
		case 2:
			c[c[i+3]] = c[c[i+1]] * c[c[i+2]]
		case 99:
			break
		}
		i += 4
	}

	out <- c[0]
}

func part1(program []int, out chan int) {
	execute(program, 2, 12, out)
}

func part2(program []int, out chan int) {
	for verb := 0; verb < 100; verb++ {
		for noun := 0; noun < 100; noun++ {
			ch := make(chan int)
			go execute(program, verb, noun, ch)
			if <-ch == 19690720 {
				out <- verb + 100*noun
				return
			}
		}
	}
}

func main() {
	bytes, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	codes := strings.Split(string(bytes), ",")
	program := make([]int, len(codes))
	for i, x := range codes {
		program[i], err = strconv.Atoi(x)
	}

	ch1 := make(chan int)
	ch2 := make(chan int)
	go part1(program, ch1)
	go part2(program, ch2)
	println("Part 1:", <-ch1)
	println("Part 2:", <-ch2)
}
