package main

import (
	"io"
	"os"
	"strconv"
	"strings"
)

func part1(program []int, out chan int) {
	c := make([]int, len(program))
	copy(c, program)
	c[1] = 12
	c[2] = 2
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

func part2(program []int, out chan int) {

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
	go part1(program, ch1)
	println("Part 1:", <-ch1)
}
