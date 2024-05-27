package main

import (
	"io"
	"os"
	"strconv"
	"strings"
)

func part1(input []int, out chan int) {
	sum := 0
	for _, m := range input {
		sum += m/3 - 2
	}
	out <- sum
}

func part2(input []int, out chan int) {
	sum := 0
	for _, m := range input {
		for {
			m = m/3 - 2
			if m <= 0 {
				break
			}
			sum += m
		}
	}
	out <- sum
}

func main() {
	bytes, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	data := string(bytes)
	lines := strings.Split(data, "\n")
	modules := make([]int, len(lines))
	for i, line := range lines {
		if line == "" {
			continue
		}
		m, _ := strconv.Atoi(line)
		modules[i] = m
	}
	sol1 := make(chan int)
	sol2 := make(chan int)
	go part1(modules, sol1)
	go part2(modules, sol2)
	println("Part 1:", <-sol1)
	println("Part 2:", <-sol2)
}
