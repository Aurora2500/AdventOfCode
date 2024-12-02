package main

import (
	"bufio"
	"os"
	"strconv"
	"strings"
)

type data = []int
type input = []data

func parse() input {
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)
	var in input
	for scanner.Scan() {
		var data data
		line := scanner.Text()
		nums := strings.Split(line, " ")
		for _, ns := range nums {
			n, _ := strconv.Atoi(ns)
			data = append(data, n)
		}
		in = append(in, data)
	}
	return in
}

func part1(in input) {
	var safe int
lines:
	for _, line := range in {
		accending := line[1] > line[0]
		for i := 0; i < len(line)-1; i++ {
			diff := line[i+1] - line[i]
			if !accending {
				diff = -diff
			}
			if diff < 1 || diff > 3 {
				continue lines
			}
		}
		safe++
	}
	println("Part 1:", safe)
}

func part2(in input) {
	var safe int
line:
	for _, line := range in {
	skip:
		for j := -1; j < len(line); j++ {
			subskip := 1
			if j >= 0 {
				subskip = 2
			}
			var accending int
			for i := 0; i < len(line)-subskip; i++ {
				prev := i
				next := i + 1
				if j >= 0 && prev >= j {
					prev++
				}
				if j >= 0 && next >= j {
					next++
				}
				dx := line[next] - line[prev]
				if dx == 0 {
					continue skip
				}
				if accending == 0 {
					if dx < 0 {
						accending = -1
					} else if dx > 0 {
						accending = 1
					}
				} else if (accending == 1 && dx < 0) || (accending == -1 && dx > 0) {
					continue skip
				}
				if accending == -1 {
					dx = -dx
				}
				if dx > 3 {
					continue skip
				}
			}
			safe++
			continue line
		}
	}
	println("Part 2:", safe)
}

func main() {
	input := parse()
	part1(input)
	part2(input)
}
