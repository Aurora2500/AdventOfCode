package main

import (
	"bufio"
	"os"
	"slices"
	"strconv"
	"strings"
)

type input struct {
	left  []int
	right []int
}

func parse() input {
	var left, right []int
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		numbers := strings.Split(line, "   ")
		ln, _ := strconv.Atoi(numbers[0])
		left = append(left, ln)
		rn, _ := strconv.Atoi(numbers[1])
		right = append(right, rn)
	}
	return input{left: left, right: right}
}

func part1(in input) {
	left := make([]int, len(in.left))
	right := make([]int, len(in.left))
	copy(left, in.left)
	copy(right, in.right)
	slices.Sort(left)
	slices.Sort(right)
	var sum int
	for i := range left {
		leftID := left[i]
		rightID := right[i]
		dist := leftID - rightID
		if dist < 0 {
			dist = -dist
		}
		sum += dist
	}
	println("Part 1:", sum)
}

func part2(in input) {
	left := make(map[int]int)
	right := make(map[int]int)
	for _, x := range in.left {
		left[x] += 1
	}
	for _, x := range in.right {
		right[x] += 1
	}
	var similarity int
	for l, count := range left {
		similarity += l * count * right[l]
	}
	println("Part 2:", similarity)
}

func main() {
	data := parse()
	part1(data)
	part2(data)
}
