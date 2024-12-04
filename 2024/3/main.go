package main

import (
	"os"
	"regexp"
	"strconv"
	"strings"
)

func parse() string {
	b, _ := os.ReadFile("input.txt")
	return string(b)
}

var re = regexp.MustCompile("mul\\((\\d{1,3}),(\\d{1,3})\\)")

func part1(in string) {
	matches := re.FindAllStringSubmatch(in, -1)
	var res int
	for _, match := range matches {
		a, _ := strconv.Atoi(match[1])
		b, _ := strconv.Atoi(match[2])
		res += a * b
	}
	println("Part 1:", res)
}

var re2 = regexp.MustCompile("mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)")

func part2(in string) {
	matches := re2.FindAllStringSubmatch(in, -1)
	enabled := true
	var res int
	for _, match := range matches {
		if strings.HasPrefix(match[0], "mul") && enabled {
			a, _ := strconv.Atoi(match[1])
			b, _ := strconv.Atoi(match[2])
			res += a * b
		} else if strings.HasPrefix(match[0], "don't") {
			enabled = false
		} else if strings.HasPrefix(match[0], "do") {
			enabled = true
		}
	}
	println("Part 2:", res)
}

func main() {
	data := parse()
	part1(data)
	part2(data)
}
