package main

import (
	"io"
	"os"
	"strings"
)

func part1(orbits map[string][]string) int {
	indirect := make(map[string]int)
	total := 0
	progress := 0
	progress_total := len(orbits)

	stack := make([]string, 0, 100)
	stack = append(stack, "COM")

	for len(stack) > 0 {
		parent := stack[0]
		stack = stack[:len(stack)-1]
		for _, child := range orbits[parent] {
			indirect[child] = indirect[parent] + 1
			total += indirect[child]
			stack = append(stack, child)
		}
		progress++
		println("Progress:", progress, "/", progress_total)
	}
	return total
}

func main() {
	bytes, _ := io.ReadAll(os.Stdin)
	lines := strings.Split(string(bytes), "\n")

	orbits := make(map[string][]string)
	for _, line := range lines {
		if line == "" {
			continue
		}
		orbit := strings.Split(line, ")")
		orbits[orbit[0]] = append(orbits[orbit[0]], orbit[1])
	}
	result := part1(orbits)
	println("Part 1:", result)
}
