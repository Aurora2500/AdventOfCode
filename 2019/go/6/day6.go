package main

import (
	"io"
	"os"
	"strings"
)

func part1(orbits map[string][]string) int {
	indirect := make(map[string]int)
	total := 0

	stack := make([]string, 0, 100)
	stack = append(stack, "COM")

	for len(stack) > 0 {
		parent := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		for _, child := range orbits[parent] {
			indirect[child] = indirect[parent] + 1
			total += indirect[child]
			stack = append(stack, child)
		}
	}
	return total
}

type node struct {
	name string
	path []string
}

func contains(slice []string, value string) bool {
	for _, v := range slice {
		if v == value {
			return true
		}
	}
	return false

}

func part2(orbits map[string][]string) int {
	inverse := make(map[string]string)
	for parent, children := range orbits {
		for _, child := range children {
			inverse[child] = parent
		}
	}
	visited := make(map[string]bool)
	visited["YOU"] = true

	queue := []node{{name: inverse["YOU"], path: []string{}}}

	for len(queue) > 0 {
		cur := queue[0]
		queue = queue[1:]
		visited[cur.name] = true

		if contains(orbits[cur.name], "SAN") {
			return len(cur.path)
		}

		for _, child := range orbits[cur.name] {
			if !visited[child] {
				path_copy := make([]string, len(cur.path), len(cur.path)+1)
				copy(path_copy, cur.path)
				queue = append(queue, node{name: child, path: append(path_copy, cur.name)})
			}
		}
		if parent, ok := inverse[cur.name]; ok && !visited[parent] {
			path_copy := make([]string, len(cur.path), len(cur.path)+1)
			copy(path_copy, cur.path)
			queue = append(queue, node{name: parent, path: append(path_copy, cur.name)})
		}
	}
	return -1
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
	r1 := part1(orbits)
	r2 := part2(orbits)
	println("Part 1:", r1)
	println("Part 1:", r2)
}
