package main

import (
	"io"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (p Point) distance() int {
	return abs(p.x) + abs(p.y)
}

type Wire struct {
	points map[Point]int
}

func build_wire(segment []string) Wire {
	wire := Wire{points: make(map[Point]int)}
	x, y := 0, 0
	steps := 0
	for _, s := range segment {
		direction := s[0]
		distance, _ := strconv.Atoi(s[1:])
		for i := 0; i < distance; i++ {
			steps++
			switch direction {
			case 'U':
				y++
			case 'D':
				y--
			case 'L':
				x--
			case 'R':
				x++
			}
			wire.points[Point{x, y}] = steps
		}
	}
	return wire
}

func part1(wires []Wire, out chan int) {
	min_distance := 1 << 31
	for p := range wires[0].points {
		if _, ok := wires[1].points[p]; ok {
			if p.distance() < min_distance {
				min_distance = p.distance()
			}
		}
	}
	out <- min_distance
}

func part2(wires []Wire, out chan int) {
	min_steps := 1 << 31
	for p, steps1 := range wires[0].points {
		if steps2, ok := wires[1].points[p]; ok {
			total_steps := steps1 + steps2
			if total_steps < min_steps {
				min_steps = total_steps
			}
		}
	}
	out <- min_steps
}

func main() {
	bytes, _ := io.ReadAll(os.Stdin)
	lines := strings.Split(string(bytes), "\n")
	wires := make([]Wire, 0, len(lines))
	for _, line := range lines {
		if len(line) == 0 {
			continue
		}
		wires = append(wires, build_wire(strings.Split(line, ",")))
	}
	ch1 := make(chan int)
	ch2 := make(chan int)
	go part1(wires, ch1)
	go part2(wires, ch2)
	println("Part 1:", <-ch1)
	println("Part 2:", <-ch2)
}
