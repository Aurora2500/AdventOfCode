package main

import (
	"bufio"
	"os"
)

type point struct {
	x, y int
}

func (p point) add(o point) point {
	return point{x: p.x + o.x, y: p.y + o.y}
}

func (p point) neg() point {
	return point{x: -p.x, y: -p.y}
}

var dirs []point = []point{
	{x: +1, y: +0},
	{x: +1, y: +1},
	{x: -0, y: +1},
	{x: -1, y: +1},
	{x: -1, y: -0},
	{x: -1, y: -1},
	{x: +0, y: -1},
	{x: +1, y: -1},
}

type input [][]rune

func (in input) contains(p point) bool {
	if p.x < 0 || p.y < 0 {
		return false
	}
	if p.y >= len(in) || p.x >= len(in[p.y]) {
		return false
	}
	return true
}

func (in input) at(p point) rune {
	return in[p.y][p.x]
}

func parse() input {
	var in input
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		in = append(in, []rune(scanner.Text()))
	}
	return in
}

const xmas string = "XMAS"

func part1(in input) {
	var count int

	for i := range in {
		for j := range in[i] {
		dirloop:
			for _, dir := range dirs {
				p := point{x: j, y: i}
				for _, r := range xmas {
					if !in.contains(p) || in.at(p) != r {
						continue dirloop
					}
					p = p.add(dir)
				}
				count++
			}
		}
	}

	println("Part 1:", count)
}

var msinv map[rune]rune = map[rune]rune{
	'M': 'S',
	'S': 'M',
}

var top_diags = []point{
	{x: +1, y: +1},
	{x: -1, y: +1},
}

func part2(in input) {
	var count int

	for i := range in {
	candidate:
		for j := range in[i] {
			p := point{x: j, y: i}
			if 'A' != in.at(p) {
				continue candidate
			}
			for _, dir := range top_diags {
				if !in.contains(p.add(dir)) || !in.contains(p.add(dir.neg())) {
					continue candidate
				}
				r1 := in.at(p.add(dir))
				r2, ok := msinv[r1]
				if !ok || in.at(p.add(dir.neg())) != r2 {
					continue candidate
				}
			}
			count++
		}
	}

	println("Part 2:", count)
}

func main() {
	data := parse()
	part1(data)
	part2(data)
}
