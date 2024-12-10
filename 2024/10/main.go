package main

import (
	"bufio"
	"os"
)

type input = chart

func parse() input {
	file, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(file)
	var size pos
	var layout []int
	for scanner.Scan() {
		line := scanner.Text()
		size[0] = len(line)
		size[1]++
		for _, c := range line {
			layout = append(layout, int(c)-48)
		}
	}
	return input{
		layout: layout,
		size:   size,
	}
}

var dirs []pos = []pos{
	{+1, 0},
	{0, +1},
	{-1, 0},
	{0, -1},
}

func part12(c input) {
	var result1 int
	var result2 int
	for i := 0; i < c.size[1]; i++ {
		for j := 0; j < c.size[0]; j++ {
			pinit := pos{j, i}
			if c.get(pinit) != 0 {
				continue
			}
			ends := make(set[pos])
			closed := make(set[pos])
			var open []pos
			open = append(open, pinit)
			for len(open) != 0 {
				p := open[0]
				open = open[1:]
				hcurr := c.get(p)
				if hcurr == 9 {
					result2++
					ends.add(p)
					continue
				}
				closed.add(p)
				for _, d := range dirs {
					pnew := p.add(d)
					if closed.contains(pnew) || !c.size.contains(pnew) {
						continue
					}
					hnew := c.get(pnew)
					if hnew != hcurr+1 {
						continue
					}
					open = append(open, pnew)
				}
			}
			result1 += len(ends)
		}
	}
	println("Part 1:", result1)
	println("Part 2:", result2)
}

func main() {
	data := parse()
	part12(data)
}

type pos [2]int

func (p pos) add(o pos) pos {
	return pos{
		p[0] + o[0],
		p[1] + o[1],
	}
}

func (p pos) contains(o pos) bool {
	return o[0] >= 0 && o[1] >= 0 && o[0] < p[0] && o[1] < p[1]
}

type chart struct {
	layout []int
	size   pos
}

func (c *chart) get(p pos) int {
	return c.layout[p[1]*c.size[0]+p[0]]
}

type set[T comparable] map[T]struct{}

func (s set[T]) add(x T) {
	s[x] = struct{}{}
}

func (s set[T]) contains(x T) bool {
	_, ok := s[x]
	return ok
}
