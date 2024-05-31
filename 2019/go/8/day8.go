package main

import (
	"io"
	"os"
	"strings"
)

type Image struct {
	data          []int
	width, height int
}

func (i *Image) LayerCount() int {
	return len(i.data) / (i.width * i.height)
}

func (i *Image) Layer(n int) []int {
	start := n * i.width * i.height
	return i.data[start : start+i.width*i.height]
}

func part1(img *Image) int {
	target := 0
	minZeros := img.width * img.height

	for i := 0; i < img.LayerCount(); i++ {
		layer := img.Layer(i)
		var count [10]int
		for _, pixel := range layer {
			count[pixel]++
		}

		if count[0] < minZeros {
			minZeros = count[0]
			target = count[1] * count[2]
		}
	}
	return target
}

func part2(img *Image) string {
	result := make([]int, img.width*img.height)
	for i := range result {
		result[i] = 2
	}

	for i := 0; i < img.LayerCount(); i++ {
		layer := img.Layer(i)
		for j, pixel := range layer {
			if result[j] == 2 {
				result[j] = pixel
			}
		}
	}

	b := strings.Builder{}
	b.Grow(img.width * img.height)

	for i := 0; i < img.height; i++ {
		for j := 0; j < img.width; j++ {
			if result[i*img.width+j] == 0 {
				b.Write([]byte(" "))
			} else {
				b.Write([]byte("█"))
			}
		}
		b.Write([]byte("\n"))
	}
	return b.String()
}

func main() {
	bytes, _ := io.ReadAll(os.Stdin)

	data := make([]int, len(bytes))
	for i, b := range bytes {
		data[i] = int(b - '0')
	}

	image := Image{data, 25, 6}
	result := part1(&image)
	println("Part 1:", result)
	render := part2(&image)
	println("Part 2:")
	println(render)
}
