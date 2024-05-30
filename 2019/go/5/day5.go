package main

import (
	"io"
	"os"
	"strconv"
	"strings"
)

func getMode(arg, mode int64, mem []int64) int64 {
	if mode == 0 {
		return mem[arg]
	}
	return arg

}

func execute(c, input []int64) []int64 {
	ip := int64(0)
	in := int64(0)
	output := make([]int64, 0, 4)

prog:
	for {
		opcode := c[ip] % 100
		modes := c[ip] / 100
		switch opcode {
		case 1:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			arg3 := c[ip+3]
			c[arg3] = arg1 + arg2
			ip += 4
		case 2:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			arg3 := c[ip+3]
			c[arg3] = arg1 * arg2
			ip += 4
		case 3:
			c[c[ip+1]] = input[in]
			in++
			ip += 2
		case 4:
			output = append(output, getMode(c[ip+1], modes%10, c))
			ip += 2
		case 5:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			if arg1 != 0 {
				ip = arg2
			} else {
				ip += 3
			}
		case 6:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			if arg1 == 0 {
				ip = arg2
			} else {
				ip += 3
			}
		case 7:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			arg3 := c[ip+3]
			if arg1 < arg2 {
				c[arg3] = 1
			} else {
				c[arg3] = 0
			}
			ip += 4
		case 8:
			arg1 := getMode(c[ip+1], modes%10, c)
			arg2 := getMode(c[ip+2], (modes/10)%10, c)
			arg3 := c[ip+3]
			if arg1 == arg2 {
				c[arg3] = 1
			} else {
				c[arg3] = 0
			}
			ip += 4
		case 99:
			break prog
		default:
			println("Unknown opcode", opcode)
			break prog
		}
	}
	return output
}

func part1(code []int64, out chan int64) {
	code_copy := make([]int64, len(code))
	copy(code_copy, code)
	in := []int64{1}
	output := execute(code_copy, in)
	out <- output[len(output)-1]
}

func part2(code []int64, out chan int64) {
	code_copy := make([]int64, len(code))
	copy(code_copy, code)
	in := []int64{5}
	output := execute(code_copy, in)
	out <- output[len(output)-1]
}

func main() {
	bytes, _ := io.ReadAll(os.Stdin)
	code_str := strings.Split(string(bytes), ",")
	code := make([]int64, len(code_str))
	for i, x := range code_str {
		inst, _ := strconv.Atoi(x)
		code[i] = int64(inst)

	}

	// ch1 := make(chan int)
	ch2 := make(chan int64)
	// go part1(code, ch1)
	go part2(code, ch2)
	// println("Part 1:", <-ch1)
	println("Part 2:", <-ch2)
}
