package main

func digitize(n int) []int {
	num_digits := 0
	d := n
	for d > 0 {
		d /= 10
		num_digits++
	}
	digits := make([]int, num_digits)
	for n > 0 {
		num_digits--
		digits[num_digits] = n % 10
		n /= 10
	}
	return digits
}

func part1(low, high int, out chan int) {
	count := 0
digit:
	for i := low; i <= high; i++ {
		digits := digitize(i)
		adjacent := false
		for j := 0; j < len(digits)-1; j++ {
			if digits[j] > digits[j+1] {
				continue digit
			}
			if digits[j] == digits[j+1] {
				adjacent = true
			}
		}
		if adjacent {
			count++
		}
	}
	out <- count
}

func part2(low, high int, out chan int) {
	count := 0
	counter := make([]int, 10)
digit:
	for i := low; i <= high; i++ {
		digits := digitize(i)
		for n := 0; n < 10; n++ {
			counter[n] = 0
		}
		for j := 0; j < len(digits)-1; j++ {
			if digits[j] > digits[j+1] {
				continue digit
			}
			counter[digits[j]]++
		}
		counter[digits[len(digits)-1]]++
		for _, v := range counter {
			if v == 2 {
				count++
				break
			}
		}
	}
	out <- count
}

func main() {
	low := 172851
	high := 675869

	ch1 := make(chan int)
	ch2 := make(chan int)
	go part1(low, high, ch1)
	println("Part 1:", <-ch1)
	go part2(low, high, ch2)
	println("Part 2:", <-ch2)
}
