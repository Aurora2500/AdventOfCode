import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

type Sequence =
  List(Int)

pub fn main(input: String) -> Nil {
  let seq = parse_sequence(input)
  io.println("--- Day 1 ---")
  part1(seq)
  part2(seq)
}

fn parse_sequence(input: String) -> Sequence {
  let lines =
    input
    |> string.split("\n")

  use line <- list.map(lines)
  case line {
    "L" <> num -> -{ int.parse(num) |> result.unwrap(0) }
    "R" <> num -> int.parse(num) |> result.unwrap(0)
    _ -> panic as "unexpected rotation!"
  }
}

fn part1(seq: Sequence) {
  let #(pass, _) = {
    use state, inst <- list.fold(seq, #(0, 50))
    let #(count, rot) = state
    let new_rot = { rot + inst } % 100
    let new_count = case new_rot {
      0 -> count + 1
      _ -> count
    }
    #(new_count, new_rot)
  }
  io.print("Part 1: ")
  io.println(int.to_string(pass))
}

fn part2(seq: Sequence) {
  io.print("Part 2: ")

  let #(pass, _) = {
    use state, inst <- list.fold(seq, #(0, 50))
    let #(count, rot) = state
    let r = rot + inst
    let new_count = case count {
      c if 1 <= r && r <= 99 -> c
      c if r >= 100 -> c + r / 100
      c if r == 0 -> c + 1
      c if r < 0 && rot == 0 -> c - r / 100
      c if r < 0 -> c - r / 100 + 1
      _ -> panic
    }
    #(new_count, mod(rot + inst, 100))
  }

  io.println(int.to_string(pass))
}

fn mod(x: Int, r: Int) -> Int {
  int.modulo(x, r) |> result.unwrap(0)
}
