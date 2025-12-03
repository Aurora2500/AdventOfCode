import functional.{on}
import gleam/int
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

type Bank =
  List(Int)

pub fn main(input: String) -> Nil {
  io.println("--- Day 3 ---")
  let banks = parse(input)
  part_n(banks, "1", 2)
  part_n(banks, "2", 12)
}

fn parse(input: String) -> List(Bank) {
  let lines =
    input
    |> string.split("\n")

  use line <- list.map(lines)

  let assert Ok(bank) =
    line
    |> string.to_graphemes
    |> list.map(int.parse)
    |> result.all

  bank
}

fn part_n(banks: List(Bank), day: String, param: Int) -> Nil {
  let sum = {
    use sum, bank <- list.fold(banks, 0)
    let joltage = highest_joltage(bank, param, 0)
    sum + joltage
  }

  io.print("Part " <> day <> ": ")
  io.println(int.to_string(sum))
}

fn highest_joltage(bank: Bank, length: Int, acc: Int) {
  case length {
    n if n > 1 -> {
      let next_n = n - 1

      let len = list.length(bank)
      let indexed_bank =
        bank
        |> list.take(len - next_n)
        |> list.index_map(pair.new)
      let assert Ok(#(left, left_idx)) =
        list.max(indexed_bank, on(int.compare, pair.first))
      highest_joltage(bank |> list.drop(left_idx + 1), next_n, left + 10 * acc)
    }
    1 -> {
      let assert Ok(max) = list.max(bank, int.compare)
      max + 10 * acc
    }
    _ -> panic as "unexpected input to highest joltage"
  }
}
