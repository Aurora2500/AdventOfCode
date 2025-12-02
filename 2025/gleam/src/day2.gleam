import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import functional.{all_eq}

type Record {
  Record(Int, Int)
}

type Records =
  List(Record)

pub fn main(input: String) -> Nil {
  let records = parse(input)
  io.println("--- Day 2 ---")
  part1(records)
  part2(records)
}

fn parse(input: String) -> Records {
  use range <- list.map(input |> string.split(","))
  let assert Ok([low, high]) =
    range
    |> string.split("-")
    |> list.map(fn(n) { int.parse(n) })
    |> result.all()

  Record(low, high)
}

fn part1(recs: Records) -> Nil {
  let sum = {
    use sum, Record(r1, r2) <- list.fold(recs, 0)
    use sum, rec <- list.fold(list.range(r1, r2), sum)
    let d = digits(rec)
    let n = list.length(d)
    let s = { n + 1 } / 2
    let c = list.sized_chunk(d, s)
    case all_eq(c) && n > 1 {
      True -> {
        sum + rec
      }
      False -> sum
    }
  }

  io.print("Part 1: ")
  io.println(int.to_string(sum))
}

fn part2(recs: Records) -> Nil {
  let sum = {
    use sum, Record(r1, r2) <- list.fold(recs, 0)
    use sum, rec <- list.fold(list.range(r1, r2), sum)

    let d = digits(rec)
    let n = list.length(d)
    let cond = {
      use <- bool.guard(n < 2, False)
      use cs <- list.any(list.range(1, { n + 1 } / 2))
      let c = list.sized_chunk(d, cs)
      all_eq(c)
    }

    case cond {
      True -> sum + rec
      False -> sum
    }
  }

  io.print("Part 2: ")
  io.println(int.to_string(sum))
}

fn digits(x: Int) -> List(Int) {
  let cur = x % 10
  let next = x / 10

  case next {
    0 -> [cur]
    n -> [cur, ..digits(n)]
  }
}
