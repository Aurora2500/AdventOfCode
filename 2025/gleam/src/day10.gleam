import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import fifo.{type Queue} as queue
import functional.{expect}
import utils.{Break, Continue, while}

type Machine {
  Machine(lights: Int, buttons: List(List(Int)), jolts: List(Int))
}

pub fn main(input: String) -> Nil {
  let machines = parse(input)
  part1(machines)
  part2(machines)
}

fn parse(input: String) -> List(Machine) {
  use line <- list.map(input |> string.trim() |> string.split("\n"))
  let #(_, line) = split(line, "[")
  let #(lights, line) = split(line, "] (")
  let #(buttons, line) = split(line, ") {")
  let #(jolts, _) = split(line, "}")

  let lights = {
    use acc, g, idx <- list.index_fold(lights |> string.to_utf_codepoints(), 0)

    case string.utf_codepoint_to_int(g) == 35 {
      True -> int.bitwise_or(acc, int.bitwise_shift_left(1, idx))
      False -> acc
    }
  }

  let buttons = {
    use button <- list.map(buttons |> string.split(") ("))
    button
    |> string.split(",")
    |> list.map(expect(int.parse, "should be int"))
  }

  let jolts =
    jolts
    |> string.split(",")
    |> list.map(expect(int.parse, "should be int"))

  Machine(lights, buttons, jolts)
}

fn split(in: String, on: String) -> #(String, String) {
  string.split_once(in, on) |> result.unwrap(#(in, ""))
}

fn part1(machines: List(Machine)) -> Nil {
  int.sum({
    use machine <- list.map(machines)
    use len <- while(1)

    let solves = {
      use combo <- list.any(list.combinations(machine.buttons, len))
      machine.lights
      == list.fold(combo, 0, fn(a, n) {
        int.bitwise_exclusive_or(a, xor_fold(n))
      })
    }

    case solves {
      True -> Break(len)
      False -> Continue(len + 1)
    }
  })
  |> int.to_string()
  |> string.append("Part 1: ", _)
  |> io.println()
}

fn xor_fold(xs: List(Int)) -> Int {
  use acc, x <- list.fold(xs, 0)
  int.bitwise_or(acc, int.bitwise_shift_left(1, x))
}

fn part2(machines: List(Machine)) -> Nil {
  int.sum({
    use machine <- list.map(machines)
    use len <- while(1)

    let solves = {
      False
    }

    case solves {
      True -> Break(len)
      False -> Continue(len + 1)
    }
  })
  |> int.to_string()
  |> string.append("Part 2: ", _)
  |> io.println()
}
