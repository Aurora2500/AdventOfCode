import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string

const ascii_s = 83

const ascii_dot = 46

const ascii_chev = 94

type Splitters =
  Set(Int)

pub fn main(input: String) -> Nil {
  let data = parser(input)
  part1(data)
  part2(data)
}

fn parser(input: String) -> #(Int, List(Splitters)) {
  let lines = input |> string.split(on: "\n")
  let assert [first, ..lines] = lines
  let assert Some(start) = {
    let codepoints = string.to_utf_codepoints(first)
    use acc, codepoint, idx <- list.index_fold(codepoints, None)
    let ascii = string.utf_codepoint_to_int(codepoint)

    case ascii == ascii_s {
      False -> acc
      True -> Some(idx)
    }
  }

  let splitters = {
    use line <- list.filter_map(lines)
    let chars =
      string.to_utf_codepoints(line)
      |> list.map(string.utf_codepoint_to_int)

    let empty = list.all(chars, fn(c) { c == ascii_dot })
    use <- bool.guard(empty, Error(Nil))

    {
      use acc, char, idx <- list.index_fold(chars, set.new())

      case char == ascii_chev {
        True -> set.insert(acc, idx)
        False -> acc
      }
    }
    |> Ok
  }

  #(start, splitters)
}

fn part1(input: #(Int, List(Splitters))) -> Nil {
  let #(start, splitters) = input

  let #(result, _) = {
    use #(count, beams), row <- list.fold(splitters, #(
      0,
      set.from_list([start]),
    ))
    let #(splits, beams) = splitter_pass(row, beams)
    #(count + splits, beams)
  }

  result
  |> int.to_string()
  |> string.append("Part 1: ", _)
  |> io.println()
}

fn part2(input: #(Int, List(Splitters))) -> Nil {
  let #(start, splitters) = input

  let result =
    {
      use beams, row <- list.fold(splitters, dict.from_list([#(start, 1)]))
      splitter_quantum_pass(row, beams)
    }
    |> dict.fold(0, fn(acc, _, n) { acc + n })

  result
  |> int.to_string()
  |> string.append("Part 2: ", _)
  |> io.println()
}

fn splitter_pass(s: Splitters, beams: Set(Int)) -> #(Int, Set(Int)) {
  use #(count, next), beam <- set.fold(beams, #(0, set.new()))
  case set.contains(s, beam) {
    True -> #(count + 1, next |> set.insert(beam - 1) |> set.insert(beam + 1))
    False -> #(count, set.insert(next, beam))
  }
}

fn splitter_quantum_pass(s: Splitters, beams: Dict(Int, Int)) -> Dict(Int, Int) {
  use next, beam, beam_count <- dict.fold(beams, dict.new())
  let inc = fn(x) {
    case x {
      Some(x) -> x + beam_count
      None -> beam_count
    }
  }

  case set.contains(s, beam) {
    True -> next |> dict.upsert(beam - 1, inc) |> dict.upsert(beam + 1, inc)
    False -> next |> dict.upsert(beam, inc)
  }
}
