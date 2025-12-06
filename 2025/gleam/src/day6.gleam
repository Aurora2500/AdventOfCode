import functional.{eq, expect, mapmap, not}
import gleam/int
import gleam/io
import gleam/list
import gleam/string

type ProblemType {
  Add
  Mul
}

type Problem {
  Problem(ProblemType, List(Int))
}

pub fn main(input: String) -> Nil {
  part1(input)
  part2(input)
}

fn parse1(input: String) -> List(Problem) {
  let lines =
    input
    |> string.split(on: "\n")
    |> list.map(fn(l) {
      l
      |> string.split(on: " ")
      |> list.filter(not(string.is_empty))
    })
    |> list.reverse

  let assert [cmds, ..nums] = lines

  let cmds = {
    use cmd <- list.map(cmds)
    case cmd {
      "+" -> Add
      "*" -> Mul
      _ -> panic as "unrecognized cmd"
    }
  }

  let nums =
    {
      use line <- list.map(nums)
      use num <- list.map(line)
      let assert Ok(x) = int.parse(num)
      x
    }
    |> list.transpose

  list.map2(cmds, nums, Problem)
}

fn part1(input: String) -> Nil {
  input
  |> parse1()
  |> solve_problems()
  |> int.to_string
  |> string.append("Part 1: ", _)
  |> io.println()
}

pub fn part2(input: String) -> Nil {
  let mat =
    input
    |> string.split(on: "\n")
    |> list.map(string.to_graphemes)
  let assert Ok(cmds) = list.last(mat)
  let cmds =
    cmds
    |> list.filter(not(eq(" ", _)))
    |> list.map(fn(c) {
      case c {
        "+" -> Add
        "*" -> Mul
        _ -> panic as "unknown op"
      }
    })
  let mat =
    mat
    |> list.take(list.length(mat) - 1)
    |> list.transpose()
  let nums =
    {
      use l <- list.map(mat)
      l
      |> string.concat()
      |> string.trim()
    }
    |> split_on(not(string.is_empty))
    |> mapmap(expect(int.parse, "Not an int"))

  list.map2(cmds, nums, Problem)
  |> solve_problems
  |> int.to_string()
  |> string.append("Part 2: ", _)
  |> io.println()
}

fn solve_problems(problems: List(Problem)) -> Int {
  use acc, p <- list.fold(problems, 0)
  acc
  + case p {
    Problem(Add, xs) -> int.sum(xs)
    Problem(Mul, xs) -> int.product(xs)
  }
}

fn split_on(l: List(a), pred: fn(a) -> Bool) -> List(List(a)) {
  list.chunk(l, pred) |> list.filter(list.all(_, pred))
}
