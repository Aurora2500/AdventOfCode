import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string
import utils.{Break, Continue}

type Vec {
  Vec(x: Int, y: Int)
}

type Map =
  Set(Vec)

pub fn main(input: String) -> Nil {
  let map =
    input
    |> parse
  io.println("--- Day 4 ---")
  part1(map)
  part2(map)
}

type ParseCtx {
  ParseCtx(map: Map, row: Int, col: Int)
}

fn parse(input: String) -> Map {
  let chars = string.to_graphemes(input)

  let ParseCtx(map:, ..) = {
    use ctx, c <- list.fold(chars, ParseCtx(set.new(), 0, 0))
    let ParseCtx(map, row, col) = ctx
    case c {
      "." -> ParseCtx(map:, row:, col: col + 1)
      "@" -> ParseCtx(map: map |> set.insert(Vec(col, row)), row:, col: col + 1)
      "\n" -> ParseCtx(map:, row: ctx.row + 1, col: 0)
      _ -> panic as "unexpected character in input"
    }
  }

  map
}

fn part1(map: Map) -> Nil {
  let sum = {
    use sum, v <- set.fold(map, 0)
    let n = neigh(v) |> list.count(set.contains(map, _))

    case n < 4 {
      True -> sum + 1
      False -> sum
    }
  }

  io.print("Part 1: ")
  io.println(int.to_string(sum))
}

fn part2(map: Map) -> Nil {
  let #(_, sum) = {
    use #(m, removed) <- utils.while(#(map, 0))
    let reach = reachable(m)
    let rl = list.length(reach)
    use <- bool.guard(rl == 0, Break(#(m, removed)))
    let new_m = list.fold(reach, m, set.delete)
    Continue(#(new_m, removed + rl))
  }

  io.print("Part 2: ")
  io.println(int.to_string(sum))
}

fn neigh(v: Vec) -> List(Vec) {
  let Vec(x, y) = v

  [
    Vec(x + 1, y + 1),
    Vec(x + 1, y + 0),
    Vec(x + 1, y - 1),
    Vec(x + 0, y + 1),
    Vec(x + 0, y - 1),
    Vec(x - 1, y + 1),
    Vec(x - 1, y + 0),
    Vec(x - 1, y - 1),
  ]
}

fn reachable(map: Map) -> List(Vec) {
  use l, v <- set.fold(map, [])
  let n = neigh(v) |> list.count(set.contains(map, _))
  case n < 4 {
    True -> [v, ..l]
    False -> l
  }
}
