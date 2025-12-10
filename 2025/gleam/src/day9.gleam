import functional.{expect, to_pair}
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

type Point {
  Point(x: Int, y: Int)
}

pub fn main(input: String) -> Nil {
  let points = parse(input)
  part1(points)
}

fn parse(input: String) -> List(Point) {
  use line <- list.map(
    input
    |> string.trim()
    |> string.split("\n"),
  )
  let assert [x, y] =
    line |> string.split(",") |> list.map(expect(int.parse, "should be int"))

  Point(x:, y:)
}

fn part1(data: List(Point)) -> Nil {
  data
  |> list.combination_pairs()
  |> list.map(to_pair(area))
  |> list.max(int.compare)
  |> result.map(int.to_string)
  |> result.unwrap("Not found")
  |> string.append("Part 1: ", _)
  |> io.println()
}

fn part2(data: List(Point)) -> Nil {
  let pairs =
    data
    |> list.combination_pairs()

  let ordered_sol = {
    todo
  }

  todo
}

fn area(l: Point, r: Point) -> Int {
  let x = int.max(l.x, r.x) - int.min(l.x, r.x) + 1
  let y = int.max(l.y, r.y) - int.min(l.y, r.y) + 1

  x * y
}

type Axis {
  Horizontal
  Vertical
}

type Line {
  Line(ax: Axis, pos: Int, low: Int, high: Int)
}

fn intersects(a: Line, b: Line) -> Bool {
  a.ax != b.ax
  && { b.low < a.pos && a.pos <= b.high }
  && { a.low < b.pos && b.pos <= a.high }
}
