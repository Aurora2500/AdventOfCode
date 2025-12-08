import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/order
import gleam/string

type Point =
  #(Int, Int, Int)

pub fn main(input: String) {
  let points = parse(input)
  part1(points)
  part2(points)
}

fn parse(input: String) -> List(Point) {
  use line <- list.map(string.split(input, "\n"))
  let assert [x, y, z] = string.split(line, ",")
  let assert Ok(x) = int.parse(x)
  let assert Ok(y) = int.parse(y)
  let assert Ok(z) = int.parse(z)
  #(x, y, z)
}

fn part1(data: List(Point)) -> Nil {
  let sets = dict.new()
  let points = list.index_map(data, fn(p, i) { #(i, p) })

  let dists =
    {
      use #(#(left_id, left_point), #(right_id, right_point)) <- list.map(
        list.combination_pairs(points),
      )

      PairDist(dist(left_point, right_point), left_id, right_id)
    }
    |> list.sort(pair_ord)

  let final_sets = {
    use acc, d <- list.fold(dists |> list.take(1000), sets)
    dsu_join(acc, d.a, d.b)
  }

  {
    use acc, k, _ <- dict.fold(final_sets, dict.new())
    let r = dsu_repr(final_sets, k)
    acc |> dict.upsert(r, fn(n) { option.unwrap(n, 0) + 1 })
  }
  |> dict.values()
  |> list.sort(order.reverse(int.compare))
  |> list.take(3)
  |> int.product()
  |> int.to_string()
  |> string.append("Part 1: ", _)
  |> io.println()
}

fn part2(data: List(Point)) -> Nil {
  let num_points = list.length(data)
  let sets: Dict(Int, Int) = dict.new()
  let points = {
    use pd, p, i <- list.index_fold(data, dict.new())
    pd |> dict.insert(i, p)
  }

  let dists =
    {
      use #(#(left_id, left_point), #(right_id, right_point)) <- list.map(
        list.combination_pairs(dict.to_list(points)),
      )

      PairDist(dist(left_point, right_point), left_id, right_id)
    }
    |> list.sort(pair_ord)

  let final = {
    use #(dists, s) <- while(#(dists, sets))
    let assert [next, ..rest] = dists
    let ns = dsu_join(s, next.a, next.b)
    let num_sets = num_points - dict.size(ns)
    case num_sets > 1 {
      True -> Continue(#(rest, ns))
      False -> Break(#(next.a, next.b))
    }
  }
  let assert Ok(#(x1, _, _)) = dict.get(points, final.0)
  let assert Ok(#(x2, _, _)) = dict.get(points, final.1)
  echo #(x1, x2)
  x1 * x2
  |> int.to_string()
  |> string.append("Part 2: ", _)
  |> io.println()
}

fn while(state: a, cond: fn(a) -> Loop(a, b)) -> b {
  case cond(state) {
    Continue(next) -> while(next, cond)
    Break(x) -> x
  }
}

type Loop(a, b) {
  Continue(a)
  Break(b)
}

type PairDist {
  PairDist(dist: Int, a: Int, b: Int)
}

fn pair_ord(x: PairDist, y: PairDist) -> order.Order {
  int.compare(x.dist, y.dist)
}

fn dist(lhs: Point, rhs: Point) -> Int {
  let #(x1, y1, z1) = lhs
  let #(x2, y2, z2) = rhs
  let x = x1 - x2
  let y = y1 - y2
  let z = z1 - z2

  x * x + y * y + z * z
}

fn dsu_repr(dsu: Dict(Int, Int), x: Int) -> Int {
  let parent = dsu |> dict.get(x)

  case parent {
    Error(_) -> x
    Ok(y) if x == y -> x
    Ok(y) -> dsu_repr(dsu, y)
  }
}

fn dsu_join(dsu: Dict(Int, Int), x: Int, y: Int) -> Dict(Int, Int) {
  let px = dsu_repr(dsu, x)
  let py = dsu_repr(dsu, y)

  case px == py {
    True -> dsu
    False -> dict.insert(dsu, px, py)
  }
}
