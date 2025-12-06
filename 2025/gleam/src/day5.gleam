import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/string

type Range {
  Range(low: Int, high: Int)
}

type Database {
  Database(fresh: List(Range), available: List(Int))
}

pub fn main(input: String) -> Nil {
  let db = parse(input)

  io.println("--- Day 5 ---")
  part1(db)
  part2(db)
}

fn parse(input: String) -> Database {
  let #(fresh, available) =
    input
    |> string.split("\n")
    |> list.split_while(fn(x) { x != "" })

  let fresh = {
    use fresh <- list.map(fresh)
    case string.split(fresh, "-") {
      [l, r] -> {
        let assert Ok(lo) = int.parse(l)
        let assert Ok(hi) = int.parse(r)

        Range(lo, hi)
      }
      _ -> panic as "unexpected input"
    }
  }

  let available = {
    use available <- list.filter_map(available)
    case available {
      "" -> Error(Nil)
      x -> int.parse(x)
    }
  }

  Database(fresh:, available:)
}

fn part1(db: Database) -> Nil {
  let Database(fresh:, available:) = db

  let count = {
    use id <- list.count(available)
    list.any(fresh, fn(r) { in_range(id, r) })
  }

  io.print("Part 1: ")
  io.println(int.to_string(count))
}

fn part2(db: Database) -> Nil {
  let Database(fresh:, available: _) = db
  let #(count, _) = {
    let sorted = list.sort(fresh, range_cmp)
    use #(acc, point), range <- list.fold(sorted, #(0, 0))
    case cmp_to_range(point, range) {
      order.Gt -> #(acc, point)
      order.Lt -> #(acc + range_len(range), range.high)
      order.Eq -> #(acc + range.high - point, range.high)
    }
  }
  io.print("Part 2: ")
  io.println(int.to_string(count))
}

fn in_range(x: Int, r: Range) -> Bool {
  let Range(low:, high:) = r

  low <= x && x <= high
}

fn cmp_to_range(x: Int, r: Range) -> order.Order {
  let Range(low:, high:) = r

  case low <= x, x <= high {
    False, True -> order.Lt
    True, True -> order.Eq
    True, False -> order.Gt
    _, _ -> panic as "this shouldn't happen to a range!"
  }
}

fn range_cmp(lhs: Range, rhs: Range) -> order.Order {
  order.break_tie(
    int.compare(lhs.low, rhs.low),
    int.compare(lhs.high, rhs.high),
  )
}

fn range_len(r: Range) -> Int {
  let Range(low:, high:) = r
  high - low + 1
}
