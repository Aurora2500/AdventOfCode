import gleam/list

pub fn all_eq(xs: List(a)) -> Bool {
  xs
  |> list.window_by_2()
  |> list.all(to_pair(eq))
}

pub fn to_pair(f: fn(a, b) -> c) -> fn(#(a, b)) -> c {
  fn(p) {
    let #(p1, p2) = p
    f(p1, p2)
  }
}

pub fn eq(lhs: a, rhs: a) -> Bool {
  lhs == rhs
}
