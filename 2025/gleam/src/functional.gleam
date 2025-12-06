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

pub fn compose(lhs: fn(b) -> c, rhs: fn(a) -> b) -> fn(a) -> c {
  fn(x) { lhs(rhs(x)) }
}

pub fn on(op: fn(b, b) -> c, using: fn(a) -> b) -> fn(a, a) -> c {
  fn(x, y) { op(using(x), using(y)) }
}

pub fn not(pred: fn(a) -> Bool) -> fn(a) -> Bool {
  fn(x) { !pred(x) }
}

pub fn mapmap(xss: List(List(a)), f: fn(a) -> b) -> List(List(b)) {
  use xs <- list.map(xss)
  use x <- list.map(xs)
  f(x)
}

pub fn expect(f: fn(a) -> Result(b, c), msg: String) -> fn(a) -> b {
  fn(x) {
    case f(x) {
      Ok(r) -> r
      _ -> panic as msg
    }
  }
}
