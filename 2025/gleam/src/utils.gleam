import gleam/list
import gleam/string

pub fn split_at(in: String, at: Int) -> #(String, String) {
  #(
    in
      |> string.slice(0, at),
    in
      |> string.slice(at, string.byte_size(in)),
  )
}

pub type Loop(a, b) {
  Continue(a)
  Break(b)
}

pub fn while(init: a, cond: fn(a) -> Loop(a, b)) -> b {
  case cond(init) {
    Continue(n) -> while(n, cond)
    Break(e) -> e
  }
}
