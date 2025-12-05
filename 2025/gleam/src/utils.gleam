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

pub fn while(init: a, cond: fn(a) -> list.ContinueOrStop(a)) -> a {
  case cond(init) {
    list.Continue(n) -> while(n, cond)
    list.Stop(e) -> e
  }
}
