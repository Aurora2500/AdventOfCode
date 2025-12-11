import gleam/list

pub opaque type Queue(a) {
  Queue(head: List(a), tail: List(a))
}

pub fn new() -> Queue(a) {
  Queue([], [])
}

pub fn push(q: Queue(a), x: a) -> Queue(a) {
  Queue([x, ..q.head], q.tail)
}

pub fn pop(q: Queue(a)) -> Result(#(a, Queue(a)), Nil) {
  case q.tail {
    [x, ..tail] -> Ok(#(x, Queue(q.head, tail)))
    [] ->
      case list.reverse(q.head) {
        [x, ..tail] -> Ok(#(x, Queue([], tail)))
        [] -> Error(Nil)
      }
  }
}
