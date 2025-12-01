import gleam/string

pub fn split_at(in: String, at: Int) -> #(String, String) {
  #(
    in
      |> string.slice(0, at),
    in
      |> string.slice(at, string.byte_size(in)),
  )
}
