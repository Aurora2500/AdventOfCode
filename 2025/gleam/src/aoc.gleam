import argv
import day1.{main as day01}
import day2.{main as day02}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import simplifile

const all_days = [
  "day1",
]

pub fn main() -> Nil {
  let days = case argv.load().arguments {
    [] -> all_days
    selected -> selected
  }

  days |> list.map(runner)

  Nil
}

fn runner(day: String) -> Nil {
  let res = {
    use input <- option.then(get_input(day))
    use start <- option.map(get_start(day))

    #(input, start)
  }

  case res {
    Some(#(input, start)) -> start(input)
    None -> io.println(day <> " is not a valid target")
  }
}

fn get_input(day: String) -> Option(String) {
  let file = case day {
    "day" <> n -> Some("inputs/" <> n <> ".txt")
    "test" <> n -> Some("inputs/test-" <> n <> ".txt")
    _ -> None
  }

  use path <- option.map(file)

  simplifile.read(path) |> result.unwrap("")
}

fn get_start(day: String) -> Option(fn(String) -> Nil) {
  case day {
    "day1" | "test1" -> Some(day01)
    "day2" | "test2" -> Some(day02)
    _ -> None
  }
}
