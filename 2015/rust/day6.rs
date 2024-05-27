
type Grid = [[bool; 1000]; 1000];

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
struct Pos{
    x: i16,
    y: i16
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Action{
    On,
    Off,
    Toggle
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
struct Command{
    from: Pos,
    to: Pos,
    action: Action
}

fn process_action(action: Action, grid: &mut Grid) {

}

fn part1() {
    let grid: Grid = [[false; 1000]; 1000];
}

fn main() {
    part1();
}
