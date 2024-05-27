use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
enum Direction {
    North,
    South,
    East,
    West,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct Position(i32, i32);

fn update(p: Position, d: Direction) -> Position {
    match d {
        Direction::North => Position(p.0, p.1 +1),
        Direction::South => Position(p.0, p.1 -1),
        Direction::East  => Position(p.0 +1, p.1),
        Direction::West  => Position(p.0 -1, p.1),
    }
}

fn main() {
    let xs: Vec<Direction> = std::fs::read_to_string("input.txt")
                .unwrap()
                .chars()
                .map(|c| match c {
                    '^' => Direction::North,
                    'v' => Direction::South,
                    '>' => Direction::East,
                    '<' => Direction::West,
                    _ => { panic!("{} is not a direction!", c); }
                })
                .collect();
    let mut pos = Position(0, 0);
    let mut visited = HashSet::new();
    visited.insert(pos);
    for d in xs.iter() {
        pos = update(pos, *d);
        visited.insert(pos);
    }
    println!("Part 1: {}", visited.len());
    let mut pos1 = Position(0, 0);
    let mut pos2 = Position(0, 0);
    let mut b = false;
    let mut visited = HashSet::new();
    visited.insert(Position(0, 0));
    for d in xs {
        if b {
            pos1 = update(pos1, d);
            visited.insert(pos1);
        } else {
            pos2 = update(pos2, d);
            visited.insert(pos2);
        }
        b = !b;
    }
    println!("Part 2: {}", visited.len());
}
