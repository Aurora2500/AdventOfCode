fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    // Part 1
    let mut count = 0;
    for c in input.chars() {
        count += match c {
            '(' => 1,
            ')' => -1,
            _ => {continue;}
        }
    };
    println!("part one: {}", count);

    // Part 2
    let mut count = 0;
    let mut index = 0;
    for (i, c) in input.chars().enumerate() {
        count += match c {
            '(' => 1,
            ')' => -1,
            _ => {continue;}
        };
        if count < 0 {
            index = i + 1;
            break;
        };
    };

    println!("Part two: {}", index);
}
