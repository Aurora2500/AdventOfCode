static INPUT: &str = include_str!("input.txt");
static ILLEGAL: [&str; 4] = ["ab", "cd", "pq", "xy"];

fn good_str1(text: &str) -> bool {
    let mut vowel_count = Some(3);
    let mut last_char = '\0';
    let mut double = false;

    if ILLEGAL.iter().map(|ss| text.contains(ss) ).any(|b| b) {
        return false;
    }

    for c in text.chars() {
        if "aeiou".contains(c) {
            vowel_count = match vowel_count {
                Some(1) | None => None,
                Some(n) => Some(n-1),
            }
        }
        double = double || (last_char == c);
        last_char = c;
    }
    vowel_count.is_none() && double
}

fn good_str2(text: &str) -> bool {
    let mut last_char = '\0';
    let mut plast_char = '\0';
    let mut sandwitch = false;

    for (i, c) in text.chars().enumerate() {
        sandwitch = sandwitch || (c == plast_char);
        plast_char = last_char;
        last_char = c;
    }

    sandwitch
}

fn main() {
    let lines : Vec<_> = INPUT.lines().collect();
    let ans1 = lines.iter().map(|s| good_str1(s)).filter(|&b| b).count();
    let ans2 = lines.iter().map(|s| good_str2(s)).filter(|&b| b).count();

    println!("Part 1: {}", ans1);
    
}
