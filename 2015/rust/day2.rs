use std::iter::FromIterator;
use std::cmp::min;

#[derive(Clone, Copy, Debug)]
struct Measurements(u32, u32, u32);

impl FromIterator<u32> for Measurements {
    fn from_iter<T>(iter: T) -> Self 
        where
            T: IntoIterator<Item = u32>
    {
        let mut i = iter.into_iter();
        Measurements(i.next().unwrap(), i.next().unwrap(), i.next().unwrap())
    }
}

fn paper(m: Measurements) -> u32 {
    let Measurements(l, w, h) = m;
    let x = l*w;
    let y = l*h;
    let z = w*h;
    let s = min(min(x, y), z);
    x*2 + y*2 + z*2 + s
}

fn ribbon(m: Measurements) -> u32 {
    let Measurements(l, w, h) = m;
    let x = l*2 + w*2;
    let y = l*2 + h*2;
    let z = w*2 + h*2;
    l*w*h + min(min(x, y), z)
}

fn main() {
    let xs: Vec<Measurements> = std::fs::read_to_string("input.txt")
            .expect("file couldn't be read")
            .lines()
            .map(|l| l.split("x")
                      .map(|n| n.parse::<u32>().unwrap())
                      .collect::<Measurements>()
                )
            .collect();
    let (p1, p2): (u32, u32) = xs.iter().map(|m| (paper(*m), ribbon(*m)))
                   .fold((0, 0), |(a, b), (c, d)| (a+c, b+d));
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}
