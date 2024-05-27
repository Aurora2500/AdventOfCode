use std::thread;
use std::sync::mpsc::{channel, TryRecvError};

use md5::{Md5, Digest};

static INPUT: &'static str = "yzbqklnj";

fn has_prefix(inp: &[u8], pc: usize) -> bool {
    if pc > inp.len() {
        return false;
    }
    let h = pc / 2;
    let i = pc % 2 == 0;

    inp.iter().take(h).all(|x| *x==0u8) && (i || inp.iter().skip(h).take(1).all(|x| *x < 16u8))
}

fn find_hash_threaded(pre: &'static str, pc: usize, n: usize) -> (usize, usize) {
    let (main_tx, main_rx) = channel();
    let mut chan_vec = Vec::with_capacity(n);
    for k in 0..n {
        let tx = main_tx.clone();
        let (ttx, trx) = channel();
        thread::spawn(move || {
            let mut hasher = Md5::new();
            let mut i = 0;
            loop {
                
                let curr = i*n+k;
                hasher.update(format!("{}{}", pre, curr));
                let out = hasher.finalize_reset();
                if has_prefix(&out, pc) {
                    tx.send(curr).unwrap();
                }
                i += 1;
                match trx.try_recv() {
                    Ok(_) | Err(TryRecvError::Disconnected) => {
                        tx.send(i).unwrap();
                        break;
                    }
                    Err(TryRecvError::Empty) => {}
                }
            }
        });
        chan_vec.push(ttx);

    };

    let res = main_rx.recv().unwrap();
    chan_vec.iter().for_each(move |tx| tx.send(()).unwrap() );
    let total: usize = (0..n).map(|_| main_rx.recv().unwrap()).sum();
    
    (res, total)
}

fn main() {
    let (p1, t1) = find_hash_threaded(INPUT, 5, 50);
    let (p2, t2) = find_hash_threaded(INPUT, 6, 100);
    println!("Part 1: {}\tN: {}", p1, t1);
    println!("Part 2: {}\tN: {}", p2, t2);
}
