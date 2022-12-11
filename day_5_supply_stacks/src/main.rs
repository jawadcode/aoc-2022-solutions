use std::{collections::VecDeque, env, fs};

fn part_one(contents: &str) {
    let (stacks, commands) = contents.split_once("\n\n").unwrap();
    let num_stacks = (stacks.lines().next().unwrap().len() + 1) / 4;
    let mut res_stacks = vec![VecDeque::new(); num_stacks];
    for row in stacks.lines() {
        for (index, krate) in row.chars().skip(1).step_by(4).enumerate() {
            if krate.is_uppercase() {
                res_stacks[index].push_front(krate);
            }
        }
    }

    for command in commands.lines() {
        let locations: Vec<usize> = command
            .split_ascii_whitespace()
            .skip(1)
            .step_by(2)
            .map(|num| num.parse().unwrap())
            .collect();
        let (amount, from, dest) = (locations[0], locations[1] - 1, locations[2] - 1);
        for _ in 0..amount {
            let krate = res_stacks[from].pop_back().unwrap();
            res_stacks[dest].push_back(krate);
        }
    }

    let mut result = String::with_capacity(res_stacks.len());
    for stack in res_stacks.iter() {
        result.push(*stack.back().unwrap());
    }
    println!("Crates: {result}");
}

fn part_two(_contents: &str) {
    todo!()
}

fn main() {
    let mut args = env::args().skip(1);
    let arg = args.next();
    let contents = fs::read_to_string("input.txt").unwrap();
    match arg.as_deref() {
        Some("one" | "1") => part_one(&contents),
        Some("two" | "2") => part_two(&contents),
        _ => println!("Usage: day_5_supply_stacks <part>"),
    }
}
