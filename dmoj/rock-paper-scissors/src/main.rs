/**
 *    author:  bernborgess
 *    problem: scissors - haskellLearning
 *    created: 09.August.2024 18:26:47
**/
use std::io;
use std::iter::zip;
fn get_line() -> String {
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    input.trim().to_string()
}
fn get_list() -> Vec<String> {
    get_line().split(' ').map(|s| s.to_string()).collect()
}

fn wins(r: &str, s: &str) -> bool {
    matches!(
        (r, s),
        ("rock", "scissors") | ("scissors", "paper") | ("paper", "rock")
    )
}

fn fun((a, b): (i32, i32), (r, s): (String, String)) -> (i32, i32) {
    let da = if wins(&r, &s) { 1 } else { 0 };
    let db = if wins(&s, &r) { 1 } else { 0 };
    (a + da, b + db)
}

fn main() {
    let _ = get_line();
    let a_s = get_list();
    let b_s = get_list();
    let iter = zip(a_s, b_s);
    let (a, b) = iter.fold((0, 0), fun);
    println!("{a} {b}");
}
