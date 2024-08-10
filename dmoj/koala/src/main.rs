use std::io;
fn main(){
    let mut i=String::new();
    io::stdin().read_line(&mut i);
    i.trim().parse::<i32>().map(|x|println!("{}",x-1));
}
