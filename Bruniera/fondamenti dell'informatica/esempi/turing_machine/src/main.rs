mod machine;
use machine::*;
use machine::Sigma::*;
use std::fmt;

fn main() {
    println!("Turing Machine");
    //let mut tape = [Empty, Empty, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Empty, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Tally, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty];
    let mut tape = [Empty, Empty, Empty, Tally, Tally, Tally, Tally, Empty, Tally, Tally, Tally, Empty, Tally, Tally, Empty, Empty, Tally, Empty, Empty, Tally, Empty, Empty, Empty, Empty, Tally, Tally, Empty, Tally, Empty, Empty];
    print_condensed_array(&tape);
    let mut prod = Machine::new(&mut tape, States::Start, map);
    let mut count = 0;

    println!("Start: {}", count);

    for (q, s, dir) in &mut prod {
        println!("{} {} {}", q, s, dir);
        count += 1;
    }

    println!("Total: {}", count);
    prod.print_header();
    let header = prod.header();
    print_condensed_array(&tape);
    let expr = decode_tape(&tape, header);
    println!("{} * {} = {}", expr.1, expr.0, expr.2);
}

#[derive(Copy, Clone)]
enum States {
    Start,
    FirstSum,
    FirstCopy,
    FirstScan,
    FirstSkip,
    Isolate,
    BackCopy1,
    BackCopy2,
    Copy1,
    Copy2,
    Copy3,
    BackSum1,
    BackSum2,
    Sum1,
    Sum2,
    NextEmpty,
}

fn map(q: States, s: Sigma) -> Option<(States, Sigma, Direction)> {
    use States::*;
    use Direction::*;

    match q {
        Start => match s {
            Empty => Some((Start, Empty, Right)),
            Tally => Some((FirstSum, Empty, Right)),
        },
        FirstSum => match s {
            Empty => Some((FirstCopy, Empty, Right)),
            Tally => Some((FirstSum, Tally, Right)),
        },
        FirstCopy => match s {
            Empty => Some((FirstCopy, Empty, Right)),
            Tally => Some((FirstScan, Empty, Right)),
        },
        FirstScan => match s {
            Empty => Some((FirstSkip, Empty, Right)),
            Tally => Some((FirstScan, Tally, Right)),
        },
        FirstSkip => Some((Isolate, Tally, Right)),
        Isolate => Some((BackCopy1, Empty, Left)),
        BackCopy1 => match s {
            Empty => Some((BackCopy2, Empty, Left)),
            Tally => Some((BackCopy1, Tally, Left))
        },
        BackCopy2 => match s {
            Empty => Some((Copy1, Tally, Right)),
            Tally => Some((BackCopy2, Tally, Left)),
        },
        Copy1 => match s {
            Empty => Some((BackSum1, Empty, Left)),
            Tally => Some((Copy2, Empty, Right))
        },
        Copy2 => match s {
            Empty => Some((Copy3, Empty, Right)),
            Tally => Some((Copy2, Tally, Right)),
        },
        Copy3 => match s {
            Empty => Some((Isolate, Tally, Right)),
            Tally => Some((Copy3, Tally, Right)),
        },
        BackSum1 => match s {
            Empty => Some((BackSum2, Empty, Left)),
            Tally => Some((BackSum1, Tally, Left))
        },
        BackSum2 => match s {
            Empty => Some((Sum1, Tally, Right)),
            Tally => Some((BackSum2, Tally, Left)),
        },
        Sum1 => match s {
            Empty => Some((NextEmpty, Empty, Right)),
            Tally => Some((Sum2, Empty, Right))
        },
        Sum2 => match s {
            Empty => Some((Copy1, Empty, Right)),
            Tally => Some((Sum2, Tally, Right)),
        },
        NextEmpty => match s {
            Empty => None,
            Tally => Some((NextEmpty, Tally, Right)),
        },
    }
}

fn print_condensed_array<T: fmt::Display>(array: &[T]) {
    for i in array {
        print!("{}", i);
    }
    println!();
}

fn decode_tape(tape: &[Sigma], header: usize) -> (usize, usize, usize) {
    let mut iter = header - 1;
    (
        {
            let mut counter = 0;
            loop{
                if let Empty = tape[iter] {
                    iter -= 1;
                    break counter
                }
                iter -= 1;
                counter += 1;
            }
        },
        {
            let mut counter = 0;
            loop{
                if let Empty = tape[iter] {
                    break counter
                }
                iter -= 1;
                counter += 1;
            }
        },
        {
            let mut iter = header + 1;
            let mut counter = 0;
            loop{
                if let Empty = tape[iter] {
                    break counter
                }
                iter += 1;
                counter += 1;
            }
        },
        )
}

impl fmt::Display for States {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self as i32)
    }
}
