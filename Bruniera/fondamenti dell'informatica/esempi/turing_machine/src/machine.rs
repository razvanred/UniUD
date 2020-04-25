use std::fmt;

#[derive(Clone, Copy)]
pub enum Direction {
    Left,
    Right,
}

impl fmt::Display for Direction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Direction::Left => write!(f, "Left"),
            Direction::Right => write!(f, "Right"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum Sigma {
    Empty,
    Tally,
}

impl fmt::Display for Sigma {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sigma::Empty => write!(f, "_"),
            Sigma::Tally => write!(f, "0"),
        }
    }
}

pub struct Machine<'a, Q: Copy, S: Copy, T: Fn(Q, S) -> Option<(Q, S, Direction)>> {
    tape: &'a mut [S],
    p: T,
    header: usize,
    state: Q
}

impl<'a, Q: Copy, S: Copy, T: Fn(Q, S) -> Option<(Q, S, Direction)>> Machine<'a, Q, S, T> {
    pub fn new(tape: &mut [S], q0: Q, p: T) -> Machine<Q, S, T> {
        Machine {
            tape,
            p,
            header: 0,
            state: q0,
        }
    }

    pub fn header(&self) -> usize {
        self.header
    }

    pub fn print_header(&self) {
        for _i in 0..self.header() {
            print!(" ");
        }
        println!("V");
    }
}

impl<'a, Q: Copy, S: Copy, T: Fn(Q, S) -> Option<(Q, S, Direction)>> Iterator for Machine<'a, Q, S, T> {
    type Item = (Q, S, Direction);

    fn next(&mut self) -> Option<Self::Item> {
        let step = (self.p)(self.state, self.tape[self.header]);
        if let Some((next_state, next_symbol, direction)) = step{
            self.state = next_state;
            self.tape[self.header] = next_symbol;
            if let Direction::Left = direction {
                self.header -= 1;
            } else {
                self.header += 1;
            }
        }

        step
    }
}
