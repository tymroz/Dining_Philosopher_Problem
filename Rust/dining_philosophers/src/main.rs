use rand::Rng;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

const NUM_PHILOSOPHERS: usize = 5;

struct Philosopher {
    id: usize,
    left_fork: Arc<Mutex<()>>,
    right_fork: Arc<Mutex<()>>,
}

impl Philosopher {
    fn new(id: usize, left_fork: Arc<Mutex<()>>, right_fork: Arc<Mutex<()>>) -> Self {
        Self { id, left_fork, right_fork }
    }

    fn dine(&self) {
        for _ in 0..20 {
            self.think();
            self.eat();
        }
    }

    fn think(&self) {
        println!("Filozof {} myśli", self.id);
        thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(0..1000)));
    }

    fn eat(&self) {
        let _left = self.left_fork.lock().unwrap();
        let _right = self.right_fork.lock().unwrap();

        println!("Filozof {} je", self.id);
        thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(0..1000)));

        println!("Filozof {} skończył jeść", self.id);
    }
}

fn main() {
    let forks: Vec<_> = (0..NUM_PHILOSOPHERS).map(|_| Arc::new(Mutex::new(()))).collect();
    let philosophers: Vec<_> = (0..NUM_PHILOSOPHERS)
        .map(|i| {
            Philosopher::new(
                i + 1,
                Arc::clone(&forks[i]),
                Arc::clone(&forks[(i + 1) % NUM_PHILOSOPHERS]),
            )
        })
        .collect();

    let handles: Vec<_> = philosophers
        .into_iter()
        .map(|p| {
            thread::spawn(move || {
                p.dine();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}
