use rand::Rng;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

const NUM_PHILOSOPHERS: usize = 5;
const NUM_OF_MEALS: usize = 10;
const STARVATION_THRESHOLD: Duration = Duration::from_secs(5);

struct Philosopher {
    id: usize,
    left_fork: Arc<Mutex<()>>,
    right_fork: Arc<Mutex<()>>,
    last_meal_time: Arc<Mutex<Instant>>,
    starved: Arc<Mutex<bool>>,
}

impl Philosopher {
    fn new(id: usize, left_fork: Arc<Mutex<()>>, right_fork: Arc<Mutex<()>>) -> Self {
        Self {
            id,
            left_fork,
            right_fork,
            last_meal_time: Arc::new(Mutex::new(Instant::now())),
            starved: Arc::new(Mutex::new(false)),
        }
    }

    fn dine(self: Arc<Self>) {
        for _ in 0..NUM_OF_MEALS {
            self.think();
            if self.eat() {
                break;
            }
        }
    }

    fn think(&self) {
        println!("Filozof {} myśli", self.id);
        thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(0..1000)));
        println!("Filozof {} jest głodny", self.id);
    }

    fn eat(&self) -> bool {
        let now = Instant::now();
        
        {
            let last_meal_time = self.last_meal_time.lock().unwrap();
            if now.duration_since(*last_meal_time) > STARVATION_THRESHOLD {
                let mut starved = self.starved.lock().unwrap();
                *starved = true;
                println!("Filozof {} zagłodził się", self.id);
                return true;
            }
        }

        let _left = self.left_fork.lock().unwrap();
        let _right = self.right_fork.lock().unwrap();

        {
            let mut last_meal_time = self.last_meal_time.lock().unwrap();
            *last_meal_time = now;
        }

        println!("Filozof {} je", self.id);
        thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(0..1000)));

        println!("Filozof {} skończył jeść", self.id);
        false
    }
}

fn main() {
    let forks: Vec<_> = (0..NUM_PHILOSOPHERS).map(|_| Arc::new(Mutex::new(()))).collect();
    let philosophers: Vec<_> = (0..NUM_PHILOSOPHERS)
        .map(|i| {
            if i == NUM_PHILOSOPHERS - 1 {
                Arc::new(Philosopher::new(
                    i + 1,
                    Arc::clone(&forks[0]),
                    Arc::clone(&forks[i]),
                ))
            } else {
                Arc::new(Philosopher::new(
                    i + 1,
                    Arc::clone(&forks[i]),
                    Arc::clone(&forks[(i + 1) % NUM_PHILOSOPHERS]),
                ))
            }
        })
        .collect();

    let handles: Vec<_> = philosophers
        .into_iter()
        .map(|p| {
            let p_clone = Arc::clone(&p);
            thread::spawn(move || {
                p_clone.dine();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}
