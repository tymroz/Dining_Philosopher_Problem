package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const numPhilosophers = 5

const numberOfMeals = 20

type Philosopher struct {
	id               int
	leftFork, rightFork *sync.Mutex
}

func (p Philosopher) dine(wg *sync.WaitGroup) {
	defer wg.Done()
	for i := 0; i < numberOfMeals; i++ {
		p.think()
		p.eat()
	}
}

func (p Philosopher) think() {
	fmt.Printf("Filozof %d myśli\n", p.id)
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
	fmt.Printf("Filozof %d jest głodny\n", p.id)
}

func (p Philosopher) eat() {
	p.leftFork.Lock()
	p.rightFork.Lock()

	fmt.Printf("Filozof %d je\n", p.id)
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)

	p.rightFork.Unlock()
	p.leftFork.Unlock()

	fmt.Printf("Filozof %d skończył jeść\n", p.id)
}

func main() {
	rand.Seed(time.Now().UnixNano())

	var forks [numPhilosophers]*sync.Mutex
	for i := 0; i < numPhilosophers; i++ {
		forks[i] = &sync.Mutex{}
	}

	var philosophers [numPhilosophers]Philosopher
	for i := 0; i < numPhilosophers; i++ {
		if i == numPhilosophers -1 {
			philosophers[i] = Philosopher{
				id:        i + 1,
				leftFork:  forks[0],
				rightFork: forks[i],
			}
		} else {
			philosophers[i] = Philosopher{
				id:        i + 1,
				leftFork:  forks[i],
				rightFork: forks[(i+1)%numPhilosophers],
			}
		}
	}

	var wg sync.WaitGroup
	wg.Add(numPhilosophers)

	for i := 0; i < numPhilosophers; i++ {
		go philosophers[i].dine(&wg)
	}

	wg.Wait()
}