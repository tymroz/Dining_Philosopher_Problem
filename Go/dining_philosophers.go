package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

const numPhilosophers = 5
const numberOfMeals = 10
const starvationThreshold = 5000

type Philosopher struct {
	id               int
	leftFork, rightFork *sync.Mutex
	lastMealTime     time.Time
	starved          bool
}

func (p *Philosopher) dine(wg *sync.WaitGroup, mu *sync.Mutex) {
	defer wg.Done()
	for i := 0; i < numberOfMeals; i++ {
		p.think()
		p.eat(mu)
		if p.starved {
			fmt.Printf("Filozof %d zagłodził się\n", p.id)
			return
		}
	}
}

func (p *Philosopher) think() {
	fmt.Printf("Filozof %d myśli\n", p.id)
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)
	fmt.Printf("Filozof %d jest głodny\n", p.id)
}

func (p *Philosopher) eat(mu *sync.Mutex) {
	mu.Lock()
	if time.Since(p.lastMealTime).Milliseconds() > starvationThreshold {
		p.starved = true
		mu.Unlock()
		fmt.Printf("Filozof %d zostal zaglodzony\n", p.id)
		return
	}
	p.lastMealTime = time.Now()
	mu.Unlock()

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

	var philosophers [numPhilosophers]*Philosopher
	for i := 0; i < numPhilosophers; i++ {
		if i == numPhilosophers-1 {
			philosophers[i] = &Philosopher{
				id:           i + 1,
				leftFork:     forks[0],
				rightFork:    forks[i],
				lastMealTime: time.Now(),
			}
		} else {
			philosophers[i] = &Philosopher{
				id:           i + 1,
				leftFork:     forks[i],
				rightFork:    forks[(i+1)%numPhilosophers],
				lastMealTime: time.Now(),
			}
		}
	}

	var wg sync.WaitGroup
	wg.Add(numPhilosophers)

	var mu sync.Mutex
	for i := 0; i < numPhilosophers; i++ {
		go philosophers[i].dine(&wg, &mu)
	}

	wg.Wait()
}
