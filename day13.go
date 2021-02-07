package main

import (
	"fmt"
	"sort"
)

import AH "./adventhelper"

type Track int

const (
        RotR Track = iota
        RotL
        X
)

type Dir int

const (
        L Dir = iota
        F
        R
)

type Pos struct {
	x, y int
}

type Train struct {
	p      Pos
	v      int // 0 right, 1 up, 2 left, 3 down 
	turn   Dir
	active bool
}

func (t *Train) Move() {
	switch t.v {
		case 0: t.p.y += 1
		case 1: t.p.x -= 1
		case 2: t.p.y -= 1
		case 3: t.p.x += 1
	}
}

func (t *Train) Turn(r Track) {
	switch r {
		case RotR: // /
			t.v = (5 - t.v) % 4
		case X:
			if t.turn == L {
				t.v = (t.v + 1) % 4
				t.turn = F
			} else if t.turn == R {
				t.v = (t.v + 3) % 4
				t.turn = L
			} else { // t.turn == F
				t.turn = R
			}
		case RotL: // \
			t.v = 3 - t.v
	}
}

func createTrain(p Pos, r rune) (t Train) {
	t = Train{p:p, turn:L, active:true}
	switch r {
		case rune('^'): t.v = 1
		case rune('>'): t.v = 0
		case rune('v'): t.v = 3
		case rune('<'): t.v = 2
	}

	return
}

func parseInput(ss []string) (map[Pos]Track, []Train) {
	track := make(map[Pos]Track)
	trains := []Train{} 

	for x, s := range ss {
		rs := []rune(s)
		for y, r := range rs {
			p := Pos{x:x, y:y}
			switch r {
				case rune('/'):  track[p] = RotR
				case rune('\\'): track[p] = RotL
				case rune('+'):  track[p] = X
				case rune('^'):  fallthrough
				case rune('<'):  fallthrough
				case rune('>'):  fallthrough
				case rune('v'):  trains = append(trains, createTrain(p, r))
			}
		}
	}

	return track, trains
}

func tick(track map[Pos]Track, trains []Train) (bool, Pos) {
	sort.Slice(trains[:], func(i, j int) bool {
    return trains[i].p.y < trains[j].p.y
	})

	crash := false
	location := Pos{x:0, y:0}

	for i, _ := range trains {
		if !trains[i].active {
			continue
		}

		trains[i].Move() // move the train forward
		if tPart, ok := track[trains[i].p]; ok { // check if there is relevant track here
			trains[i].Turn(tPart)
		}

		// check for collisions
		loc := trains[i].p
		trainCount := 0
		crashIndices := []int{}
		for j, tt := range trains {
			if (loc == tt.p) && tt.active {
				trainCount++
				crashIndices = append(crashIndices, j)
			}
		}

		if trainCount > 1 {
			trains[i].active = false
			for _, j := range crashIndices {
				trains[j].active = false
			}

			crash = true
			location = loc
		}		
	}

	return crash, location
}

func run(track map[Pos]Track, trains []Train) (loc Pos) {
	crashed := false
	for !crashed {
		crashed, loc = tick(track, trains)
	}
	return
}

func countActive(trains []Train) (total int) {
	total = 0
	for _, t := range trains {
		if t.active {
			total++
		}
	}

	return
}

func part2(track map[Pos]Track, trains []Train) (loc Pos) {
	activeTrains := countActive(trains)

	for activeTrains > 1 {
		run(track, trains)
		activeTrains = countActive(trains)
	}

	for _, t := range trains {
		if t.active {
			loc = t.p
		}
	}
	return
}

func main() {
	ss, _ := AH.ReadStrFile("input_13.txt")
	track, trains := parseInput(ss)

	p1 := run(track, trains)
	p2 := part2(track, trains)

	AH.PrintSoln(13, fmt.Sprintf("%d,%d", p1.y, p1.x),
	                 fmt.Sprintf("%d,%d", p2.y, p2.x))

}
