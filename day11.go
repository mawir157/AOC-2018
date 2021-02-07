package main

import (
	"fmt"
)

import AH "./adventhelper"

type Triple struct {
	x, y, level int
}

type Squares = map[Triple]int

func posToFuel(x int, y int, serial int) (power int) {
	rack := x + 10
	power = ((rack * y) + serial) * rack
	power = (power / 100) % 10
	power -= 5
	return
}

func rowAndColumn(x int, y int, level int, serial int) (T int) {
	for dx := 0; dx < level; dx++ {
		T += posToFuel(x+dx, y+level - 1, serial)
	}
	for dy := 0; dy < level - 1; dy++ {
		T += posToFuel(x+level - 1, y+dy, serial)
	}

	return
}

func inductionSquares(ss Squares, sides int, level int, serial int) Squares {
	for x := 0; x < sides - level + 1; x++ {
		for y := 0; y < sides - level + 1; y++ {
			triplePrev := Triple{x:x, y:y, level:level-1}
			triple := Triple{x:x, y:y, level:level}
			ss[triple] = ss[triplePrev] + rowAndColumn(x, y, level, serial)
		}
	}

	return ss
}

func maxPower(ss Squares, at int) (x int, y int, level int) {
	max := -100

	for k, v := range ss {
		if (at != -1) && (at != k.level) {
			continue
		}

		if v > max {
			x = k.x
			y = k.y
			level = k.level
			max = v
		}
	}

	return
}

func gridCount(sides int, serial int) (ss Squares) {
	ss = make(Squares)

	for x := 0; x < sides; x++ {
		for y := 0; y < sides; y++ {
			triple := Triple{x:x, y:y, level:1}
			ss[triple] = posToFuel(x, y, serial)
		}
	}

	for i := 2; i < 50; i++ {
		ss = inductionSquares(ss, sides, i, serial)
	}

	return
}

func main() {
	serial := 3628

	r := gridCount(300, serial)
	x1, y1, _  := maxPower(r, 3)
	x2, y2, l2 := maxPower(r, -1)

	AH.PrintSoln(11, fmt.Sprintf("%d,%d", x1, y1),
	                 fmt.Sprintf("%d,%d,%d", x2, y2, l2))

	return
}
