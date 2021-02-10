package main

import (

)

import AH "./adventhelper"

type Pos struct {
	x, y, gear int
}

func posToGI(x int, y int, depth int) (int) {
	if x == 0 && y == 0 {
		return 0
	} else if y == 0 {
		return (x * 16807)
	} else if x == 0 {
		return (y * 48271)
	} else {
		return posToEL(x-1, y, depth) * posToEL(x, y-1, depth)
	}
}

func posToEL(x int, y int, depth int) int {
	 return (posToGI(x, y, depth) + depth) % 20183
}

func Cave(widthX int, widthY int, targetX int, targetY int, depth int) (grid [][]int) {
	row := make([]int, widthY)
	grid = [][]int{}

	// first column
	for y := 0; y < widthY; y++ {
		el := posToEL(0, y, depth)
		row[y] = el
	}

	gridRow1 := make([]int, widthY)
	copy(gridRow1, row)
		for j := 0; j < widthY; j++ {
			gridRow1[j] %= 3
	}	
	grid = append(grid, gridRow1)

	// later columns
	for r := 1; r < widthX; r++ {
		el := posToEL(r, 0, depth)
		row[0] = el
		
		for y := 1; y < widthY; y++ {
			if r == targetX && y == targetY {
				el = ((0 + depth) % 20183)
			} else {
				el = ((row[y] * row[y-1]) + depth) % 20183
			}
			row[y] = el
		}

		gridRow := make([]int, widthY)
		copy(gridRow, row)
		for j := 0; j < widthY; j++ {
			gridRow[j] %= 3
		}	
		grid = append(grid, gridRow)
	}

	return
}

func minAlt(flagged map[Pos]int) (pMin Pos) {
	min := -1

	for k, v := range flagged {
		if (min == -1) || (v < min) {
			min  = v
			pMin = k
		}
	}
	delete(flagged, pMin)

	return
}

func nbrs(p Pos, xLim int, yLim int) (ns []Pos) {
	ns = []Pos{}

	if p.gear == 0 {
		ns = []Pos{Pos{x:p.x, y:p.y, gear:1},
	             Pos{x:p.x, y:p.y, gear:2}}
	} else if p.gear == 1 {
		ns = []Pos{Pos{x:p.x, y:p.y, gear:0},
	             Pos{x:p.x, y:p.y, gear:2}}
	} else if p.gear == 2 {
		ns = []Pos{Pos{x:p.x, y:p.y, gear:0},
	             Pos{x:p.x, y:p.y, gear:1}}
	}

	if p.x != 0 {
		ns = append(ns, Pos{x:p.x-1, y:p.y, gear:p.gear})
	}
	if p.x != xLim - 1 {
		ns = append(ns, Pos{x:p.x+1, y:p.y, gear:p.gear})
	}
	if p.y != 0 {
		ns = append(ns, Pos{x:p.x, y:p.y-1, gear:p.gear})
	}
	if p.y != yLim - 1 {
		ns = append(ns, Pos{x:p.x, y:p.y+1, gear:p.gear})
	}

	return
}

// 0 neither
// 1 torch
// 2 climbing gear

// 0 Rocky
// 1 Wet
// 2 Narrow

// In rocky regions, you can use the climbing gear or the torch. You cannot use
// 		neither (you'll likely slip and fall).
// In wet regions, you can use the climbing gear or neither tool. You cannot use
//    the torch (if it gets wet, you won't have a light source).
// In narrow regions, you can use the torch or neither tool. You cannot use the
//    climbing gear (it's too bulky to fit).

func distance(p0 Pos, p1 Pos, grid [][]int) int {
	infinity := 1000000

	// we want to change gear
	if p0.gear != p1.gear {
		if p1.gear == grid[p0.x][p0.y] {
			return infinity
		} else {
			return 7
		}
	}

	// we want to change location
	if p0.gear == grid[p1.x][p1.y] {
		return infinity
	} else {
		return 1
	}
}


func Dijkstra(grid [][]int, source Pos, target Pos) (dist map[Pos]int) {
	Q := make(map[Pos]bool)
	dist = make(map[Pos]int)
	marked := make(map[Pos]int)

	xLim := len(grid)
	yLim := len(grid[0])

	for x := 0; x < xLim; x++ {
		for y := 0; y < yLim; y++ {
			for g := 0; g < 3; g ++ {
				if (g != grid[x][y]) {
					p := Pos{x:x, y:y, gear:g}
					Q[p] = false
					dist[p] = 10000000
				}
			}
		}	
	}
	Q[source] = true
	dist[source] = 0
	marked[source] = 0

	for len(Q) > 0 {
		if len(marked) == 0 {
			return
		}
		u := minAlt(marked)

		if u == target {
			return
		}

		distU := dist[u]
		delete(Q, u)

		nbrsOfU := nbrs(u, xLim, yLim)
		for _, n := range nbrsOfU {
			if _, ok := Q[n] ; ok {
				alt := distU + distance(u, n, grid)
				if alt < dist[n] {
					dist[n] = alt
					marked[n] = alt
				}
			}
		}
	}

	return
}

func main() {
	depth := 10914
	start := Pos{x:0, y:0, gear:1}
	target := Pos{x:9, y:739, gear:1}
	grid := Cave(target.x+100, target.y+100, target.x, target.y, depth)

	total := 0
	for x := 0; x <= target.x; x++ {
		for y := 0; y <= target.y; y++ {
			total += grid[x][y]
		}
	}

	ds := Dijkstra(grid, start, target)
	AH.PrintSoln(22, total, ds[target])
}
