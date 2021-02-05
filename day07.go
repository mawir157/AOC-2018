package main

import (
	"sort"
)

import AH "./adventhelper"

type reln struct {
	from, to rune
}

type System struct {
	rs      []reln
	verts   map[rune]bool
	counter map[rune]int
	workers []rune
	word    string
	time    int
}

func (sys System) Done() bool {
	for _, v := range sys.verts {
		if v {
			return false
		}
	}
	return true
}

func unique(e []rune) []rune {
    r := []rune{}

    for _, s := range e {
        if !contains(r[:], s) {
            r = append(r, s)
        }
    }
    return r
}

func contains(e []rune, c rune) bool {
    for _, s := range e {
        if s == c {
            return true
        }
    }
    return false
}

func all(rs []reln) (vs []rune) {
	for _, r := range rs {
		vs = append(vs, r.from)
		vs = append(vs, r.to)
	}

	vs = unique(vs)
	sort.Slice(vs, func(i, j int) bool { return vs[i] < vs[j] })

	return
}

func hasParent(r rune, rs []reln) (bool) {
	for _, v := range rs {
		if v.to == r {
			return true
		}
	}

	return false
}

func (sys *System) tick2() () {
	os := []rune{}
	for r, c := range sys.counter {
		if r == 0 {
			continue
		}
		if c <= 0 {
			continue
		}
		if hasParent(r, sys.rs) {
			continue
		}
		os = append(os, r)
	}
	sort.Slice(os, func(i, j int) bool { return os[i] < os[j] })

	for i, w := range sys.workers {
		if w == 0 { // worker is free
			for _, o := range os { // find an orphan that has not already been assigned?
				if !contains(sys.workers, o) {
					sys.workers[i] = o
					break
				}
			}
		}	
	}

	for _, w := range sys.workers {
		sys.counter[w]--
	}

	ss := []reln{}
	for i, w := range sys.workers {
		ss = []reln{}
		if sys.counter[w] == 0 {
			sys.word = sys.word + string(w)
			sys.verts[w] = false
			for _, s := range sys.rs {
				if s.from != w {
					ss = append(ss, s)
				}
			}
			sys.rs = ss
			sys.workers[i] = 0
		}
	}

	sys.time++

	return
}

func (sys *System) disassemble2() {
	for !sys.Done() {
		sys.tick2()
	}
}

func main() {
	ss, _ := AH.ReadStrFile("input_07.txt")

	counters1 := make(map[rune]int)
	counters1[0] = 1000 // garbage
	for i := 1; i <= 26; i++ {
		counters1[rune(i+64)] = 1
	}
	
	counters2 := make(map[rune]int)
	counters2[0] = 1000 // garbage
	for i := 1; i <= 26; i++ {
		counters2[rune(i+64)] = i + 60
	}

	var graph []reln
	var sys1, sys2 System
	for _,s := range(ss) {
		rs := []rune(s)
		graph = append(graph, reln{from:rs[5], to:rs[36]})
	}

	sys1 = System{rs:graph, counter:counters1, workers:[]rune{0}, time:0}
	sys2 = System{rs:graph, counter:counters2, workers:[]rune{0,0,0,0,0}, time:0}

	seen1 := make(map[rune]bool)
	seen2 := make(map[rune]bool)
	for _, v := range all(graph) {
		seen1[v] = true
		seen2[v] = true
	}

	sys1.verts = seen1
	sys2.verts = seen2

	sys1.disassemble2()
	sys2.disassemble2()

	AH.PrintSoln(7, sys1.word, sys2.time)

	return
}
