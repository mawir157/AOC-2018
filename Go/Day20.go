package main

import AH "./adventhelper"

import (
	"regexp"
	"strings"
)
func reduce(ss string) string {
	regex := "\\([NESW]*[\\|[NESW]*]*\\)"
	re := regexp.MustCompile(regex)
	match := re.Find([]byte(ss))

	if len(match) == 0 {
		return ss
	}
	
	s := string(match)
	s = AH.TrimFirstRune(AH.TrimLastRune(s))
	qs := strings.Split(s, "|")

	repl := ""
	longest := 0
	for _, is := range qs {
		if len(is) > longest {
			repl = is
			longest = len(is)
		}
	}

	flag := false
  new := re.ReplaceAllStringFunc(ss, func(a string) string {
      if flag {
          return a
      }
      flag = true
      return re.ReplaceAllString(a, repl)
  })

  return new
}

func collapse(ss string) string {
	ok := true
	for ok {
		tt := reduce(ss)
		ok = (tt != ss)
		ss = tt
	}

	ok = true
	for ok {
		re := regexp.MustCompile("(NS|SN|EW|WE)")
		tt := re.ReplaceAllString(ss, "")
		ok = (tt != ss)
		ss = tt
	}

	return ss
}

func main() {
	fl, _ := AH.ReadStrFile("../input/input_20.txt")
	ss := AH.TrimFirstRune(AH.TrimLastRune(fl[0]))

 	new := collapse(ss)
	AH.PrintSoln(20, len(new), 20)

	return
}