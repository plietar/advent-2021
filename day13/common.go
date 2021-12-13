package day13

import "fmt"

type Point struct {
	x uint
	y uint
}
type Fold struct {
	value uint
	axis  rune
}

type Points map[Point]bool

func parse() (Points, []Fold) {
	var points Points = make(Points)
	var folds []Fold
	for {
		var p Point
		_, err := fmt.Scanf("%d,%d\n", &p.x, &p.y)
		if err != nil {
			break
		}
		points[p] = true
	}
	for {
		var f Fold
		_, err := fmt.Scanf("fold along %c=%d\n", &f.axis, &f.value)
		if err != nil {
			break
		}
		folds = append(folds, f)
	}
	return points, folds
}

func applyFold(points Points, fold Fold) {
	for point := range points {
		if fold.axis == 'y' && point.y > fold.value {
			delete(points, point)
			points[Point{point.x, 2*fold.value - point.y}] = true
		} else if fold.axis == 'x' && point.x > fold.value {
			delete(points, point)
			points[Point{2*fold.value - point.x, point.y}] = true
		}
	}
}

func printPage(points Points) {
	var w uint = 0
	var h uint = 0
	for p := range points {
		if p.x >= w {
			w = p.x + 1
		}
		if p.y >= h {
			h = p.y + 1
		}
	}

	for y := uint(0); y < h; y++ {
		for x := uint(0); x < w; x++ {
			if points[Point{x, y}] {
				fmt.Print("X")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}
