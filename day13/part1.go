package day13

import (
	"fmt"
)

func main() {
	points, folds := parse()
	applyFold(points, folds[0])
	fmt.Println(len(points))
}
