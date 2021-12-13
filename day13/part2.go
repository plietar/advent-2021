package day13

func main() {
	points, folds := parse()

	for _, f := range folds {
		applyFold(points, f)
	}
	printPage(points)
}
