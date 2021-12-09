#!/usr/bin/env php
<?php

$CELLS = array();
while ($line = fgets(STDIN)) {
	$CELLS[] = str_split(trim($line));
}
$H = count($CELLS);
$W = count($CELLS[0]);

for ($x = 0; $x < $W; $x++) {
	for ($y = 0; $y < $H; $y++) {
		$CELLS[$y][$x] = intval($CELLS[$y][$x]);
	}
}

function value($x, $y) {
	global $H, $W, $CELLS;
	if ($x >= 0 && $x < $W && $y >= 0 && $y < $H) {
		return $CELLS[$y][$x];
	}
	return 9;
}

function neighbour_values($x, $y) {
	return [
		value($x+1, $y),
		value($x-1, $y),
		value($x, $y+1),
		value($x, $y-1),
	];
}

function add_neighbour_basin(&$todo, &$visited, $x, $y) {
	global $W;
	$N = $y * $W + $x;
	if (!array_key_exists($N, $visited) && value($x, $y) < 9) {
		$todo[] = [$x, $y];
	}
}

function find_basin($x, $y) {
	$visited = [];
	$todo = [[$x, $y]];

	$size = 0;
	global $H, $W, $CELLS;
	while (count($todo) > 0) {
		[$x, $y] = array_pop($todo);
		$N = $y * $W + $x;
		if (!array_key_exists($N, $visited)) {
			$visited[$N] = true;
			$size += 1;
			add_neighbour_basin($todo, $visited, $x+1, $y);
			add_neighbour_basin($todo, $visited, $x-1, $y);
			add_neighbour_basin($todo, $visited, $x, $y+1);
			add_neighbour_basin($todo, $visited, $x, $y-1);
		}
	}
	return $size;
}

$BASINS = [];
for ($x = 0; $x < $W; $x++) {
	for ($y = 0; $y < $H; $y++) {
		if ($CELLS[$y][$x] < min(neighbour_values($x, $y))) {
			$BASINS[] = find_basin($x, $y);
		}
	}
}
rsort($BASINS);
$result = $BASINS[0] * $BASINS[1] * $BASINS[2];
echo "$result";
