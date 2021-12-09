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

$risk = 0;
for ($x = 0; $x < $W; $x++) {
	for ($y = 0; $y < $H; $y++) {
		if ($CELLS[$y][$x] < min(neighbour_values($x, $y))) {
			$risk += $CELLS[$y][$x] + 1;
		}
	}
}

echo "$risk";
