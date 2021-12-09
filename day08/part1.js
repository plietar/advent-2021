#!/usr/bin/env node

var fs = require('fs');
var data = fs.readFileSync(0, 'utf-8').split("\n");

N = 0
for (row of data) {
	MAPPING = {}
	if (!row.length) {
		continue;
	}
	parts = row.split(" | ");
	outputs = parts[1].split(" ");
	for (o of outputs) {
		if (o.length == 2 || o.length == 3 || o.length == 4 || o.length == 7) {
			N += 1
		}
	}
}
console.log(N)
