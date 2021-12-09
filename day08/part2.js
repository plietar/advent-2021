#!/usr/bin/env node

var fs = require('fs');
var data = fs.readFileSync(0, 'utf-8').split("\n");

const DIGITS = {
	0b1110111: 0,
	0b0010010: 1,
	0b1011101: 2,
	0b1011011: 3,
	0b0111010: 4,
	0b1101011: 5,
	0b1101111: 6,
	0b1010010: 7,
	0b1111111: 8,
	0b1111011: 9,
}

// https://stackoverflow.com/a/20871714
const permutator = (inputArr) => {
	let result = [];

	const permute = (arr, m = []) => {
		if (arr.length === 0) {
			result.push(m)
		} else {
			for (let i = 0; i < arr.length; i++) {
				let curr = arr.slice();
				let next = curr.splice(i, 1);
				permute(curr.slice(), m.concat(next))
			}
		}
	}

	permute(inputArr)

	return result;
}

function checkAssignment(assignment, patterns) {
	for (p of patterns) {
		let N = 0
		for (c of p) {
			N |= 1 << (assignment[c])
		}
		if (!(N in DIGITS)) {
			return false;
		}
	}
	return true;
}

function findAssignment(patterns) {
	let L = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
	let result = null;
	for (l of permutator(L)) {
		let assignment = {}
		for (i in l) {
			assignment[l[i]] = i
		}
		if (checkAssignment(assignment, patterns)) {
			return assignment;
		}
	}
	throw "No valid assignment";
}

function resolve(output, assignment) {
	let N = 0
	for (c of output) {
		N |= (1 << assignment[c]);
	}
	return DIGITS[N];
}

N = 0
for (row of data) {
	if (!row.length) {
		continue;
	}
	parts = row.split(" | ");
	patterns = parts[0].split(" ");
	outputs = parts[1].split(" ");
	assignment = findAssignment(patterns)
	let x = 0;
	for (o of outputs) {
		x *= 10;
		x += resolve(o, assignment);
	}
	N += x;
}
console.log(N)
