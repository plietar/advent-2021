#!/usr/bin/env python3

import sys

def part1(data):
    count = 0
    previous = data[0]
    for v in data[1:]:
        if v > previous:
            count += 1
        previous = v
    print(count)

def part2(data):
    count = 0
    previous = sum(data[0:3])
    for i in range(1, len(data)-2):
        v = sum(data[i:i+3])
        if v > previous:
            count += 1
        previous = v
    print(count)

def main():
    data = list(map(int, sys.stdin.readlines()))
    if 'part1' in sys.argv[0]:
        part1(data)
    elif 'part2' in sys.argv[0]:
        part2(data)

main()
