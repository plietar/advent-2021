#!/usr/bin/env lua

require "common"

min, max, data = parse()
print(solve(min, max, data, function(d) return d * (d+1) / 2 end))
