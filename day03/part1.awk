#!/usr/bin/env -S gawk -f

{
  split($0, bits, "")
  for (i = 1; i <= length($0); i++) {
    if (bits[i] == "1") {
      count[i]++
    }
  }
}
END {
  gamma = 0
  for (i = 1; i <= length(count); i++) {
    gamma *= 2
    delta *= 2
    if (count[i] > NR / 2) {
      gamma += 1
    } else {
      delta += 1
    }
  }
  print gamma * delta
}
