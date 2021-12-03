#!/usr/bin/env gawk -f

{
  oxygen[NR] = $0
  co2[NR] = $0
}
END {
  print(solve(oxygen, 1) * solve(co2, 0))
}

function copy_array(src, dst, i) {
  delete dst
  for (i in src) {
    dst[i] = src[i]
  }
}

function solve(candidates, mode) {
  for (i=1;length(candidates) > 1;i++) {
    count = 0
    N = length(candidates)
    delete positives
    delete negatives
    for (c in candidates) {
      split(candidates[c], bits, "")
      if (bits[i] == "1") {
        positives[c] = candidates[c]
      } else {
        negatives[c] = candidates[c]
      }
    }

    has_positive = length(positives) >= length(candidates) / 2
    if (mode == has_positive) {
      copy_array(positives, candidates)
    } else {
      copy_array(negatives, candidates)
    }
  }
  for (c in candidates) {
    return b2d(candidates[c])
  }
}

function b2d(s, bits, i, N) {
  split(s, bits, "")
  for (i in bits) {
    N *= 2
    if (bits[i] == "1") {
      N += 1
    }
  }
  return N
}
