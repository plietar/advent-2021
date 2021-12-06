#!/usr/bin/env -S nix-instantiate --eval
import ./main.nix (builtins.readFile /dev/stdin) 256
