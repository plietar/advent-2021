with import <nixpkgs> {};
let subdirs = [ ./day01 ./day02 ./day03 ./day04 ./day05 ];
in mkShell {
  buildInputs = lib.concatMap (d: import d pkgs) subdirs;
}
