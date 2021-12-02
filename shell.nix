with import <nixpkgs> {};
let subdirs = [ ./day01 ./day02 ];
in mkShell {
  buildInputs = lib.concatMap (d: import d pkgs) subdirs;
}
