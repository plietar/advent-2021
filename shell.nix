with import <nixpkgs> {};
mkShell {
  buildInputs = [ rustc libiconv ];
}
