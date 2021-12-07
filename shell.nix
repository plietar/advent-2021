with import <nixpkgs> {};
let
  subdirs = path:
    let
      isDayDir = name: type: type == "directory" && lib.hasPrefix "day" name && lib.pathExists (path + ("/" + name + "/default.nix"));
    in map (n: path + ("/" + n)) (lib.attrNames (lib.filterAttrs isDayDir (builtins.readDir ./.)));
in mkShell {
  buildInputs = lib.concatMap (d: import d pkgs) (subdirs ./.);
}
