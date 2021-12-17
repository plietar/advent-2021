pkgs: with pkgs; [ idris2 gmp ] ++ lib.optional stdenv.isDarwin [ zsh ]
