{ sources ? import nix/sources.nix {}
, compiler-nix-name ? "ghc884"
}:
let
  haskellNix = import sources."haskell.nix" {};
  pkgs = import haskellNix.sources.nixpkgs-2003 haskellNix.nixpkgsArgs;
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "modular-arithmetic";
    src = ./.;
  };
  inherit compiler-nix-name;
}
