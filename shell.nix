{ compiler-nix-name ? "ghc884" }:

(import ./. { inherit compiler-nix-name; }).shellFor {
  tools = { cabal = "3.2.0.0"; };
  exactDeps = true;
}
