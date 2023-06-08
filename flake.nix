{
  description = "A type for integers modulo some constant.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        package = ghc:
          pkgs.haskell.packages."${ghc}".developPackage {
            name = "modular-arithmetic";
            root = ./.;

            source-overrides = {
              typelits-witnesses = "0.4.0.0";
            };

            overrides = self: super: with pkgs.haskell.lib; { };
          };

        shellFor = ghc:
          pkgs.haskell.packages."${ghc}".shellFor {
            packages = p: [ (package ghc) ];

            buildInputs = with haskellPackages; [
              cabal-install
              ghcid
              haskell-language-server

              pkgs.haskellPackages.cabal-fmt
              pkgs.nixpkgs-fmt
            ];
          };
      in
      rec {
        # TODO: cleaner way to manage multiple GHC versions...
        packages = {
          modular-arithmetic_961 = package "ghc961";
          modular-arithmetic_944 = package "ghc944";
          modular-arithmetic_924 = package "ghc924";
          modular-arithmetic_902 = package "ghc902";
        };

        devShells = {
          modular-arithmetic_961 = shellFor "ghc961";
          modular-arithmetic_944 = shellFor "ghc944";
          modular-arithmetic_924 = shellFor "ghc924";
          modular-arithmetic_902 = shellFor "ghc902";
        };

        defaultPackage = packages.modular-arithmetic_961;
        devShell = devShells.modular-arithmetic_961;
      });
}
