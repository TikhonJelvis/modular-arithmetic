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

            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with haskellPackages; [
                cabal-install
                ghcid
                haskell-language-server

                pkgs.haskellPackages.cabal-fmt
                pkgs.nixpkgs-fmt
              ]);
          };

        shellFor = ghc:
          pkgs.haskell.packages."${ghc}".shellFor {
            packages = p: [ (package ghc) ];
          };
      in rec {
        # TODO: cleaner way to manage multiple GHC versions...
        packages = {
          modular-arithmetic_902 = package "ghc902";
          modular-arithmetic_8107 = package "ghc8107";
        };

        devShells = {
          modular-arithmetic_902 = shellFor "ghc902";
          modular-arithmetic_8107 = shellFor "ghc8107";
        };

        defaultPackage = packages.modular-arithmetic_902;
        devShell = devShells.modular-arithmetic_902;
      });
}
