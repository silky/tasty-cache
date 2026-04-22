{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;
      in
      {
        # Run via `nix fmt`
        formatter = treefmtEval.config.build.wrapper;

        # Nix flake check
        checks = {
          formatting = treefmtEval.config.build.check self;
        };

        devShells = {
          default = pkgs.mkShell {
            packages =
              let
                hask = pkgs.haskell.packages.ghc98.override { };
              in
              with pkgs; [
                cabal-install
                pkgs.haskell.packages.ghc98.hiedb
                (hask.ghcWithPackages (ps: with ps; [
                  bytestring
                  containers
                  directory
                  filepath
                  tasty-hunit
                  template-haskell
                  tasty
                ]))
              ];
          };
        };
      }
    );
}
