{
  description = "Tasty ingredient that skips unchanged tests using GHC HIE files";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix }:
    let
      ghcVersion = "ghc98";

      # Source filter: stop cache/build dirs invalidating the store path.
      src = nixpkgs.lib.cleanSourceWith {
        src = ./.;
        name = "tasty-cache-source";
        filter = path: type:
          let base = baseNameOf (toString path); in
          !(builtins.elem base [
            ".git"
            ".direnv"
            ".hie"
            ".cache"
            "dist-newstyle"
            "result"
          ]) && !(nixpkgs.lib.hasPrefix "result-" base);
      };

      # Overlay: inject tasty-cache into haskell.packages.<ghcVersion>
      # so downstream consumers can use:
      #
      #   pkgs.haskell.packages.ghc98.ghcWithPackages (p: [ p.tasty-cache ])
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVersion} = prev.haskell.packages.${ghcVersion}.extend
              (hfinal: hprev: {
                tasty-cache =
                  prev.haskell.lib.compose.dontCheck
                    (hfinal.callCabal2nix "tasty-cache" src { });
              });
          };
        };
      };
    in
    {
      overlays.default = overlay;
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        hsPkgs = pkgs.haskell.packages.${ghcVersion};
        hsLib = pkgs.haskell.lib.compose;
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;
      in
      {
        packages = {
          default = hsPkgs.tasty-cache;
          tasty-cache = hsPkgs.tasty-cache;
        };

        devShells.default = hsPkgs.shellFor {
          packages = ps: [ ps.tasty-cache ];
          withHoogle = false;
          doBenchmark = false;
          nativeBuildInputs = [
            pkgs.cabal-install
            hsPkgs.hiedb
          ];
        };

        # Run via `nix fmt`
        formatter = treefmtEval.config.build.wrapper;

        # Nix flake check
        checks = {
          formatting = treefmtEval.config.build.check self;
          tasty-cache = hsPkgs.tasty-cache;
          tasty-cache-tests = hsLib.doCheck hsPkgs.tasty-cache;
        };
      });
}
