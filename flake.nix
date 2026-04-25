{
  description = "Tasty ingredient that skips unchanged tests using GHC HIE files";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix }:
    let
      # GHC versions tasty-cache is tested against. Each one gets a
      #   packages.tasty-cache-<v>          (library only, dontCheck'd)
      #   checks.tasty-cache-<v>-tests      (library + test-suite, doCheck'd)
      ghcVersions = [
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
        "ghc912"
        "ghc914"
      ];

      # GHC used by `packages.default`, `packages.tasty-cache` and the dev shell.
      defaultGhc = "ghc98";

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

      # Overlay: inject tasty-cache into every supported haskell.packages.<v>
      # so downstream consumers can pick whichever GHC they're targeting:
      #
      #   pkgs.haskell.packages.ghc910.ghcWithPackages (p: [ p.tasty-cache ])
      overlay = final: prev:
        let
          extendOne = ghcVer:
            prev.haskell.packages.${ghcVer}.extend
              (hfinal: hprev: {
                tasty-cache =
                  prev.haskell.lib.compose.dontCheck
                    (hfinal.callCabal2nix "tasty-cache" src { });
              });
          extended = builtins.listToAttrs
            (map (v: { name = v; value = extendOne v; }) ghcVersions);
        in
        {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // extended;
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
        hsLib = pkgs.haskell.lib.compose;
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;

        tastyCacheFor = ghcVer: pkgs.haskell.packages.${ghcVer}.tasty-cache;
        hsPkgsDefault = pkgs.haskell.packages.${defaultGhc};
      in
      {
        packages = {
          default = hsPkgsDefault.tasty-cache;
          tasty-cache = hsPkgsDefault.tasty-cache;
        } // builtins.listToAttrs (map
          (v: {
            name = "tasty-cache-${v}";
            value = tastyCacheFor v;
          })
          ghcVersions);

        devShells.default = hsPkgsDefault.shellFor {
          packages = ps: [ ps.tasty-cache ];
          withHoogle = false;
          doBenchmark = false;
          nativeBuildInputs = [
            pkgs.cabal-install
            hsPkgsDefault.hiedb
          ];
        };

        # Run via `nix fmt`
        formatter = treefmtEval.config.build.wrapper;

        # `nix flake check` runs treefmt and builds + tests the package on
        # every supported GHC version (the multi-version test matrix).
        checks = {
          formatting = treefmtEval.config.build.check self;
        } // builtins.listToAttrs (map
          (v: {
            name = "tasty-cache-${v}-tests";
            value = hsLib.doCheck (tastyCacheFor v);
          })
          ghcVersions);
      });
}
