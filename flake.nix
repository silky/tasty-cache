{
  description = "Tasty ingredient that skips unchanged tests using GHC HIE files";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nix-fast-build.url = "github:Mic92/nix-fast-build";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, nix-fast-build }:
    let
      # GHC versions tasty-cache is tested against. Each label gets a
      #   packages.tasty-cache-<label>         (library only, dontCheck'd)
      #   checks.tasty-cache-<label>-tests     (library + test-suite, doCheck'd)
      # The right-hand side pins the exact nixpkgs haskell.packages
      # attribute matching the cabal `tested-with` patch version, so a
      # nixpkgs default-bump cannot silently drift the matrix.
      ghcVersions = {
        ghc94 = "ghc948";
        ghc96 = "ghc967";
        ghc98 = "ghc984";
        ghc910 = "ghc9103";
        ghc912 = "ghc9124";
        ghc914 = "ghc9141";
      };

      ghcLabels = builtins.attrNames ghcVersions;

      # Label used by `packages.default`, `packages.tasty-cache` and the dev shell.
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
          extendOne = label:
            prev.haskell.packages.${ghcVersions.${label}}.extend
              (hfinal: hprev: {
                # doJailbreak: ignore upper bounds in tasty-cache.cabal at
                # build time. The cabal bounds remain authoritative for
                # Hackage; doJailbreak just lets the matrix exercise newer
                # bundled libraries (e.g. containers-0.8 with GHC 9.14)
                # without waiting for a bound bump + release cycle.
                tasty-cache =
                  prev.haskell.lib.compose.dontCheck
                    (prev.haskell.lib.compose.doJailbreak
                      (hfinal.callCabal2nix "tasty-cache" src { }));
              });
          extended = builtins.listToAttrs
            (map
              (label: {
                name = ghcVersions.${label};
                value = extendOne label;
              })
              ghcLabels);
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
          overlays = [
            overlay
            (final: _: {
              nix-fast-build = nix-fast-build.packages.${final.system}.default;
            })
          ];
        };
        hsLib = pkgs.haskell.lib.compose;
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./nix/treefmt.nix;

        tastyCacheFor = label:
          pkgs.haskell.packages.${ghcVersions.${label}}.tasty-cache;
        hsPkgsDefault = pkgs.haskell.packages.${ghcVersions.${defaultGhc}};

        # Hackage upload artifacts, built from the defaultGhc package.
        hackageSdist = hsLib.sdistTarball hsPkgsDefault.tasty-cache;
        hackageDocs = hsLib.documentationTarball
          (hsLib.doHaddock hsPkgsDefault.tasty-cache);
      in
      {
        packages = {
          default = hsPkgsDefault.tasty-cache;
          tasty-cache = hsPkgsDefault.tasty-cache;
          hackage = pkgs.symlinkJoin {
            name = "tasty-cache-hackage-packages";
            paths = [ hackageSdist hackageDocs ];
          };
        } // builtins.listToAttrs (map
          (label: {
            name = "tasty-cache-${label}";
            value = tastyCacheFor label;
          })
          ghcLabels);

        devShells.default = hsPkgsDefault.shellFor {
          packages = ps: [ ps.tasty-cache ];
          withHoogle = false;
          doBenchmark = false;
          nativeBuildInputs = [
            pkgs.cabal-install
            hsPkgsDefault.hiedb
            pkgs.just
            pkgs.nix-fast-build.out
          ];
        };

        # Run via `nix fmt`
        formatter = treefmtEval.config.build.wrapper;

        # `nix flake check` runs treefmt and builds + tests the package on
        # every supported GHC version (the multi-version test matrix).
        checks = {
          formatting = treefmtEval.config.build.check self;
        } // builtins.listToAttrs (map
          (label: {
            name = "tasty-cache-${label}-tests";
            value = hsLib.doCheck (tastyCacheFor label);
          })
          ghcLabels);
      });
}
