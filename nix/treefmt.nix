{ pkgs, ... }:
{
  projectRootFile = "flake.nix";

  # Nix
  programs.nixpkgs-fmt.enable = true;

  # Haskell
  programs.stylish-haskell.enable = true;
  programs.hlint.enable = true;
}
