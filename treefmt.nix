{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs.hlint.enable = true;
  programs.ormolu.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.cabal-fmt.enable = true;
}
