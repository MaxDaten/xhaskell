{ pkgs, config, ... }:
{
  # Used to find the project root
  inherit (config.flake-root) projectRootFile;
  programs.hlint.enable = true;
  programs.ormolu.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.cabal-fmt.enable = true;
  programs.terraform.enable = true;
}
