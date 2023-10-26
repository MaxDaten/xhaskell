{ self, ... }:
{
  perSystem = {
      config
    , self'
    , inputs'
    , ... 
    }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;

      xhaskell = pkgs.haskell-nix.cabalProject {
        inherit (config.haskell-nix) evalSystem;
        name = "xhaskell";
        src = ./.;
        compiler-nix-name = "ghc928";
      };

      haskellNixFlake = xhaskell.flake { };
    in
    {
      apps = haskellNixFlake.apps;
      packages = haskellNixFlake.packages;
    };
  flake = { };
}