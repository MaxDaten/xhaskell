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
        name = "xhaskell";
        src = ./.;
        compiler-nix-name = "ghc928";
        evalSystem = "aarch64-darwin";
      };

      haskellNixFlake = xhaskell.flake { };
    in
    {
      apps = haskellNixFlake.apps;
      packages = haskellNixFlake.packages;
    };
  flake = { };
}