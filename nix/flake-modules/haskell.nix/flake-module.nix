# https://github.com/ArdanaLabs/Ardana-dApps/blob/master/nix/flake-modules/haskell.nix/flake-module.nix
{ lib, self, ... }:
let
  inherit (lib)
    mkOption
    types
    ;
in
{
  perSystem = { config, self', inputs', system, ... }: 
    {
      options.haskell-nix = {
        pkgs = mkOption {
          type = types.uniq (types.attrsOf types.unspecified);
          description = ''
            Nixpkgs to use for haskell-nix invocations. This is more of a
            fixed-point that can be referred to in order to ensure we import
            haskell-nix only once, for performance, since we haven't implemented
            the rest of the module.
          '';
          default = inputs'.haskell-nix.legacyPackages;
        };

        evalSystem = mkOption {
          type = types.str;
          description = ''
            The system to evaluate haskell-nix expressions for. Required
            for `nix flake show/check` to work propably.
          '';
          default = "aarch64-darwin";
        };
      };
  };
}
