{ nixpkgs }:
{ config, lib, flake-parts-lib, withSystem, inputs', ... }: {
  options.perSystem = flake-parts-lib.mkPerSystemOption (perSystem@{ config, self', inputs', ... }: {

    config.packages = withSystem "aarch64-linux" ({ config, system, ... }:
      let
        packageOverlay = import ./package.nix;

        pkgs = import nixpkgs {
          localSystem = perSystem.system;
          crossSystem = system;
          overlays = [
            packageOverlay
          ];
        };
        nix2container = inputs'.nix2container.packages.nix2container;
      in
      {
        "container-xhaskell-${system}" = nix2container.buildImage {
          name = "xhaskell";
          config.entrypoint = [ "${pkgs.xhaskell}/bin/xhaskell" ];
        };
      });
  });
}
