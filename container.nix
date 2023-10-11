{ nixpkgs }:
{ config, lib, flake-parts-lib, withSystem, inputs', ... }: {
  options.perSystem = flake-parts-lib.mkPerSystemOption (perSystem@{ config, self', inputs', ... }: {

    config.packages =
      let
        containerPlatforms = [ "x86_64-linux" "aarch64-linux" ];
        mergeAttrsList = lib.foldl lib.mergeAttrs { };
        forAllContainerPlatforms = f: mergeAttrsList (lib.forEach containerPlatforms f);
        registry = "europe-west3-docker.pkg.dev/ai-playground-c437/docker";
      in
      forAllContainerPlatforms (ws: withSystem ws ({ config, system, ... }:
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
            name = "${registry}/xhaskell";
            config = {
              entrypoint = [ "${lib.getExe pkgs.xhaskell}" ];
              Env = [
                "XHASKELL_PORT=80"
              ];
            };
            maxLayers = 100;
          };
        })
      );
  });
}
