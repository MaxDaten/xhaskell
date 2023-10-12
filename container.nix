{ nixpkgs }:
{ config, lib, flake-parts-lib, withSystem, ... }: {
  options = {
    perSystem = flake-parts-lib.mkPerSystemOption (
      perSystem@{ config
      , self'
      , inputs'
      , ...
      }:
      let
        cfg = config.gcloud-run-deploy-container;
        nix2container = inputs'.nix2container.packages.nix2container;
        mergeAttrsList = lib.foldl lib.mergeAttrs { };
        forAllContainerPlatforms = f: mergeAttrsList (lib.forEach cfg.platforms f);

        packageOverlay = import ./package.nix;
      in
      {

        # Module Configuration
        options.gcloud-run-deploy-container = {
          registry = lib.mkOption {
            type = lib.types.str;
            default = "ghcr.io";
            description = "The container registry to deploy to";
          };

          platforms = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "x86_64-linux" "aarch64-linux" ];
            description = "The platforms to deploy to";
          };
        };

        # Module Implementation
        config.packages = forAllContainerPlatforms (system:
          let
            pkgs = import nixpkgs {
              localSystem = perSystem.system;
              crossSystem = system;
              overlays = [
                packageOverlay
              ];
            };
          in
          {
            "container-xhaskell-${system}" = nix2container.buildImage {
              name = "${cfg.registry}/xhaskell";
              config = {
                entrypoint = [ "${lib.getExe pkgs.xhaskell}" ];
                Env = [
                  "PORT=80"
                ];
              };
              maxLayers = 100;
            };
          }
        );

        config.devenv.shells.default = {
          scripts.deploy.exec = "echo 'Deploying...' ";
        };
      }
    );
  };
}
