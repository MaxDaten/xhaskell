{ lib, flake-parts-lib, ... }: {
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

        containerDefPerSystem = system: definitionAttrs: lib.mapAttrs'
          (containerName: config:
            lib.nameValuePair
              "container-${containerName}-${system}"
              (buildImage containerName (config.image system))
          )
          definitionAttrs;

        buildImage = name: args:
          nix2container.buildImage (
            args // {
              name =
                if cfg.registry == null
                then name
                else cfg.registry + "/" + name;
            }
          );


        containerType = lib.types.submodule {
          options = {
            image = lib.mkOption {
              type = lib.types.functionTo lib.types.attrs;
              default = system: { };
            };
          };
        };
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

          containers = lib.mkOption {
            type = lib.types.lazyAttrsOf containerType;
            default = { };
            example = lib.literalExpression ''
              {
                service-a = {
                  image = system: {
                    config = {
                      entrypoint = [ "''${lib.getExe self.packages.''${system}.default}" ];
                      Env = [
                        "PORT=80"
                      ];
                    };
                    maxLayers = 100;
                  };
                };
              };
            '';
          };
        };

        # Module Implementation
        config.packages = forAllContainerPlatforms (system: containerDefPerSystem system cfg.containers);

        # config.devenv.shells.default = {
        #   scripts.deploy.exec = "echo 'Deploying...' ";
        # };
      }
    );
  };
}
