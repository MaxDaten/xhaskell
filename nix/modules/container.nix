{ lib, flake-parts-lib, inputs, ... }: {
  options = {
    perSystem = flake-parts-lib.mkPerSystemOption (
      perSystem@{ config
      , self'
      , inputs'
      , pkgs
      , system
      , ...
      }:
      let
        cfg = config.gcloud-run-deploy-container;

        nix2container = inputs'.nix2container.packages.nix2container;

        mergeAttrsList = lib.foldl lib.mergeAttrs { };

        forAllContainerPlatforms = f: mergeAttrsList (lib.forEach cfg.platforms f);

        containerDefPerSystem = system: definitionAttrs:
          lib.mapAttrs'
            (containerName: config:
              lib.nameValuePair
                "container-${containerName}-${system}"
                (buildImage containerName (config.image (cfg.pkgs system)))
            )
            definitionAttrs;

        artifact-registry = "${cfg.location}-docker.pkg.dev/${cfg.project_id}/${cfg.repository-name}";

        buildImage = name: args:
          let
            image = nix2container.buildImage
              (
                args // {
                  name = artifact-registry + "/" + name;
                }
              );
          in
          image // {
            deployGceCloudRun = deployGceCloudRun image;
          };

        # Deploy to GCE Cloud Run via Terraform configuration
        # in ./deployment
        deployGceCloudRun = image:
          let
            deploymentConfigDir = "deployment";

            vars = {
              inherit artifact-registry;
              project_id = cfg.project_id;
              location = cfg.location;
              image-name = builtins.baseNameOf image.imageName;
              image-tag = image.imageTag;
            };

            var-file = pkgs.writeText "terraform.tfvars.json" (builtins.toJSON vars);
          in
          pkgs.writeShellScriptBin "deploy-gce-cloud-run" ''
            set -x
            set -e
            cat ${image}
            echo "$GOOGLE_CREDENTIALS" | ${lib.getExe pkgs.skopeo} login -u _json_key --password-stdin "https://${cfg.location}-docker.pkg.dev"
            ${lib.getExe image.copyToRegistry}
            
            flake_root=$(${lib.getExe config.flake-root.package})
            export TF_CLI_ARGS="-var-file=\"${var-file}\""
            
            echo "Deploying ${vars.image-name}:${vars.image-tag} to GCE Cloud Run..."
            export TF_LOG=DEBUG
            ${lib.getExe pkgs.terraform} -chdir="''${flake_root}/${deploymentConfigDir}" init
            ${lib.getExe pkgs.terraform} -chdir="''${flake_root}/${deploymentConfigDir}" "$@"
          '';

        # Module Type Definition

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
          project_id = lib.mkOption {
            type = lib.types.str;
            description = "The GCP project to deploy to";
          };

          location = lib.mkOption {
            type = lib.types.str;
            description = "The GCP location to deploy to";
          };

          repository-name = lib.mkOption {
            type = lib.types.str;
            description = ''
              The GCP Artifact Registry docker/container repository to deploy to. 
              Without prefix like `europe-west3-docker.pkg.dev/my-project/`.
            '';
          };

          platforms = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "x86_64-linux" "aarch64-linux" ];
            description = "The platforms to deploy to";
          };

          pkgs = lib.mkOption {
            type = lib.types.functionTo lib.types.attrs;
            default = crossSystem: import inputs.nixpkgs {
              inherit crossSystem;
              localSystem = system;
            };
            description = "The nixpkgs to use for building the container";
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
      }
    );
  };
}
