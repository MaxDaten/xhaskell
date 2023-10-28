{ lib, flake-parts-lib, self, ... }:
let
  inherit (lib)
    mkOption
    mkEnableOption
    nameValuePair
    mkIf
    types
    ;
  inherit (flake-parts-lib)
    mkPerSystemOption
    ;

  imageBySystemType = types.submodule {
    options = {
      image = mkOption {
        type = types.functionTo types.package;
        default = system: { };
      };
    };
  };
in
{
  options.perSystem = mkPerSystemOption (
    { config
    , pkgs
    , ...
    }: {
      options.multiplatform-container = {
        platforms = lib.mkOption {
          type = types.listOf types.str;
          default = [ "x86_64-linux" "aarch64-linux" ];
          description = "The platforms create images for";
        };
        containers = mkOption {
          type = types.anything;
          default = { };
          description = ''
            Attributes of containers to build.
          '';
        };
      };
    }
  );

  # Implementation
  config = {
    perSystem = { config, self', inputs', system, pkgs, ... }:
      let
        cfg = config.multiplatform-container;

        mergeAttrsList = lib.foldl lib.mergeAttrs { };
        forAllContainerPlatforms = f: mergeAttrsList (lib.forEach cfg.platforms f);

        images =
          forAllContainerPlatforms
            (targetSystem:
              lib.mapAttrs' (name: imageBySystem: nameValuePair ("container-${name}-${targetSystem}") (imageBySystem targetSystem))
                cfg.containers
            );
      in
      {
        config.packages = images;
      };
  };
}
