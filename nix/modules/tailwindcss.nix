{ lib, flake-parts-lib, inputs, ... }: {
  options = {
    perSystem = flake-parts-lib.mkPerSystemOption (
      { config
      , self'
      , inputs'
      , pkgs
      , system
      , ...
      }:
      let
        cfg = config.tailwindcss;

        tailwindPkgsWithPlugins = pkgs.nodePackages.tailwindcss.overrideAttrs (oldAttrs: {
          plugins = lib.forEach cfg.plugins (plugin: pkgs.nodePackages.${plugin});
        });
      in
      {

        options.tailwindcss = {

          plugins = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "List of tailwindcss plugins to install";
          };

          # Outputs
          build = {
            cli = lib.mkOption {
              type = lib.types.package;
              default = tailwindPkgsWithPlugins;
              description = ''
                The tailwindcss package with plugins, as npm package. 
                For example: "@tailwindcss/forms"
              '';
            };
          };
        };
      }
    );
  };

  # config = {
  #   perSystem =
  #     { config
  #     , pkgs
  #     , ...
  #     }:
  #     let
  #       cfg = config.tailwindcss;

  #       tailwindPkgsWithPlugins = pkgs.tailwindcss.overrideAttrs (oldAttrs: {
  #         plugins = lib.forEach cfg.plugins (plugin: pkgs.nodePackages.plugin);
  #       });
  #     in
  #     {
  #       packages.tailwindcss = tailwindPkgsWithPlugins;
  #     };
  # };
}
