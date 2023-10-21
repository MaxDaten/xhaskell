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

        tailwindcssWithPlugins = pkgs.nodePackages.tailwindcss.overrideAttrs (oldAttrs: {
          plugins = lib.forEach cfg.plugins (plugin: pkgs.nodePackages.${plugin});
        });

        tailwind-config-js =
          let
            plugins = lib.concatStringsSep ",\n" (lib.forEach cfg.plugins (plugin: "require('${plugin}')"));
            content = lib.concatStringsSep ", " (lib.forEach cfg.contentPattern (content: "'${content}'"));
          in
          pkgs.writeText "tailwind.config.js" ''
            /** @type {import('tailwindcss').Config} */
            module.exports = {
              content: [
                ${content}
              ],
              theme: {
                extend: {},
              },
              plugins: [
              ],
            }
          '';

        tailwindWrapper = pkgs.writeShellScriptBin "tailwindcss" ''
          set -ex
          ${tailwindcssWithPlugins}/bin/tailwindcss \
            --config "${tailwind-config-js}" \
            --input "${cfg.inputCss}" \
            "$@"
        '';
      in
      {

        options.tailwindcss = {

          plugins = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "List of tailwindcss plugins to install";
          };

          # TODO: Check path
          inputCss = lib.mkOption {
            type = lib.types.path;
            default = null;
            description = ''
              The input css file to process with tailwindcss.
              This file will be processed with postcss.
            '';
          };

          contentPattern = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "app/**/*.hs" ];
            description = ''
              The pattern to match the content files.
            '';
          };

          outputCssFileName = lib.mkOption {
            type = lib.types.str;
            default = "style.css";
            description = ''
              The output css file to process with tailwindcss.
              This file will be processed with postcss.
            '';
          };

          # Outputs
          build = {
            cli = lib.mkOption {
              type = lib.types.package;
              default = tailwindWrapper;
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

  config = {
    perSystem =
      { config
      , pkgs
      , ...
      }:
      let
        cfg = config.tailwindcss;
        output-css = pkgs.runCommand cfg.outputCssFileName { } ''
          ${lib.getExe cfg.build.cli} -o $out
        '';
      in
      {
        packages.tailwindcss = cfg.build.cli;
        packages.tailwindcss-output-css = output-css;
      };
  };
}
