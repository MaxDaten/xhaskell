{ self, lib, ... }:
{
  perSystem =
    { config
    , self'
    , inputs'
    , ...
    }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      compiler-nix-name = "ghc928";
      ghc = pkgs.haskell-nix.compiler.${compiler-nix-name};

      xhaskell = pkgs.haskell-nix.cabalProject {
        inherit (config.haskell-nix) evalSystem;
        inherit compiler-nix-name;
        name = "xhaskell";
        src = ./.;
        # index-state = "2021-01-04T00:00:00Z";
        # plan-sha256 = "04hdgqwpaswmyb0ili7fwi6czzihd6x0jlvivw52d1i7wv4gaqy7";
        # materialized = ./xhaskell.materialized;
      };

      haskellNixFlake = xhaskell.flake { };

      google-cloud-project = "ai-playground-c437";
    in
    {
      gcloud-run-deploy-container = {
        project_id = google-cloud-project;
        location = "europe-west3";
        repository-name = "docker";

        containers = {

          xhaskell = {
            image = pkgs: {
              copyToRoot = [
                config.packages.xhaskell-static-files
              ];
              config = {
                # TODO: Use platform specific xhaskell
                # entrypoint = [ "${lib.getExe pkgs.xhaskell}" ];
                Env = [
                  "PORT=80"
                  "STATIC_DIR=/var/www"
                ];
              };
              maxLayers = 100;
            };
          };
        };

      };


      tailwindcss = {
        src = ./app;
        inputCss = ./app/static/style.css;
        # Pattern relative to src
        content = [
          "./**/*.hs"
          "./templates/**/*"
        ];
        plugins = [
          "@tailwindcss/forms"
          "@tailwindcss/aspect-ratio"
          "@tailwindcss/language-server"
          "@tailwindcss/line-clamp"
          "@tailwindcss/typography"
        ];
      };


      # Dev Shell for XHaskell!
      devenv.shells.xhaskell = {
        name = "xhaskell";

        imports = [
          # This is just like the imports in devenv.nix.
          # See https://devenv.sh/guides/using-with-flake-parts/#import-a-devenv-module
          # ./devenv-foo.nix
        ];

        languages.nix.enable = true;
        languages.haskell = {
          enable = true;
          package = ghc;
          stack = null;
        };

        # https://devenv.sh/reference/options/
        packages = [
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.ghcid

          pkgs.skopeo
          pkgs.google-cloud-sdk
          pkgs.terraform
          config.tailwindcss.build.cli
          config.treefmt.build.wrapper
        ] ++ lib.attrValues config.treefmt.build.programs;

        enterShell = ''
          gcloud config set project ${google-cloud-project}
        '';

        env.STATIC_DIR = "${config.devenv.shells.xhaskell.env.DEVENV_STATE}/xhaskell/static";

        # Development setup
        process = {
          implementation = "process-compose";
          before = ''
            echo "Creating $STATIC_DIR"
            mkdir -p $STATIC_DIR
          '';
        };

        processes = {

          watch-statics.exec = ''
            cp -r "app/static/" "$STATIC_DIR"
            ${pkgs.fswatch}/bin/fswatch -0 "app/static/" | while read -d "" event; \
            do
              echo "''${event}"
              cp -r "''${event}" "$STATIC_DIR"
            done
          '';

          watch-style = {
            exec = ''
              set -euo pipefail
              set -x
              ${lib.getExe config.tailwindcss.build.cli} \
                --watch=always \
                --input "app/static/style.css" \
                --output "$STATIC_DIR/style.css"
            '';
          };

          dev = {
            exec = "ghcid";
            process-compose = {
              depends_on.watch-style.condition = "process_started";
              depends_on.watch-statics.condition = "process_started";
              readiness_probe = {
                exec.command = "curl -s http://localhost:8080/";
                initial_delay_seconds = 2;
                period_seconds = 30;
              };
              shutdown.signal = 2; # SIGINT (Ctrl-C) to ghcid
            };
          };
        };
      };


      # General flake output
      apps = haskellNixFlake.apps;
      packages = haskellNixFlake.packages;

    };
  flake = { };
}
