{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    devenv.url = "github:cachix/devenv";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
    allow-broken = true;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix2container, flake-root, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } (
      { flake-parts-lib, withSystem, ... }: {

        imports = [
          inputs.flake-root.flakeModule
          inputs.devenv.flakeModule
          inputs.treefmt-nix.flakeModule
          ./nix/modules/container.nix
          ./nix/modules/tailwindcss.nix
        ];

        systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

        perSystem = { config, self', inputs', pkgs, system, lib, ... }:
          let
            ghc = pkgs.haskell.compiler.ghc946;
            google-cloud-project = "ai-playground-c437";
          in
          {
            flake-root.projectRootFile = "flake.nix";
            _module.args.pkgs = import nixpkgs {
              inherit system;

              config = {
                allowUnfree = true;
              };

              overlays = [
                self.overlays.default
              ];
            };
            # Per-system attributes can be defined here. The self' and inputs'
            # module parameters provide easy access to attributes of the same
            # system.

            # Formatting of all source files
            treefmt.config = import ./treefmt.nix { inherit pkgs config; };

            tailwindcss = {
              plugins = [
                "@tailwindcss/forms"
                "@tailwindcss/aspect-ratio"
                "@tailwindcss/language-server"
                "@tailwindcss/line-clamp"
                "@tailwindcss/typography"
              ];
              inputCss = ./app/static/style.css;
            };

            # Deploy to Google Cloud Run
            gcloud-run-deploy-container = {

              project_id = google-cloud-project;
              location = "europe-west3";
              repository-name = "docker";

              pkgs = crossSystem: import nixpkgs {
                inherit crossSystem;
                localSystem = system;

                overlays = [
                  self.overlays.default
                ];
              };

              containers = {

                xhaskell = {
                  image = pkgs: {
                    config = {
                      entrypoint = [ "${lib.getExe pkgs.xhaskell}" ];
                      Env = [
                        "PORT=80"
                      ];
                    };
                    maxLayers = 100;
                  };
                };

              };

            };


            # Development Shell
            devenv.shells.default = {
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

              scripts.dev.exec = "ghcid";
              scripts.prod.exec = "${lib.getExe pkgs.xhaskell}";

              enterShell = ''
                gcloud config set project ${google-cloud-project}
              '';
            };

            packages.default = pkgs.xhaskell;
            packages.xhaskell = pkgs.xhaskell;
            apps.default.program = pkgs.xhaskell;
          };

        flake = {
          overlays.default = import ./package.nix;
        };

      }
    );
}
