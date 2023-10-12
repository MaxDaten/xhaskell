{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.05";
    devenv.url = "github:cachix/devenv";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
    allow-broken = true;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix2container, nixpkgs-stable, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } (
      { flake-parts-lib, withSystem, ... }: {
        imports = [
          inputs.devenv.flakeModule
          inputs.treefmt-nix.flakeModule
          (flake-parts-lib.importApply
            ./container.nix
            { nixpkgs = nixpkgs-stable; })
        ];
        systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

        perSystem = { config, self', inputs', pkgs, system, lib, ... }:
          let
            ghc = pkgs.haskell.compiler.ghc946;
            packageOverlay = import ./package.nix;
          in
          {
            _module.args.pkgs = import nixpkgs {
              inherit system;
              overlays = [
                packageOverlay
              ];
            };
            # Per-system attributes can be defined here. The self' and inputs'
            # module parameters provide easy access to attributes of the same
            # system.

            treefmt.config = import ./treefmt.nix { inherit pkgs config; };

            gcloud-run-deploy-container = {
              registry = "europe-west3-docker.pkg.dev/ai-playground-c437/docker";

              containers = {
                xhaskell = {
                  image = system: {
                    config = {
                      entrypoint = [ "${lib.getExe self.packages.${system}.xhaskell}" ];
                      Env = [
                        "PORT=80"
                      ];
                    };
                    maxLayers = 100;
                  };
                };
              };
            };

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
                config.treefmt.build.wrapper
              ] ++ lib.attrValues config.treefmt.build.programs;

              scripts.dev.exec = "ghcid";
              scripts.prod.exec = "${lib.getExe pkgs.xhaskell}";
            };

            packages.default = pkgs.xhaskell;
            packages.xhaskell = pkgs.xhaskell;
            apps.default.program = pkgs.xhaskell;
          };

        flake = { };

      }
    );
}
