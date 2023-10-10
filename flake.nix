{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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

  outputs = inputs@{ nixpkgs, flake-parts, nix2container, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } (
      { flake-parts-lib, withSystem, ... }: {
        imports = [
          inputs.devenv.flakeModule
          inputs.treefmt-nix.flakeModule
          (flake-parts-lib.importApply
            ./container.nix
            { inherit nixpkgs; })
        ];
        systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

        perSystem = { config, self', inputs', pkgs, system, lib, ... }:
          let
            ghcPackage = pkgs.ghc; # Currently pkgs.haskell.compiler.ghc96 is broken with ormolu
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

            # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
            # packages.default = pkgs.xhaskell;

            treefmt.config = import ./treefmt.nix { inherit pkgs config; };

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
                package = ghcPackage;
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

            apps.default.program = pkgs.xhaskell;

            # Container
            # packages = withSystem "aarch64-linux" ({ config, inputs', system, pkgs, ... }: {
            #   # _module.args.pkgs = import nixpkgs {
            #   #   localSystem = args.system;
            #   #   crossSystem = system;
            #   #   overlays = [
            #   #     overlay
            #   #   ];
            #   # };

            #   "container-xhaskell-${system}" = inputs'.nix2container.packages.nix2container.buildImage {
            #     name = "xhaskell";
            #     config.entrypoint = [ "${pkgs.xhaskell}/bin/xhaskell" ];
            #   };
            # });
          };

        flake = { };
        # withSystem "x86_64-linux" ({ config, inputs', system, pkgs }: {
        #   packages.container-xhaskell = pkgs.hello;
        # });
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
        # packages.container-xhaskell = withSystem "x86_64-linux" ({ config, inputs' }: { });

      }
    );
}
