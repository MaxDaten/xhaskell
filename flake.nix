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

  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    let
      overlay = final: prev: {
        haskell =
          prev.haskell
          // {
            packageOverrides = hself: hsuper:
              prev.haskell.packageOverrides hself hsuper
              // {
                xhaskell = hself.callCabal2nix "xhaskell" ./. { };
                servant-lucid2 = hself.callCabal2nix "servant-lucid2"
                  (builtins.fetchGit {
                    url = "https://github.com/Briends/servant-lucid2.git";
                    rev = "b79e3d7b39f07d3d2ab33cf6c229bc6e4e20818d";
                    # sha256 = "0xq5jz3q8z6jg1j8z6zv7q5q5z1v6jz7j3f6q5z1v6jz7j3f6q5z";
                  })
                  { };
                lucid2-hyperscript = hself.callCabal2nix "lucid2-hyperscript"
                  (builtins.fetchGit {
                    url = "https://github.com/Briends/lucid2-hyperscript.git";
                    rev = "8d37673c3b10c4163db24626c94ed24dff5f0905";
                    # sha256 = "0xq5jz3q8z6jg1j8z6zv7q5q5z1v6jz7j3f6q5z1v6jz7j3f6q5z";
                  })
                  { };
              };
          };
        xhaskell = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.xhaskell;
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devenv.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { config, self', inputs', pkgs, system, lib, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            overlay
          ];
        };
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
        packages.default = pkgs.xhaskell;

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
            stack = null;
          };

          # https://devenv.sh/reference/options/
          packages = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ghcid

            pkgs.nixpkgs-fmt
            config.treefmt.build.wrapper
          ] ++ lib.attrValues config.treefmt.build.programs;

          scripts.dev.exec = "ghcid";
        };

        apps.default.program = pkgs.xhaskell;

      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

      };
    };
}
