{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    devenv.url = "github:cachix/devenv";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    #  Should be pinned via haskellNix for caching of ghc compiler
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  };

  nixConfig = {
    extra-trusted-public-keys = ''
      devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    '';
    extra-substituters = ''
      https://devenv.cachix.org
      https://cache.iog.io
    '';
    allow-broken = true;
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix2container, flake-root, ... }:

    (flake-parts.lib.evalFlakeModule 
      { inherit self inputs; }
      {
        systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

        imports = [
          inputs.flake-root.flakeModule
          inputs.devenv.flakeModule
          inputs.treefmt-nix.flakeModule
          ./nix/flake-modules
          ./xhaskell.nix
        ];

        perSystem = { config, self', inputs', pkgs, system, lib, ... }:
        {
            _module.args.pkgs = import nixpkgs { config.allowUnfree = true; inherit system; };

            flake-root.projectRootFile = "flake.nix";
            treefmt.config = import ./treefmt.nix { inherit pkgs config; };

            devShells.default = self'.devShells.xhaskell;
        };
      }).config.flake;
}

