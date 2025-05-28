{
  inputs = rec {
    common.url = "github:YuMingLiao/common";
    nixpkgs.follows = "common/nixpkgs";
    #kamoii-replica.url = "github:YuMingLiao/kamoii-replica";
    kamoii-replica.url = "git+file:///home/nixos/fix/kamoii-replica";
    kamoii-replica.inputs.common.follows = "common";
    list-zipper.url = "github:system-f/list-zipper";
    list-zipper.flake = false;
  };
  outputs =
    inputs@{ self, common, ... }:
    common.lib.mkFlake { inherit inputs; } {
      perSystem =
        {
          self',
          pkgs,
          config,
          system,
          ...
        }:

        let
          pkgs' = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.self.overlays.default ];
          };
        in
        {

          haskellProjects.default = {
            basePackages = pkgs'.haskell.packages.ghc965; # config.haskellProjects.ghc965.outputs.finalPackages;
            imports = [ inputs.kamoii-replica.haskellFlakeProjectModules.output ];
            projectRoot = builtins.toString (
              pkgs.lib.fileset.toSource {
                root = ./.;
                fileset = pkgs.lib.fileset.difference ./. ./flake.nix;
              }
            );
            settings = {
              list-zipper.jailbreak = true;
              ghc.haddock = false;
            };
            packages = {
              list-zipper.source = inputs.list-zipper;
              replica.source = inputs.kamoii-replica;
            };
            devShell = {
              tools = hp: {
                haskell-language-server = null;
                hlint = null;
              };
              #hlsCheck.enable = false;
            };
          };
          packages.default = self'.packages.concur-control;
          #packages.default = self'.packages;
          devShells.final = pkgs.mkShell {
            name = "my-haskell-package custom development shell";
            inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
          };
          checks.default = pkgs.stdenv.mkDerivation {
            name = "test orr";
            src = ./test;
            buildInputs = [
              (config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (
                p: with p; [
                  tasty
                  tasty-hunit
                  concur-control
                ]
              ))
            ];
            buildPhase = ''
              runghc testOrBlock 
            '';

          };
        };
    };
}
