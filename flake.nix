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

        {

          haskellProjects.default = {
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;
            imports = [ inputs.kamoii-replica.haskellFlakeProjectModules.output ];
            projectRoot = builtins.toString (
              pkgs.lib.fileset.toSource {
                root = ./.;
                fileset = pkgs.lib.fileset.difference ./. ./flake.nix;
              }
            );
            settings = {
              list-zipper.jailbreak = true;
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

              hoogle = false;
              #hlsCheck.enable = false;
            };
            otherOverlays = [
            #(final: prev: { ghc = pkgs.haskell.lib.dontHaddock prev.ghc;}) 
            ];
          };
          packages.default = self'.packages.concur-control;
          devShells.final = pkgs.mkShell {
            name = "A shell that has the final concur-control";
            inputsFrom = [ config.haskellProjects.default.outputs.devShell ];
            nativeBuildInputs = [
              (config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (
                p: with p; [
                  concur-control
                ]
              ))
            ];
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
