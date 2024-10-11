{
  inputs = rec {
    common.follows = "kamoii-replica/common";
    nixpkgs.follows = "common/nixpkgs";
    kamoii-replica.url = "github:YuMingLiao/kamoii-replica";
    list-zipper.url = "github:system-f/list-zipper";
    list-zipper.flake = false;
  };
  outputs =
    inputs@{
      self,
      common,
      ...
    }:
    common.lib.mkFlake { inherit inputs; } {

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {
          haskellProjects.default = {
            basePackages = config.haskellProjects.ghc9101.outputs.finalPackages;
            imports = [inputs.kamoii-replica.haskellFlakeProjectModules.output];
            projectRoot = builtins.toString (pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.difference ./. ./flake.nix; 
            });
            settings = {
              list-zipper.jailbreak = true;
            };
            packages = {
              list-zipper.source = inputs.list-zipper;
              replica.source = inputs.kamoii-replica;
            };
            devShell = {

              mkShellArgs = {
                packages = hp: with hp; [
                  tasty
                ];
              };
            };
          };

          packages.default = self'.packages.concur-control;
          checks.default = pkgs.stdenv.mkDerivation {
            name = "test orr";
            src = ./test;
            buildInputs = [(config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (p: with p; [tasty tasty-hunit concur-control]))];
            buildPhase =''
              runghc testOrBlock 
            '';

          }; 
        };
    };
}
