{
  inputs = rec {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    kamoii-replica.url = "git+file:///home/nixos/fix/kamoii-replica";
    kamoii-replica.flake = false;
    replica.url = "git+file:///home/nixos/fix/replica";
    replica.flake = false;
    check-flake.url = "github:srid/check-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      haskell-flake,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ 
        inputs.haskell-flake.flakeModule 
        inputs.check-flake.flakeModule
      ];

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {
          haskellProjects.ghc965 = {
            basePackages = pkgs.haskell.packages.ghc965;
            defaults.packages = { };
            settings = {
              list-zipper.jailbreak = true;
            };
          };

          haskellProjects.no-session-synchron = {
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;
            packages = {
              replica.source = inputs.replica;
            };

          };
          haskellProjects.default = {
            projectRoot = builtins.toString (pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.difference ./. ./flake.nix; 
            });
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;

            packages = {
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
