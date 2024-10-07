{
  inputs = rec {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    kamoii-replica.url = "git+file:///home/nixos/fix/kamoii-replica";
    kamoii-replica.flake = false;
    replica.url = "git+file:///home/nixos/fix/replica";
    replica.flake = false;
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
      imports = [ inputs.haskell-flake.flakeModule ];

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
            defaults.packages = {};
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
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;

            packages = {
              replica.source = inputs.kamoii-replica;
            };

          };
          packages.default = self'.packages.concur-control; 
        };
    };
}
