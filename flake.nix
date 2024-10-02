{
  inputs = rec {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    replica.url = "git+file:///home/nixos/fix/kamoii-replica";
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
           };

          haskellProjects.default = {
            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
            basePackages = config.haskellProjects.ghc965.outputs.finalPackages;

            packages = {
              # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
              # shower.source = inputs.shower; # Override shower to a custom source path
              replica.source = inputs.replica;
            };
            settings = {
              list-zipper.jailbreak = true;
              #  aeson = {
              #    check = false;
              #  };
              #  relude = {
              #    haddock = false;
              #    broken = false;
              #  };
            };

            devShell = {
              # Enabled by default
              # enable = true;

              # Programs you want to make available in the shell.
              # Default programs can be disabled by setting to 'null'
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

              # Check that haskell-language-server works
              # hlsCheck.enable = true; # Requires sandbox to be disabled
            };
          };

          # haskell-flake doesn't set the default package, but you can do it here.
          # packages.default = config.haskellProjects.ghc948.outputs.finalPackages.lattices;
          packages.default = self'.packages.concur-control; 
        };
    };
}
