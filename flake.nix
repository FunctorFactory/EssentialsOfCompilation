{
  description = "EssentialsOfCompilation: Working Siek's book in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];

      perSystem = { self', lib, pkgs, config, ... }: {

        haskellProjects.default = {
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./app
              ./lib
              ./tests
              ./LICENSE
              ./compiler.cabal
              ./CHANGELOG.md
            ];
          });
          settings = {
            check = true;
            haddock = true;
            jailbreak = false;
            broken = false;
          };

          devShell = {
            enable = true;
            hlsCheck.enable = pkgs.stdenv.isDarwin;
          };

          autoWire = [ "packages" "apps" "checks" ];
        };

        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          record-brace-space = true;
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };

        packages.default = self'.packages.compiler;
        apps.default = self'.packages.compiler;

        devShells.default = pkgs.mkShell {
          name = "EssentialsOfCompilation";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
