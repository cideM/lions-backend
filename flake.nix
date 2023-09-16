{
  description = "Nix Flake template using the 'nixpkgs-unstable' branch and 'flake-utils'";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
        };
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            coreutils
            moreutils
            jq
            go-migrate
            awscli2
            sqlite-interactive
            litestream
            flyctl
            alejandra
            terraform

            nodePackages.typescript-language-server
            nodePackages.prettier

            cabal2nix
            haskellPackages.ormolu
            haskellPackages.ghcid
            haskellPackages.cabal-install
            haskellPackages.cabal-fmt
            haskellPackages.hlint
            haskellPackages.fast-tags
            # GHC 92+ should have the new record syntax
            haskell.compiler.ghc810

            # Failed to build zlib-0.6.3.0. The failure occurred during the configure step.
            # Build log ( /Users/fbs/.cache/cabal/logs/ghc-8.10.7/zlb-0.6.3.0-47c3bb32.log
            # ):
            # Configuring library for zlib-0.6.3.0..
            # Error: .cabal-wrapped: Missing dependency on a foreign library:
            # * Missing (or bad) C library: z
            zlib
          ];
        };
      }
    );
}
