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
        pkgs = import nixpkgs {inherit system;};
        workaround140774 = haskellPackage:
          with pkgs.haskell.lib;
            overrideCabal haskellPackage (drv: {
              enableSeparateBinOutput = false;
            });
      in rec {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            coreutils
            moreutils
            jq
            go-migrate
            awscli2
            sqlite-interactive
            cabal2nix
            litestream
            flyctl
            nodePackages.typescript-language-server
            nodePackages.prettier
            (workaround140774 haskellPackages.ormolu)
            (workaround140774 haskellPackages.ghcid)
            (haskell.packages.ghc810.ghcWithPackages (hpkgs:
              with hpkgs; [
                cabal-install
                cabal-fmt
                hlint
                fast-tags
              ]))
            terraform # Remove this and properly document the AWS stuff then also remove all DO stuff when I'm using fly
          ];
        };
      }
    );
}
