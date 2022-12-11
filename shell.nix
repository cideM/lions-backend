{ pkgs, spago2nix, projectEnv }:
let
  # Generate tags based on the exposed Haskell modules. "fast-tags" doesn't
  # respect .gitignore and as such just calling fast-tags will result in lots
  # of false positives.
  lions-tags = pkgs.writeShellScriptBin "lions-tags" ''
    ${pkgs.haskellPackages.fast-tags}/bin/fast-tags --cabal ./backend/lions-backend.cabal
  '';

  spago2nix' = import spago2nix { inherit pkgs; };

  clientShell = pkgs.npmlock2nix.shell {
    nodejs = pkgs.nodejs-14_x;
    src = ./client/.;
  };

in
pkgs.mkShell {
  inputsFrom = [ projectEnv ];
  nativeBuildInputs = [
    spago2nix'
  ] ++ clientShell.buildInputs;
  propagatedBuildInputs = clientShell.propagatedBuildInputs;
  propagatedNativeBuildInputs = clientShell.propagatedNativeBuildInputs;
  buildInputs = with pkgs.haskellPackages;
    [
      # Haskell
      ghcid
      ormolu
      hlint
      cabal2nix
      cabal-install
      cabal-fmt
      fast-tags
      hoogle

      pkgs.nixpkgs-fmt

      # Database
      pkgs.go-migrate
      pkgs.sqlite-interactive
      pkgs.sqlite-web
      pkgs.litestream

      # Purescript
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.purty

      pkgs.nodePackages.npm-check-updates

      # Scripts
      lions-tags

      # Infra
      pkgs.terraform
      pkgs.nodejs
      # pkgs.cli53
      pkgs.nodePackages.sass
      pkgs.nodePackages.postcss-cli
      pkgs.awscli2
    ] ++ clientShell.buildInputs;
}
