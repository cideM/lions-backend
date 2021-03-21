let
  sources = import ./nix/sources.nix;

  migrateOverlay = self: super: {
    go-migrate = super.go-migrate.overrideAttrs (old: {
      buildFlags = "-tags 'sqlite3'";
    });
  };

  pkgs = import sources.nixpkgs { overlays = [ migrateOverlay ]; };

  projectEnv = (import ./release.nix).project.env;

  lions-dev = pkgs.writeScriptBin "lions-dev" ''
    #!/bin/sh
    rm -f public
    nix-build -A assets release.nix -o public/
    cabal v2-run
  '';

in
pkgs.mkShell {
  inputsFrom = [ projectEnv ];
  buildInputs = with pkgs.haskellPackages; [
    ghcid
    ormolu
    hlint
    cabal2nix
    haskell-language-server
    cabal-install
    cabal-fmt
    fast-tags
    pkgs.go-migrate
    pkgs.sqlite-interactive
    pkgs.sqlite-web

    lions-dev
  ];
}
