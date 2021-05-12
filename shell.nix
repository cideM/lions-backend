{ pkgs, spago2nix, projectEnv, deploy-rs, sops-nix }:
let

  lions-dummy = pkgs.writeScriptBin "lions-dummy" ''
    #!${pkgs.fish}/bin/fish
    for f in ./dev/*; sqlite3 $LIONS_SQLITE_PATH < $f; end
  '';

  lions-ghcid = pkgs.writeScriptBin "lions-ghcid" ''
    #!/bin/sh
    ghcid --no-height-limit --clear --reverse
  '';

  lions-dev = pkgs.writeScriptBin "lions-dev" ''
    #!/bin/sh
    echo hi
    nix build .#assets
    cp -r -n ./result/* ./public/
    cabal v2-run
  '';

  spago2nix' = import spago2nix { inherit pkgs; };

in
pkgs.mkShell {
  sopsPGPKeyDirs = [
    "./keys/hosts"
    "./keys/users"
  ];
  inputsFrom = [ projectEnv ];
  nativeBuildInputs = [
    sops-nix.packages.x86_64-linux.sops-pgp-hook
    spago2nix'
  ];
  buildInputs = with pkgs.haskellPackages;
    [
      # Haskell
      ghcid
      ormolu
      hlint
      cabal2nix
      haskell-language-server
      cabal-install
      cabal-fmt
      fast-tags
      hoogle

      pkgs.nixpkgs-fmt

      # Database
      pkgs.go-migrate
      pkgs.sqlite-interactive
      pkgs.sqlite-web

      # Purescript
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.purescript-language-server
      pkgs.nodePackages.purty

      # Scripts
      lions-dev
      lions-ghcid
      lions-dummy

      # Infra
      pkgs.terraform_0_15
      pkgs.cli53
      pkgs.packer
      pkgs.awscli2
      deploy-rs.packages.x86_64-linux.deploy-rs
    ];
}
