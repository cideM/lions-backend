{ pkgs, spago2nix, projectEnv, deploy-rs, sopsHook, litestream }:
let

  lions-dummy = pkgs.writeScriptBin "lions-dummy" ''
    #!${pkgs.fish}/bin/fish
    for f in ./dev/*; sqlite3 $LIONS_SQLITE_PATH < $f; end
  '';

  lions-ghcid = pkgs.writeScriptBin "lions-ghcid" ''
    #!/bin/sh
    ghcid --no-height-limit --clear --reverse
  '';

  lions-vm = pkgs.writeShellScriptBin "lions-vm" ''
    nix build .#vm
    echo "visit https://localhost:8081/"
    echo "or http://localhost:8080/"
    export QEMU_NET_OPTS="hostfwd=tcp::2221-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443"
    ./result/bin/run-nixos-vm
  '';

  lions-vm-db = pkgs.writeShellScriptBin "lions-vm-db" ''
    scp -P 2221 $LIONS_SQLITE_PATH root@localhost:/var/lib/lions-server/db
  '';

  lions-dev = pkgs.writeShellScriptBin "lions-dev" ''
    nix build .#assets
    trap 'kill $LITESTREAM_ID; exit' INT
    litestream replicate $LIONS_SQLITE_PATH s3://lions-achern-litestream-replica-1/dev-db &
    LITESTREAM_ID=$!
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
    sopsHook
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
      litestream

      # Purescript
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.purescript-language-server
      pkgs.nodePackages.purty

      # Scripts
      lions-dev
      lions-vm
      lions-ghcid
      lions-dummy
      lions-vm-db

      # Infra
      pkgs.terraform_0_15
      pkgs.cli53
      pkgs.packer
      pkgs.awscli2
      deploy-rs
    ];
}
