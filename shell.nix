{ pkgs, spago2nix, projectEnv, deploy-rs, sopsHook, litestream, bootstrapSrc }:
let
  # Generate tags based on the exposed Haskell modules. "fast-tags" doesn't
  # respect .gitignore and as such just calling fast-tags will result in lots
  # of false positives.
  lions-tags = pkgs.writeShellScriptBin "lions-tags" ''
    ${pkgs.haskellPackages.fast-tags}/bin/fast-tags --cabal ./backend/lions-backend.cabal
  '';

  # Run this command to BUILD AND START a QEMU VM. This should work on both
  # MacOS and Linux. On MacOS it uses Docker, since virtfs is not supported.
  # The VM uses hardcoded DB values as seed data. Please update the vm nix
  # derivation if more data is needed, rather than trying to somehow mirror the
  # local SQLite DB into the QEMU VM.
  lions-vm = pkgs.writeShellScriptBin "lions-vm" (if pkgs.stdenv.isDarwin then ''
    docker run -it -p 127.0.0.1:81:8081 -p 127.0.0.1:80:8080 --rm -v nixcache2:/nix -v $(pwd):/foo -w /foo  -v ~/.ssh:/root/.ssh:ro nixpkgs/nix-flakes bash -c 'nix build .#packages.x86_64-linux.vm && QEMU_NET_OPTS="hostfwd=tcp::2221-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443" ./result/bin/run-lions-server-vm'
  '' else ''
    nix build .#packages.x86_64-linux.vm || exit 1
    echo "visit https://localhost:8081/"
    echo "or http://localhost:8080/"
    export QEMU_NET_OPTS="hostfwd=tcp::2221-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8081-:443"
    ./result/bin/run-lions-server-vm
  '');

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
      pkgs.bash_5
      pkgs.jq
      lions-vm
      lions-tags

      # Infra
      # This doesn't work on NixOS because of yet another fucking NPM package
      # that tries to reimplement an OS package manager and downloads random
      # binaries from the internet. The problem here is `node-re2`. I really
      # hate NPM and Javascript. https://github.com/uhop/node-re2/issues/107
      # pkgs.nodePackages.firebase-tools
      pkgs.terraform_0_15
      pkgs.nodejs
      pkgs.cli53
      pkgs.nodePackages.sass
      pkgs.nodePackages.node2nix
      pkgs.nodePackages.postcss-cli
      pkgs.packer
      pkgs.awscli2
      deploy-rs
    ];
}
