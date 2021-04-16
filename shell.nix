let
  sources = import ./nix/sources.nix;

  migrateOverlay = self: super: {
    go-migrate = super.go-migrate.overrideAttrs (old: {
      buildFlags = "-tags 'sqlite3'";
    });
  };

  # TODO: Remove this in a few days. Was merged 6d ago, and at the time of
  # writing it's Apr 2
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          # Too strict version bounds on base16-bytestring and http-link-header.
          # This patch will be merged when next release comes.
          github = pkgs.haskell.lib.appendPatch haskellPackagesOld.github (pkgs.fetchpatch {
            url = "https://github.com/phadej/github/commit/514b175851dd7c4a9722ff203dd6f652a15d33e8.patch";
            sha256 = "0pmx54xd7ah85y9mfi5366wbnwrp918j0wbx8yw8hrdac92qi4gh";
          });
        };
      };
    };
  };

  pkgs = import sources.nixpkgs { inherit config; overlays = [ migrateOverlay ]; };

  projectEnv = (import ./release.nix).project.env;

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
    nix-build -A assets release.nix
    cp -r -n ./result/* ./public/
    cabal v2-run
  '';

  spago2nix = import sources.spago2nix { inherit pkgs; };

in
pkgs.mkShell {
  inputsFrom = [ projectEnv ];
  nativeBuildInputs = [ spago2nix ];
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
    ];
}
