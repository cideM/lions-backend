{ pkgs, bootstrap, bootstrap-icons }:
let
  css = pkgs.stdenv.mkDerivation {
    name = "lions-css";
    src = bootstrap;
    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp css/bootstrap.min.css* $out/
      cp js/bootstrap.bundle.min.js* $out/
    '';
  };

  icons = pkgs.stdenv.mkDerivation {
    name = "boostrap-icons";
    src = bootstrap-icons;
    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp *.svg $out/
    '';
  };

  assets = pkgs.stdenv.mkDerivation {
    name = "lions-assets";
    src = builtins.path { name = "assets"; path = ./assets; };
    dontUnpack = true;
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp $src/* $out/
      cp ${css}/* $out/
      mkdir $out/icons
      cp ${icons}/* $out/icons
    '';
  };

  spagoPkgs = import ./spago-packages.nix { inherit pkgs; };

  clientside = pkgs.stdenv.mkDerivation {
    name = "lions-client";
    src = [
      ./packages.dhall
      ./spago.dhall
      ./src
    ];
    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildSpagoStyle
    ];
    nativeBuildInputs = with pkgs; [
      purescript
      spago
    ];
    unpackPhase = ''
      for srcFile in $src; do
        cp -r $srcFile $(stripHash $srcFile)
      done
      install-spago-style
    '';
    buildPhase = ''
      build-spago-style "./src/**/*.purs"
      ${pkgs.spago}/bin/spago bundle-app --no-install --main Main --no-build --global-cache skip
    '';
    installPhase = ''
      mkdir $out
      mv output $out/
      mv index.js $out/
    '';
  };

  bin = pkgs.haskellPackages.callPackage ./project.nix { };

  production = pkgs.stdenv.mkDerivation {
    name = "lions-website";
    dontUnpack = true;
    dontBuild = true;
    installPhase = ''
      mkdir $out
      mkdir $out/public/
      cp -r ${assets}/* $out/public/
      cp -r ${clientside} $out/public/
      cp ${bin}/bin/lions-backend $out/server
    '';
  };

  migrationsDir = builtins.path {
    name = "lions-migrations";
    path = ./migrations;
  };

  lions-server =
    let
      wrapped = pkgs.writeScriptBin "migrate-and-serve" ''
        #!${pkgs.bash}/bin/bash
        if [ -z ''${LIONS_SQLITE_PATH+x} ]; then
          echo "LIONS_SQLITE_PATH not set"
          exit 1
        fi
        echo "running migrations"
        ${pkgs.go-migrate}/bin/migrate -path ${migrationsDir} -database "sqlite3://$LIONS_SQLITE_PATH" up
        echo "starting server"
        ${production}/server
      '';
    in
    # We want the script that runs the migrations and starts the server to
    # reside in the same folder in /nix/store so it's easier to handle the
    # working directory for systemd
    pkgs.symlinkJoin {
      name = "lions-server";
      paths = [
        wrapped
        production
      ];
    };

in
{
  inherit css assets icons production clientside lions-server;
  project = bin;
}
