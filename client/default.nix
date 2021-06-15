{ pkgs, bootstrap, bootstrap-icons, backend }:
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

  allAssets = pkgs.symlinkJoin {
    name = "lions-all-client-assets";
    paths = [
      clientside
      assets
    ];
  };

in
{
  inherit css assets icons clientside allAssets;
}