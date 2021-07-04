{ pkgs, bootstrap, bootstrap-icons, backend }:
let
  bootstrapSrc = pkgs.stdenv.mkDerivation {
    name = "bootstrap-scss-source";
    src = bootstrap;
    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp -r scss/* $out/
    '';
  };

  bootstrapJs = pkgs.stdenv.mkDerivation {
    name = "bootstrap-js-bundle";
    src = bootstrap;
    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp dist/js/bootstrap.bundle.min.js* $out/
    '';
  };

  styles = pkgs.stdenv.mkDerivation {
    name = "lions-styles";
    src = builtins.path { name = "lions-scss"; path = ./sass; };
    dontUnpack = true;
    buildPhase = ''
      ${pkgs.nodePackages.sass}/bin/sass --load-path=${bootstrapSrc} $src/styles.scss style.css
      mkdir $out
      cp style.css $out/style.css
    '';
    dontInstall = true;
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
      cp ${styles}/* $out/
      cp ${bootstrapJs}/* $out/
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
  inherit bootstrapSrc styles assets icons clientside allAssets;
}
