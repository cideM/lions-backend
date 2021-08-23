{ pkgs }:
let
  nodeModules = pkgs.npmlock2nix.node_modules {
    src = ./.;
  };

  styles = pkgs.stdenv.mkDerivation {
    name = "lions-styles";
    src = builtins.path { name = "lions-scss"; path = ./sass; };
    dontUnpack = true;
    buildPhase = ''
      ln -s ${nodeModules}/node_modules ./node_modules
      export PATH="${nodeModules}/node_modules/.bin:$PATH"
      ${pkgs.nodePackages.sass}/bin/sass --style=compressed --load-path=${pkgs.bootstrapCss} $src/styles.scss style.css
      mkdir build
      postcss style.css --use autoprefixer -d build/
      mkdir $out
      cp build/style.css $out/style.css
    '';
    dontInstall = true;
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
      cp ${pkgs.bootstrapJs}/* $out/
      mkdir $out/icons
      cp ${pkgs.bootstrap-icons}/* $out/icons
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
  inherit styles assets allAssets;
}
