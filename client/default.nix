{ pkgs }:
let
  nodeModules = pkgs.npmlock2nix.node_modules {
    nodejs = pkgs.nodejs-14_x;
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
      export HOME=$TMP
      mkdir $out
      cp $src/* $out/
      cp ${styles}/* $out/
      cp ${pkgs.bootstrapJs}/* $out/
      mkdir $out/icons
      cp ${pkgs.bootstrap-icons}/* $out/icons
    '';
  };

  allAssets = pkgs.symlinkJoin {
    name = "lions-all-client-assets";
    paths = [
      assets
    ];
  };

in
{
  inherit styles assets allAssets;
}
