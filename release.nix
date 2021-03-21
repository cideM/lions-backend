let
  sources = import ./nix/sources.nix;

  pkgs = import sources.nixpkgs { };

  css = pkgs.stdenv.mkDerivation {
    name = "lions-css";
    src = sources.bootstrap;
    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
    dontBuild = true;
    installPhase = ''
      mkdir $out
      cp css/bootstrap.min.css* $out/
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
    '';
  };

in
{
  css = css;
  assets = assets;
  project = pkgs.haskellPackages.callPackage ./project.nix { };
}
