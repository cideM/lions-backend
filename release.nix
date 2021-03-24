let
  sources = import ./nix/sources.nix;

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

  pkgs = import sources.nixpkgs { inherit config; };

  css = pkgs.stdenv.mkDerivation {
    name = "lions-css";
    src = sources.bootstrap;
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
    src = sources.bootstrap-icon;
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
    src = builtins.path { name = "purescript"; path = ./.; };
    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildSpagoStyle
    ];
    nativeBuildInputs = with pkgs; [
      purescript
      spago
    ];
    unpackPhase = ''
      cp $src/spago.dhall .
      cp $src/packages.dhall .
      cp -r $src/src .
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
      cp ${bin}/bin/lions-backend $out/server
    '';
  };

in
{
  css = css;
  assets = assets;
  icons = icons;
  project = bin;
  production = production;
  clientside = clientside;
}
