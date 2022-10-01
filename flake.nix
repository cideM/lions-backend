{
  description = "Lions Club Website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-20-09.url = "github:NixOS/nixpkgs/nixos-20.09";
    npmlock2nix.url = "github:nix-community/npmlock2nix";
    npmlock2nix.flake = false;
    spago2nix = {
      url = "github:justinwoo/spago2nix/master";
      flake = false;
    };
    bootstrapsrc = {
      url = "https://github.com/twbs/bootstrap/archive/v5.0.2.zip";
      flake = false;
    };
    bootstrap-icons = {
      url = "https://github.com/twbs/icons/releases/download/v1.5.0/bootstrap-icons-1.5.0.zip";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , spago2nix
    , bootstrap-icons
    , bootstrapsrc
    , nixpkgs
    , flake-utils
    , nixpkgs-20-09
    , npmlock2nix
    }:
    let
      allSystems = flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
        (system':
          let
            # This is a hopefully temporary workaround until GHC works with aarch64
            system =
              if system' == "aarch64-darwin" then "x86_64-darwin"
              else system';

            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                (import ./nix/migrate.nix)

                # TODO: Use this for bootstrap etc. as well
                (self: super: {
                  npmlock2nix = pkgs.callPackage npmlock2nix { };
                })

                (self: super: {
                  bootstrapJs = pkgs.stdenv.mkDerivation {
                    name = "bootstrap-js-bundle";
                    src = bootstrapsrc;
                    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
                    dontBuild = true;
                    installPhase = ''
                      mkdir $out
                      cp dist/js/bootstrap.bundle.min.js* $out/
                    '';
                  };
                })

                (self: super: {
                  bootstrap-icons = pkgs.stdenv.mkDerivation {
                    name = "boostrap-icons";
                    src = bootstrap-icons;
                    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
                    dontBuild = true;
                    installPhase = ''
                      mkdir $out
                      cp *.svg $out/
                    '';
                  };
                })

                (self: super: {
                  bootstrapCss = pkgs.stdenv.mkDerivation {
                    name = "bootstrap-scss-source";
                    src = bootstrapsrc;
                    unpackCmd = "${pkgs.unzip}/bin/unzip $curSrc";
                    dontBuild = true;
                    installPhase = ''
                      mkdir $out
                      cp -r scss/* $out/
                    '';
                  };
                })

                (self: super: {
                  lionsServer = pkgs.stdenv.mkDerivation {
                    name = "lions-website";
                    dontUnpack = true;
                    dontBuild = true;
                    installPhase = ''
                      mkdir $out
                      mkdir $out/public/
                      mkdir $out/migrations
                      cp -r ${clientStuff.allAssets}/* $out/public/
                      cp ${migrationsDir}/* $out/migrations/
                      cp ${backend}/bin/run-lions-backend $out/server
                    '';
                  };
                })

                (self: super: {
                  # Hardcode the environment to "test"
                  lionsServerTest = pkgs.writeShellScriptBin "lions-server-test" ''
                    LIONS_ENV=test exec ${pkgs.lionsServer}/server
                  '';
                })


              ];
              config = import ./nix/config.nix;
            };


            # Add turtle to my environment so any Haskell script inside
            # scripts/ can be run with runghc without having to think about
            # where the dependencies come from.
            haskellScriptDeps = [ pkgs.haskellPackages.turtle ];

            backend = import ./backend/default.nix { inherit pkgs; };
            projectEnv = (pkgs.haskell.lib.overrideCabal backend (drv: {
              libraryHaskellDepends = drv.libraryHaskellDepends ++ haskellScriptDeps;
            })).env;

            clientStuff = import ./client/default.nix { inherit pkgs; };

            migrationsDir = builtins.path {
              name = "lions-migrations";
              path = ./backend/migrations;
            };

            lionsE2e = pkgs.writeShellScriptBin "lions-e2e" ''
              exec ${backend}/bin/run-lions-e2e
            '';

            docker = pkgs.dockerTools.buildImage {
              name = "server";
              config = {
                Cmd = [ "${pkgs.lionsServer}/server" ];
              };
            };
          in
          rec {
            packages = flake-utils.lib.flattenTree
              ({
                litestream = pkgs.litestream;
                server = pkgs.lionsServer;
                e2e = lionsE2e;
                testServer = pkgs.lionsServerTest;
                allAssets = clientStuff.allAssets;
                docker = docker;
              });

            defaultPackage = packages.server;

            apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "server"; };
            apps.testServer = flake-utils.lib.mkApp { drv = packages.testServer; exePath = "server"; };
            apps.e2e = flake-utils.lib.mkApp { drv = packages.e2e; exePath = "/bin/lions-e2e"; };

            defaultApp = apps.server;

            devShell = import ./shell.nix {
              inherit pkgs spago2nix projectEnv;
            };
          }
        );

    in
    allSystems;
}
