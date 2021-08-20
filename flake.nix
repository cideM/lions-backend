{
  description = "Lions Club Website";

  inputs = {
    litestream-src.url = "github:benbjohnson/litestream/v0.3.4";
    litestream-src.flake = false;
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-20-09.url = "github:NixOS/nixpkgs/nixos-20.09";
    deploy-rs.url = "github:serokell/deploy-rs";
    spago2nix = {
      url = "github:justinwoo/spago2nix/master";
      flake = false;
    };
    bootstrap = {
      url = "https://github.com/twbs/bootstrap/releases/download/v5.0.1/bootstrap-5.0.1-dist.zip";
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
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs =
    { self
    , spago2nix
    , bootstrap-icons
    , bootstrap
    , bootstrapsrc
    , nixpkgs
    , flake-utils
    , nixpkgs-20-09
    , deploy-rs
    , sops-nix
    , litestream-src
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
              overlays = [ (import ./nix/migrate.nix) ];
              config = import ./nix/config.nix;
            };

            litestream = pkgs.buildGoModule rec {
              pname = "litestream";
              version = "0.3.4";

              src = litestream-src;
              vendorSha256 = "sha256-O1d2xQ+1Xn88JCaVv4ge8HmrFqEl3lRTJIhgZoAri7U=";
            };

            # Add turtle to my environment so any Haskell script inside
            # scripts/ can be run with runghc without having to think about
            # where the dependencies come from.
            haskellScriptDeps = [ pkgs.haskellPackages.turtle ];

            backend = import ./backend/default.nix { inherit pkgs; };
            projectEnv = (pkgs.haskell.lib.overrideCabal backend (drv: {
              libraryHaskellDepends = drv.libraryHaskellDepends ++ haskellScriptDeps;
            })).env;

            clientStuff = import ./client/default.nix { inherit bootstrap-icons pkgs backend system; bootstrap = bootstrapsrc; };

            migrationsDir = builtins.path {
              name = "lions-migrations";
              path = ./backend/migrations;
            };

            # Hardcode the environment to "test"
            lionsServerTest = pkgs.writeShellScriptBin "lions-server-test" ''
              LIONS_ENV=test exec ${lionsServer}/server
            '';

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

            # Note that we're using the test server which is just a wrapper
            # around the normal server with LIONS_ENV set to "test"
            vm = (import ./nix/vm.nix {
              inherit nixpkgs sops-nix;
              # Yes, this is correct. If we use "testServer" we just end up in
              # the directory of the wrapper, which includes only a "bin"
              # folder.
              serverWorkingDir = "${allSystems.packages.x86_64-linux.server}/";
              serverExe = "${allSystems.packages.x86_64-linux.testServer}/bin/lions-server-test";
            });
          in
          rec {
            packages = flake-utils.lib.flattenTree
              ({
                litestream = litestream;
                server = lionsServer;
                testServer = lionsServerTest;
                allAssets = clientStuff.allAssets;
                clientside = clientStuff.clientside;
              } // (nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
                vm = vm.vm;
              }));

            defaultPackage = packages.server;

            apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "server"; };
            apps.testServer = flake-utils.lib.mkApp { drv = packages.testServer; exePath = "server"; };

            defaultApp = apps.server;

            devShell = import ./shell.nix {
              inherit pkgs spago2nix projectEnv litestream;
              bootstrapSrc = clientStuff.bootstrapSrc;
              sopsHook = sops-nix.packages.${system}.sops-import-keys-hook;
              deploy-rs = deploy-rs.packages.${system}.deploy-rs;
            };
          }
        );

      serverSystem = (import ./nix/server.nix {
        inherit nixpkgs sops-nix;
        serverWorkingDir = "${allSystems.packages.x86_64-linux.server}/";
        serverExe = "${allSystems.packages.x86_64-linux.server}/server";
        litestream = allSystems.packages.x86_64-linux.litestream;
      });

    in
    allSystems // rec {
      deploy.nodes.server = {
        sshUser = "root";
        hostname = "134.122.81.69";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos serverSystem;
        };
      };
      nixosConfigurations.server = serverSystem;
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
