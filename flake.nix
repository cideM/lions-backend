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
      allSystems = flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
        (system:
          let
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

            vm = (import "${nixpkgs}/nixos" {
              system = "x86_64-linux";
              configuration = { config, pkgs, ... }:
                {
                  imports = [
                    ({ nixpkgs.overlays = [ (import ./nix/migrate.nix) ]; })
                    ({
                      systemd.services.devSecrets =
                        {
                          before = [ "server" ];
                          script = ''
                            [[ -d /run/secrets ]] || mkdir /run/secrets
                            echo "${(builtins.getEnv "LIONS_AWS_SES_ACCESS_KEY")}" > /run/secrets/aws_ses_access_key
                            echo "${(builtins.getEnv "LIONS_AWS_SES_SECRET_ACCESS_KEY")}" > /run/secrets/aws_ses_secret_access_key
                            echo "${(builtins.getEnv "LIONS_SCRYPT_SIGNER_KEY")}" > /run/secrets/signerkey
                            echo "${(builtins.getEnv "LIONS_SCRYPT_SALT_SEP")}" > /run/secrets/saltsep
                            echo "${(builtins.getEnv "LITESTREAM_ACCESS_KEY_ID")}" > /run/secrets/litestream_aws_key
                            echo "${(builtins.getEnv "LITESTREAM_SECRET_ACCESS_KEY")}" > /run/secrets/litestream_aws_secret
                          '';
                          wantedBy = [ "multi-user.target" ];
                        };
                    })
                    ./nix/vm.nix
                    ./nix/systemd-server.nix
                  ];

                  config.serverWorkingDir = "${allSystems.packages.x86_64-linux.server}/";
                  config.serverExe = "${allSystems.packages.x86_64-linux.server}/server";
                };
            });
          in
          rec {
            packages = flake-utils.lib.flattenTree
              ({
                litestream = litestream;
                server = lionsServer;
                allAssets = clientStuff.allAssets;
                clientside = clientStuff.clientside;
              } // (nixpkgs.lib.optionalAttrs (system == "x86_64-linux") {
                vm = vm.vm;
              }));

            defaultPackage = packages.server;

            apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "server"; };

            defaultApp = apps.server;

            devShell = import ./shell.nix {
              inherit pkgs spago2nix projectEnv litestream;
              bootstrapSrc = clientStuff.bootstrapSrc;
              sopsHook = sops-nix.packages.${system}.sops-pgp-hook;
              deploy-rs = deploy-rs.packages.${system}.deploy-rs;
            };
          }
        );

      serverSystem = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          sops-nix.nixosModules.sops
          {
            # TODO: This sucks, install the server systemwide or something
            config.serverWorkingDir = "${allSystems.packages.x86_64-linux.server}/";
            config.serverExe = "${allSystems.packages.x86_64-linux.server}/bin/migrate-and-serve";
          }
          {
            environment.systemPackages = [ allSystems.packages.x86_64-linux.litestream ];
          }
          ./nix/configuration.nix
          ./nix/systemd-server.nix
          ./nix/systemd-litestream.nix
          {
            imports = [
              "${nixpkgs-20-09}/nixos/modules/virtualisation/digital-ocean-image.nix"
            ];
          }

        ];
      };

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
