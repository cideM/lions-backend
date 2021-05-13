{
  description = "Lions Club Website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-20-09.url = "github:NixOS/nixpkgs/nixos-20.09";
    deploy-rs.url = "github:serokell/deploy-rs";
    spago2nix = {
      url = "github:justinwoo/spago2nix/master";
      flake = false;
    };
    bootstrap = {
      url = "https://github.com/twbs/bootstrap/releases/download/v5.0.0-beta2/bootstrap-5.0.0-beta2-dist.zip";
      flake = false;
    };
    bootstrap-icons = {
      url = "https://github.com/twbs/icons/releases/download/v1.4.1/bootstrap-icons-1.4.1.zip";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = { self, spago2nix, bootstrap-icons, bootstrap, nixpkgs, flake-utils, nixpkgs-20-09, deploy-rs, sops-nix }:
    let
      allSystems = flake-utils.lib.eachSystem [ "x86_64-linux" ]
        (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import ./migrate.nix) ];
              config = import ./config.nix;
            };
            release = import ./release.nix { inherit bootstrap-icons bootstrap pkgs; };
            projectEnv = release.project.env;
          in
          rec {
            packages = flake-utils.lib.flattenTree {
              vm = (import "${nixpkgs}/nixos" {
                inherit system;
                configuration = { config, pkgs, ... }:
                  {
                    imports = [
                      ./vm.nix
                      ./systemd-server.nix
                    ];

                    config.serverWorkingDir = "${release.lions-server}/";
                    config.serverExe = "${release.lions-server}/bin/migrate-and-serve";
                  };
              }).vm;
              server = release.lions-server;
              assets = release.assets;
            };

            defaultPackage = packages.server;

            apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "/bin/migrate-and-serve"; };

            defaultApp = apps.server;

            devShell = import ./shell.nix { inherit pkgs spago2nix projectEnv deploy-rs sops-nix; };
          }
        );

      serverSystem = nixpkgs-20-09.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          sops-nix.nixosModules.sops
          {
            config.server = allSystems.packages.x86_64-linux.server;
          }
          ./configuration.nix
          ./systemd-server.nix
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
          path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server;
        };
      };
      nixosConfigurations.server = serverSystem;
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
