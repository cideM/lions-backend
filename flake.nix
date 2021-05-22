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
    , nixpkgs
    , flake-utils
    , nixpkgs-20-09
    , deploy-rs
    , sops-nix
    , litestream-src
    }:
    let
      allSystems = flake-utils.lib.eachDefaultSystem
        (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import ./migrate.nix) ];
              config = import ./config.nix;
            };

            litestream = pkgs.buildGoModule rec {
              pname = "litestream";
              version = "0.3.4";

              src = litestream-src;
              vendorSha256 = "sha256-O1d2xQ+1Xn88JCaVv4ge8HmrFqEl3lRTJIhgZoAri7U=";
            };

            release = import ./release.nix { inherit bootstrap-icons bootstrap pkgs; };

            projectEnv = release.project.env;
          in
          rec {
            packages = flake-utils.lib.flattenTree {
              litestream = litestream;
              server = release.lions-server;
              assets = release.assets;
              clientside = release.clientside;
            };

            defaultPackage = packages.server;

            apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "/bin/migrate-and-serve"; };

            defaultApp = apps.server;

            devShell = import ./shell.nix {
              inherit pkgs spago2nix projectEnv litestream;
              sopsHook = sops-nix.packages.${system}.sops-pgp-hook;
              deploy-rs = deploy-rs.packages.${system}.deploy-rs;
            };
          }
        );

      serverSystem = system: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          sops-nix.nixosModules.sops
          {
            # This sucks, install the server systemwide or something
            config.serverWorkingDir = "${allSystems.packages.x86_64-linux.server}/";
            config.serverExe = "${allSystems.packages.x86_64-linux.server}/bin/migrate-and-serve";
          }
          {
            environment.systemPackages = [ allSystems.packages.x86_64-linux.litestream ];
          }
          ./configuration.nix
          ./systemd-server.nix
          ./systemd-litestream.nix
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
