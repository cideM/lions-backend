{
  description = "Lions Club Website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
  };

  outputs = { self, spago2nix, bootstrap-icons, bootstrap, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
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
            server = release.production;
          };

          defaultPackage = packages.server;

          apps.server = flake-utils.lib.mkApp { drv = packages.server; exePath = "/server"; };

          defaultApp = apps.server;

          devShell = import ./shell.nix { inherit pkgs spago2nix projectEnv; };
        }
      );
}
