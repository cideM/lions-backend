let
  release = import ./release.nix;

  src = (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "refs/heads/nixos-20.09-small";
    rev = "ae1b121d9a68518dbf46124397e34e465d3cdf6c";
  });

  pkgs = import src { };

  config = {
    imports = [
      "${src}/nixos/modules/virtualisation/digital-ocean-image.nix"
    ];
  };

# TODO:
# Create user
# User that to run application
# Env vars, at least SQLite
# world -> caddy -> server
# MAKE IT WORK

in {
  satsuki = {
    systemd.services.server = {
      wantedBy = ["multi-user.target"];

      serviceConfig = {
        WorkingDirectory = "${release.production}/"
        ExecStart = "${release.production}/server";
        Restart = "on-failure";
      };
    };

    services.caddy = {
      enable = true;
      config = ''
      '';
    };
  };
}

