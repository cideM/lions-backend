let
  src = (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs";
    ref = "refs/heads/nixos-20.09-small";
    rev = "ae1b121d9a68518dbf46124397e34e465d3cdf6c";
  });

  pkgs = import src { };

  config = {
    nix = {
      package = pkgs.nixUnstable;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';
    };

    imports = [
      "${src}/nixos/modules/virtualisation/digital-ocean-image.nix"
    ];
  };

in
(pkgs.nixos config).digitalOceanImage
