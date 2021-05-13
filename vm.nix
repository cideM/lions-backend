{ pkgs, config, ... }:

{
  config = {
    users.users.root.password = "";
    users.mutableUsers = false;

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];

    services.caddy = {
      enable = true;
      config = ''
        {
          auto_https disable_redirects
        }

        localhost:443, localhost:80

        reverse_proxy 127.0.0.1:3000
      '';
    };
  };
}
