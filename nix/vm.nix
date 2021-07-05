{ pkgs, config, ... }:

{
  config = {
    users.mutableUsers = false;
    users.users.root = {
      password = "";
      openssh = {
        authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRrxsAwzXqj1qtLUJoPMCT5lusAE2DV84McC/EXR/96yq3il61o1RmsDEMRZTIrcha1QOBywGLcwCIGQpkL3nWA+cS47zMqfozsrehU3VuCGktpr2gL8d/Avl71hXsyrmpRysdY1liZPwXaHjm8DKCx+WiVD+SpuCk1dA8uPa4QNMPkxpJtPD5T1pZBYAfWtxXXpkGkBwZIGeQIhzeSLQSQY603MOE8lXIIGfvraRFvby3xN3Cp9ZOJlxhfB6JFAeCuWg5n4focLJDP6xDf2eDck5lJJ5yLLp0wN5IPqfm89B8a7CiY8nCG/m59d+hrg0Zkv4hBMynX/bHgWUUAZ8T yuuki@yuuki-desktop"
        ];
      };
    };

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];

    services.openssh.enable = true;
    services.openssh.permitRootLogin = "yes";

    virtualisation.graphics = false;

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
