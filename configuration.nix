{ pkgs, config, ... }: {
  boot.loader.grub.enable = true;

  sops.defaultSopsFile = ./secrets.yaml;

  users.mutableUsers = false;

  environment.systemPackages = with pkgs; [
    sqlite-interactive
  ];

  users.users.admin = {
    isNormalUser = true;
    password = "";
    extraGroups = [ "wheel" "sudo" config.users.groups.keys.name ];
    openssh = {
      authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRrxsAwzXqj1qtLUJoPMCT5lusAE2DV84McC/EXR/96yq3il61o1RmsDEMRZTIrcha1QOBywGLcwCIGQpkL3nWA+cS47zMqfozsrehU3VuCGktpr2gL8d/Avl71hXsyrmpRysdY1liZPwXaHjm8DKCx+WiVD+SpuCk1dA8uPa4QNMPkxpJtPD5T1pZBYAfWtxXXpkGkBwZIGeQIhzeSLQSQY603MOE8lXIIGfvraRFvby3xN3Cp9ZOJlxhfB6JFAeCuWg5n4focLJDP6xDf2eDck5lJJ5yLLp0wN5IPqfm89B8a7CiY8nCG/m59d+hrg0Zkv4hBMynX/bHgWUUAZ8T yuuki@yuuki-desktop"
      ];
    };
  };

  services.openssh = { enable = true; };

  networking.hostName = "lions-server";

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.caddy = {
    enable = true;
    email = "yuuki@protonmail.com";
    config = ''
      members.lions-achern.de

      reverse_proxy localhost:3000
    '';
  };
}
