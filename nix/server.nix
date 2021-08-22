# Open this side-by-side with vm.nix to see the differences between VM and server.
{ nixpkgs, sops-nix, serverWorkingDir, serverExe, litestream }:

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    sops-nix.nixosModules.sops
    ./sops.nix
    ./systemd-server.nix
    ./systemd-litestream.nix
    ./shared.nix
    ({ pkgs, config, ... }:
      {
        imports = [
          "${nixpkgs-20-09}/nixos/modules/virtualisation/digital-ocean-image.nix"
        ];

        boot.loader.grub.enable = true;

        sops.defaultSopsFile = ../secrets/prod.yaml;

        users.users.admin.isNormalUser = true;
        config.serverWorkingDir = serverWorkingDir;
        config.serverExe = serverExe;

        environment.systemPackages = with pkgs; [
          litestream
          sqlite-interactive
          go-migrate
        ];

        services.caddy = {
          enable = true;
          email = "yuuki@protonmail.com";
          config = ''
            members.lions-achern.de

            reverse_proxy localhost:3000
          '';
        };
      })
  ];
}
