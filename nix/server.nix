# Open this side-by-side with vm.nix to see the differences between VM and server.
{ nixpkgs, sops-nix, serverWorkingDir, serverExe }:

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    ({ pkgs, config, ... }: { nixpkgs.overlays = [ (import ./migrate.nix) ]; })
    sops-nix.nixosModules.sops
    ./sops.nix
    ./systemd-server.nix
    ./systemd-litestream.nix
    ./shared.nix
    ({ pkgs, config, ... }:
      {
        imports = [
          "${nixpkgs}/nixos/modules/virtualisation/digital-ocean-image.nix"
        ];

        config = {
          boot.loader.grub.enable = true;

          sops.defaultSopsFile = ../secrets/prod.yaml;

          users.users.admin.isNormalUser = true;
          serverWorkingDir = serverWorkingDir;
          serverExe = serverExe;

          nix.gc = {
            automatic = true;
            dates = "weekly";
          };

          environment.systemPackages = with pkgs; [
            pkgs.litestream
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
        };
      })
  ];
}
