# This derivation generates a QEMU VM.
# It's probably very hacky because I threw this together as I needed it. I
# never took the time to properly read and understand the QEMU documentation.
# This can be run with the "lions-vm" command, which is declared in
# "shell.nix".
# The VM tries to be as close to the production server ("server.nix") as
# possible. It also uses SOPS but only includes dummy secrets. It doesn't use
# litestream since it can't connect to AWS and in any case it wouldn't make
# sense to back up the VM DB since it starts with seed data and any changes
# should be ephemeral.
{ sops-nix, nixpkgs, serverWorkingDir, serverExe }:

(import "${nixpkgs}/nixos" {
  system = "x86_64-linux";
  configuration = { config, pkgs, ... }:
    {
      imports = [
        sops-nix.nixosModules.sops
        ./sops.nix
        ./shared.nix
        ./systemd-server.nix
        {
          serverWorkingDir = serverWorkingDir;
          serverExe = serverExe;

          # Make sure we use the development SOPS setup, meaning a SOPS file
          # with dummy secrets and the corresponding GPG key.
          sops.defaultSopsFile = ../secrets/vm.yaml;
          sops.gnupg.home = "/etc/pgpstuff/";
          sops.gnupg.sshKeyPaths = [ ];
          environment.etc."pgpstuff/pubring.kbx~".source = builtins.path {
            name = "pubringkbx";
            path = ../. + "/vm-pgp/pubring.kbx~";
          };
          environment.etc."pgpstuff/pubring.kbx".source = ../vm-pgp/pubring.kbx;
          environment.etc."pgpstuff/private-keys-v1.d/ADC6091860174378DC87EFB03C23963A1B4EACA0.key".source = ../vm-pgp/private-keys-v1.d/ADC6091860174378DC87EFB03C23963A1B4EACA0.key;
          environment.etc."pgpstuff/openpgp-revocs.d/27BAD88E87C18A972AC5D6DF54189C237851DE5D.rev".source = ../vm-pgp/openpgp-revocs.d/27BAD88E87C18A972AC5D6DF54189C237851DE5D.rev;

          # Load some seed data into the SQLite database so we can at least login.
          # Use "foo@bar.com" and "foobar"
          systemd.services.prepareDb = {
            before = [ "server.service" ];
            after = [ "migrations.service" ];
            serviceConfig = { Type = "oneshot"; };
            script = ''
              ${pkgs.sqlite-interactive}/bin/sqlite3 /var/lib/lions-server/db << 'EOF'
              INSERT OR REPLACE INTO users (id, email, first_name, last_name, address, mobile_phone_nr, landline_nr, birthday, first_name_partner, last_name_partner, birthday_partner, password_digest)
              VALUES (1, "foo@bar.com", "foo", "bar", "some address, random

                stuff", "120348971203", "120348971203", date('1988-09-02'), "foopartner", "parner last name", date('1988-09-02'), "$2y$04$NFwlwssLnLtvJEwZ0XtXgOjAHPqUIDHZfd2CiZsVDgmk1NTrdwT1a");
              EOF

              ${pkgs.sqlite-interactive}/bin/sqlite3 /var/lib/lions-server/db << 'EOF'
              INSERT OR REPLACE INTO user_roles (userid, roleid)
              VALUES (1, 0);

              INSERT OR REPLACE INTO user_roles (userid, roleid)
              VALUES (1, 1);
              EOF
            '';
            wantedBy = [ "multi-user.target" ];
          };

          nixpkgs.overlays = [ (import ./migrate.nix) ];

          # For debugging
          environment.systemPackages = [ pkgs.sqlite-interactive ];

          # We want to be able to log in as root through SSH
          services.openssh.permitRootLogin = "yes";

          # Don't open a new window, instead do everything through SSH ideally.
          virtualisation.graphics = false;

          # Just proxy to the server without any let's encrypt stuff.
          services.caddy = {
            enable = true;
            config = ''
              {
                auto_https disable_redirects
              }

              localhost:443, localhost:80

              reverse_proxy localhost:3000
            '';
          };
        }
      ];
    };
})
