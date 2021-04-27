{
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        # Too strict version bounds on base16-bytestring and http-link-header.
        # This patch will be merged when next release comes.
        github = pkgs.haskell.lib.appendPatch haskellPackagesOld.github (pkgs.fetchpatch {
          url = "https://github.com/phadej/github/commit/514b175851dd7c4a9722ff203dd6f652a15d33e8.patch";
          sha256 = "0pmx54xd7ah85y9mfi5366wbnwrp918j0wbx8yw8hrdac92qi4gh";
        });
      };
    };
  };
}
