{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, case-insensitive, fetchgit, lib, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-sso";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka";
    sha256 = "15vc0fc4qys5ggcvyiib0khi2fpbv9r3r7ixi037404qn6ik3x67";
    rev = "006563901b94ee586453855ebd73ad69442f9235";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/services/amazonka-sso; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring case-insensitive tasty
    tasty-hunit text time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Single Sign-On SDK";
  license = lib.licenses.mpl20;
}
