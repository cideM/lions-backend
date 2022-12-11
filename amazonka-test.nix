{ mkDerivation, aeson, amazonka-core, base, bifunctors, bytestring
, case-insensitive, conduit, conduit-extra, fetchgit, groom
, http-client, http-types, lib, process, resourcet, tasty
, tasty-hunit, template-haskell, temporary, text, time
, unordered-containers, yaml
}:
mkDerivation {
  pname = "amazonka-test";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka";
    sha256 = "15vc0fc4qys5ggcvyiib0khi2fpbv9r3r7ixi037404qn6ik3x67";
    rev = "006563901b94ee586453855ebd73ad69442f9235";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka-test; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core base bifunctors bytestring case-insensitive
    conduit conduit-extra groom http-client http-types process
    resourcet tasty tasty-hunit template-haskell temporary text time
    unordered-containers yaml
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Common functionality for Amazonka library test-suites";
  license = lib.licenses.mpl20;
}
