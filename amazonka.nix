{ mkDerivation, aeson, amazonka-core, amazonka-sso, amazonka-sts
, base, bytestring, conduit, directory, exceptions, fetchgit
, http-client, http-conduit, http-types, ini, lens, lib, resourcet
, retry, text, time, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "amazonka";
  version = "2.0";
  src = fetchgit {
    url = "https://github.com/brendanhay/amazonka";
    sha256 = "15vc0fc4qys5ggcvyiib0khi2fpbv9r3r7ixi037404qn6ik3x67";
    rev = "006563901b94ee586453855ebd73ad69442f9235";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/lib/amazonka; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson amazonka-core amazonka-sso amazonka-sts base bytestring
    conduit directory exceptions http-client http-conduit http-types
    ini lens resourcet retry text time transformers
    unordered-containers uuid
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = lib.licenses.mpl20;
}
