{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-s3
, amazonka-ses, base, base64, bcrypt, bytestring, cipher-aes
, clientsession, cmark-gfm, containers, cookie, crypto-api
, crypto-cipher-types, data-default, directory, email-validate
, errors, filepath, filepattern, http-client, http-types, katip
, lens, lib, lucid, mtl, parsec, process, req, resourcet, retry
, safe-exceptions, scrypt, sqlite-simple, string-interpolate, tasty
, tasty-hunit, text, time, transformers, unliftio, uri-encode, uuid
, vault, wai, wai-extra, wai-middleware-static, wai-session, warp
, xss-sanitize
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-s3 amazonka-ses base base64
    bcrypt bytestring cipher-aes clientsession cmark-gfm containers
    cookie crypto-api crypto-cipher-types data-default directory
    email-validate errors filepath filepattern http-client http-types
    katip lens lucid mtl parsec resourcet safe-exceptions scrypt
    sqlite-simple string-interpolate text time transformers unliftio
    uri-encode uuid vault wai wai-extra wai-middleware-static
    wai-session warp xss-sanitize
  ];
  executableHaskellDepends = [
    base process req retry safe-exceptions tasty tasty-hunit
  ];
  testHaskellDepends = [
    amazonka-ses base bytestring clientsession containers cookie
    directory email-validate filepath filepattern http-types katip
    lucid mtl resourcet safe-exceptions sqlite-simple
    string-interpolate tasty tasty-hunit text time transformers
    unliftio uri-encode vault wai wai-extra warp
  ];
  license = "unknown";
}
