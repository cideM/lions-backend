{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-s3
, amazonka-ses, base, base64, bcrypt, bytestring, cipher-aes
, clientsession, containers, cookie, crypto-api
, crypto-cipher-types, directory, email-validate, fast-logger
, filepattern, http-types, lens, lib, lucid, resourcet
, safe-exceptions, scrypt, sqlite-simple, string-interpolate, tasty
, tasty-hunit, text, time, uri-encode, uuid, vault, wai, wai-extra
, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-s3 amazonka-ses base base64
    bcrypt bytestring cipher-aes clientsession containers cookie
    crypto-api crypto-cipher-types directory email-validate fast-logger
    http-types lens lucid resourcet safe-exceptions scrypt
    sqlite-simple string-interpolate text time uri-encode uuid vault
    wai wai-extra wai-middleware-static wai-session warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    amazonka-ses base bytestring clientsession containers cookie
    email-validate fast-logger filepattern http-types lens lucid
    sqlite-simple string-interpolate tasty tasty-hunit text time
    uri-encode vault wai wai-extra warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
