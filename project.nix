{ mkDerivation, aeson, amazonka, amazonka-ses, base, base64, bcrypt
, bytestring, cipher-aes, clientsession, containers, cookie
, crypto-api, crypto-cipher-types, email-validate, fast-logger
, filepattern, http-types, lens, lib, lucid, safe-exceptions
, scrypt, sqlite-simple, string-interpolate, tasty, tasty-hunit
, text, time, uri-encode, uuid, vault, wai, wai-extra
, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-ses base base64 bcrypt bytestring
    cipher-aes clientsession containers cookie crypto-api
    crypto-cipher-types email-validate fast-logger http-types lens
    lucid safe-exceptions scrypt sqlite-simple string-interpolate text
    time uri-encode uuid vault wai wai-extra wai-middleware-static
    wai-session warp
  ];
  testHaskellDepends = [
    base filepattern sqlite-simple tasty tasty-hunit
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
