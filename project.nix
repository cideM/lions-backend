{ mkDerivation, aeson, amazonka, amazonka-ses, base, bcrypt
, bytestring, clientsession, containers, cookie, crypto-api
, email-validate, fast-logger, http-types, lens, lucid
, safe-exceptions, sqlite-simple, stdenv, string-interpolate, text
, time, uri-encode, uuid, vault, wai, wai-extra
, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson amazonka amazonka-ses base bcrypt bytestring clientsession
    containers cookie crypto-api email-validate fast-logger http-types
    lens lucid safe-exceptions sqlite-simple string-interpolate text
    time uri-encode uuid vault wai wai-extra wai-middleware-static
    wai-session warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
