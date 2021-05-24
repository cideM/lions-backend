{ mkDerivation, aeson, amazonka, amazonka-ses, base, bcrypt
, bytestring, capability, clientsession, containers, cookie
, crypto-api, email-validate, errors, http-types, katip, lens, lib
, lucid, mtl, safe-exceptions, sqlite-simple, string-interpolate
, text, text-show, time, transformers, unliftio, uri-encode, uuid
, vault, wai, wai-extra, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson amazonka amazonka-ses base bcrypt bytestring capability
    clientsession containers cookie crypto-api email-validate errors
    http-types katip lens lucid mtl safe-exceptions sqlite-simple
    string-interpolate text text-show time transformers unliftio
    uri-encode uuid vault wai wai-extra wai-middleware-static
    wai-session warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
