{ mkDerivation, aeson, base, bcrypt, bytestring, capability
, clientsession, containers, cookie, crypto-api, email-validate
, errors, http-types, katip, lucid, mtl, safe-exceptions
, sqlite-simple, stdenv, text, text-show, time, transformers
, unliftio, uri-encode, uuid, vault, wai, wai-extra
, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bcrypt bytestring capability clientsession containers
    cookie crypto-api email-validate errors http-types katip lucid mtl
    safe-exceptions sqlite-simple text text-show time transformers
    unliftio uri-encode uuid vault wai wai-extra wai-middleware-static
    wai-session warp
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
