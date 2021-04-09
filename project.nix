{ mkDerivation, base, bcrypt, bytestring, capability, clientsession
, containers, cookie, crypto-api, email-validate, errors
, fast-logger, http-types, lib, logging-effect, lucid, mtl
, safe-exceptions, sqlite-simple, text, text-show, time
, transformers, unliftio, vault, wai, wai-extra
, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bcrypt bytestring capability clientsession containers cookie
    crypto-api email-validate errors fast-logger http-types
    logging-effect lucid mtl safe-exceptions sqlite-simple text
    text-show time transformers unliftio vault wai wai-extra
    wai-middleware-static wai-session warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
