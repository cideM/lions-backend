{ mkDerivation, base, bcrypt, bytestring, clientsession, cookie
, fast-logger, http-types, lib, lucid, sqlite-simple, text, wai
, wai-extra, wai-middleware-static, wai-session, warp
}:
mkDerivation {
  pname = "lions-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bcrypt bytestring clientsession cookie fast-logger http-types
    lucid sqlite-simple text wai wai-extra wai-middleware-static
    wai-session warp
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
