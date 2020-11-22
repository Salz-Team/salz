{ mkDerivation, aeson, amqp, base, bytestring, stdenv, text }:
mkDerivation {
  pname = "bell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson amqp base bytestring text ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
