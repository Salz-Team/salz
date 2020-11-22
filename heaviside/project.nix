{ mkDerivation, aeson, amqp, base, bytestring, containers
, postgresql-simple, stdenv, text, turtle
}:
mkDerivation {
  pname = "heaviside";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson amqp base bytestring containers postgresql-simple text turtle
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
