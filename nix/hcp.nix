{ stdenv
, lib
, fetchurl
, unzip
}:

stdenv.mkDerivation rec {
  pname = "hcp";
  version = "0.4.0";

  src = fetchurl {
    url = "https://releases.hashicorp.com/${pname}/${version}/${pname}_${version}_linux_amd64.zip";
    hash = "sha256-9g7ZO1fXPT1fom31mwncOxb5tO4urf72tasha2KRM1I=";
  };

  sourceRoot = ".";

  nativeBuildInputs = [
    unzip
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp hcp $out/bin
  '';
}
