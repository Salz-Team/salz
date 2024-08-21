{ stdenv
, lib
, fetchurl
, unzip
}:
let
  pname = "hcp";
  version = "0.4.0";
  sources = {
    "x86_64-linux" = {
      url = "https://releases.hashicorp.com/${pname}/${version}/${pname}_${version}_linux_amd64.zip";
      hash = "sha256-9g7ZO1fXPT1fom31mwncOxb5tO4urf72tasha2KRM1I=";
    };
    "aarch64-darwin" = {
      url = "https://releases.hashicorp.com/${pname}/${version}/${pname}_${version}_darwin_arm64.zip";
      hash = "sha256-JWrkJfio795WNnJawqPlkIK3mvlVQZqPtXlayLZrVxw=";
    };
  };
in stdenv.mkDerivation rec {
  inherit pname version;

  src = fetchurl sources."${stdenv.hostPlatform.system}";

  sourceRoot = ".";

  nativeBuildInputs = [
    unzip
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp hcp $out/bin
  '';

  meta = with lib; {
    platforms = with platforms; [ "x86_64-linux" "aarch64-darwin" ];
  };
}
