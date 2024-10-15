{ pkgs, lib, config, inputs, ... }:

{
  env.GREET = "devenv";
  env.VAULT_ORG_ID = "25ee0d1b-4e14-42f8-bf34-d3191ff704b7";
  env.VAULT_PROJECT_ID = "3196ec06-6d59-4fc4-affa-fa3fdd12cd1c";
  env.VAULT_APP_NAME = "salz";

  # PGHOST, PGPORT already set (probably by devenv?)
  env.PGUSER = "salz";
  env.PGPASSWORD = "superdupersecret";
  env.PGDATABASE = "salz";
  env.PGREQUIREAUTH = "false";

  languages.javascript.enable = true;
  languages.typescript.enable = true;

  packages = [
    pkgs.git
    pkgs.graphviz
    pkgs.devenv
    (pkgs.callPackage ./nix/hcp.nix {})
    pkgs.jsonschema

    pkgs.nodePackages.npm
    pkgs.nodePackages.typescript-language-server
    pkgs.nodePackages.svelte-language-server
  ];

  services.postgres = {
    enable = true;
    initialDatabases = [{ name = "salz"; schema = ./db/init.sql; }];
    initialScript = ''
      CREATE ROLE salz WITH LOGIN PASSWORD 'superdupersecret';
      ALTER ROLE salz WITH SUPERUSER; -- TODO CHANGE THIS IN PROD LOL
    '';
    listen_addresses = "127.0.0.1";
  };

  services.minio = {
    enable = true;
    consoleAddress = "127.0.0.1:9111";
    listenAddress = "127.0.0.1:9110";
    buckets = ["salz"];
  };
  services.caddy.enable = true;

  languages.go.enable = true;
}
