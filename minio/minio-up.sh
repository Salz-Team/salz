#!/usr/bin/env bash
if ! test -d "$STATEDIR"
then
    echo "$STATEDIR folder not found -- are you running this from the repository root?"
    exit 1
fi

mkdir -p "$MC_CONFIG_DIR"
cat <<EOF > "$MC_CONFIG_DIR/config.json"
{
	"version": "10",
	"aliases": {
		"local": {
			"url": "http://$MINIO_ENDPOINT",
			"accessKey": "$MINIO_ROOT_USER",
			"secretKey": "$MINIO_ROOT_PASSWORD",
			"api": "S3v4",
			"path": "auto"
		}
	}
}
EOF


minio server --json "$STATEDIR/minio"
