#!/usr/bin/env bash

psql -c "select * from users order by random() limit 2;" salz --csv | sed '1d' | awk -F',' '{ print $1 }'
