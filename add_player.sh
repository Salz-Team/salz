#!/bin/bash

psql -c "insert into users values (default, 'mosiman', '0', 'bot1.sh', 'good');" salz
mc cp bot1.sh local/bots/bot1.sh
psql -c "insert into users values (default, 'japorized', '0', 'bot2.sh', 'good');" salz
mc cp bot2.sh local/bots/bot2.sh
psql -c "insert into users values (default, 'lugarun', '0', 'bot3.sh', 'good');" salz
mc cp bot3.sh local/bots/bot3.sh

