#!/bin/bash

echo "What is your token?"
read token

#echo "Getting frames..."
#http GET localhost:8080/frames

echo "Getting player info..."
http GET localhost:8080/user "Authorization:Bearer ${token}"

echo "Uploading testbot..."
echo "So... Dillon hard coded in bot"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
echo "Clap"
http -f POST localhost:8080/user/upload "Authorization:Bearer ${token}" bot=testbot bot@../starter-kits/bot.tar.gz
