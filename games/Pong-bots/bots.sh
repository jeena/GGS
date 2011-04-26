#!/bin/sh

for (( i = 0; i < 2; i++ )); do
    ruby pong-bot.rb &
done
