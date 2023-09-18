#!/bin/bash

# script to build archaeodash docker container

docker build -t archaeodash Archaeodash/docker

docker run \
--name=archaeodash \
-d \
-v ./Archaeodash:/srv/shiny-server/Archaeodash \
-w /srv/shiny-server/Archaeodash \
--publish=13838:3838 \
--restart unless-stopped \
archaeodash

