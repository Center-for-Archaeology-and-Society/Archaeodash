#!/bin/bash

# script to build archaeodash docker container

docker build -t archaeodashbeta Archaeodashbeta/docker

docker run \
--name=archaeodashbeta \
-d \
-v ./Archaeodashbeta:/srv/shiny-server/Archaeodash \
-w /srv/shiny-server/Archaeodash \
--publish=23838:3838 \
--restart unless-stopped \
archaeodashbeta

