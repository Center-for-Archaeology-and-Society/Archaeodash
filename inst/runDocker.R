#! bash

# script to build archaeodash docker container

docker build -t archaeodash ./docker

docker run \
--name=arcaeodash \
-d \
--publish=3838:3838 \
--restart unless-stopped \
archaeodash
