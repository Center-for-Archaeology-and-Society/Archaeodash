#!/bin/bash
echo "updating repository";

ARG1=${1};
echo "selected repository is $ARG1";
lower="${ARG1,,}";

echo "Pulling git repository"
git -C /mnt/storage/apps/CASRShinySrvr/$ARG1 pull;
echo "installing package"
docker exec -it -w /srv/shiny-server  $lower R -e 'devtools::install_local(".", force = T, dependencies = F)';
echo "restarting container"
docker restart $lower;
echo "completed"

