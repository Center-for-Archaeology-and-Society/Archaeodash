# update repository
git -C /mnt/storage/apps/CASRShinySrvr/Archaeodash pull;
# install package
docker exec -it -w /srv/shiny-server  archaeodash R -e 'devtools::install_local("/home/shiny/Archaeodash", force = T, dependencies = F)';
# restart container
docker restart archaeodash;

