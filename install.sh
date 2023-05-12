# update repository
git -C /home/rjbischo/app/apps/Archaeodash pull;
# install package
docker exec -it -w /home/shiny/Archaeodash casrshiny R -e 'devtools::install_local("/home/shiny/Archaeodash", force = T, dependencies = F)';
# restart container
docker restart casrshiny;

