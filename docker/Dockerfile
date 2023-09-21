FROM rocker/shiny-verse:4.3.1

LABEL description="This is the docker container for Archaeodash"

RUN apt-get update

RUN  apt-get install -y --no-install-recommends \
	vim \
	git \
	libgl1-mesa-dev \
	libglu1-mesa-dev

RUN install2.r --error --skipinstalled \
    cluster \
    cowplot \
    DataExplorer \
    dendextend \
    dplyr \
    DT \
    factoextra \
    ggplot2 \
    magrittr \
    mice \
    plotly \
    randomForest \
    ranger \
    rio \
    shiny \
    shinydashboard \
    shinythemes \
    stats \
    tidyselect \
    ICSNP \
    shinyjs \
    bslib \
    janitor \
    devtools \
    candisc

RUN git clone -b dev https://github.com/Center-for-Archaeology-and-Society/Archaeodash.git /srv/shiny-server/Archaeodash

RUN R -e "devtools::install_local('srv/shiny-server/Archaeodash',force = T, dependencies =F)"
