FROM rocker/shiny-verse:4.3.2

LABEL description="This is the docker container for Archaeodash"

COPY . /srv/shiny-server

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
    MASS

COPY . /srv/shiny-server

WORKDIR /srv/shiny-server


RUN R -e "devtools::install_local('.',force = T, dependencies = F)"
