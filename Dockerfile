FROM rocker/shiny-verse:4.4.3

LABEL description="This is the docker container for Archaeodash"

COPY . /srv/shiny-server

WORKDIR /srv/shiny-server

RUN apt-get update

RUN  apt-get install -y \
    vim \
    git \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libsodium-dev

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
    MASS \
    candisc \
    RMySQL \
    DBI \
    sodium \
    bsicons \
    markdown \
    umap

RUN R -e 'devtools::install_local(".", force = T, dependencies = F)'
