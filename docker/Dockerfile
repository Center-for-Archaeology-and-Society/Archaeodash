FROM rocker/rocker-verse:4.3.1

LABEL description="This is the docker container for Archaeodash"

RUN apt-get update

RUN  apt-get install -y --no-install-recommends \
	vim

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
    janitor


