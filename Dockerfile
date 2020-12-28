# DOCKER FILE FOR POPHELPERSHINY
# 2021 Roy Francis

FROM rocker/shiny:4.0.0
LABEL Description="Docker image for R package pophelperShiny"
LABEL Maintainer="roy.m.francis@outlook.com"

RUN apt-get update -y \
  && apt-get install --no-install-recommends -y \
  libfreetype6-dev \
  libcurl4-openssl-dev \
  libssl-dev \
  libudunits2-dev \
  libxml2-dev \
  libgeos-dev \
  libfontconfig1-dev \
  gdal-bin \
  libgdal-dev \
  libcairo2-dev \
  libxt-dev \
  libnlopt-dev \
  mesa-common-dev \
  libglu1-mesa-dev \
  libx11-dev \
  && rm -rf /var/lib/apt/lists/*
  
# install r packages
RUN Rscript -e 'install.packages(c("ggplot2","gridExtra","label.switching","tidyr","remotes","colourpicker","DT","highcharter","htmlwidgets","magrittr","markdown","RColorBrewer","shiny","shinyAce","shinyBS","shinythemes","shinyWidgets","viridisLite","writexl"),repos = "http://cran.us.r-project.org");'

# install pophelper package from github
RUN Rscript -e 'remotes::install_github("royfrancis/pophelper")'
RUN Rscript -e 'remotes::install_github("royfrancis/pophelperShiny")'

RUN cd /srv/shiny-server && \
    ln -s /usr/local/lib/R/site-library/pophelperShiny/app && \
    sudo chown -R shiny:shiny /srv/shiny-server/app
    
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app/', host = '0.0.0.0', port = 8787)"]
