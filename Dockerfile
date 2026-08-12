FROM rocker/shiny:4.6.1

RUN R -e "install.packages(c('shiny', 'tidyverse', 'janitor', 'readxl', 'sf', 'leaflet', 'plotly', 'RColorBrewer'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838