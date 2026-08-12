FROM rocker/shiny:4.6.1

RUN R -e "install.packages(c(\"shiny\", \"arrow\", \"tidyverse\", \"janitor\", \"readxl\", \"sf\", \"leaflet\", \"plotly\", \"RColorBrewer\"))"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD [\"/usr/bin/shiny-server\"]