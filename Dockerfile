FROM rocker/shiny:4.6.1

# Dependencias de sistema para sf, tidyverse, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    && rm -rf /var/lib/apt/lists/*

# Detecta el codename y la ruta real de R, y configura el repo binario de Posit
RUN CODENAME=$(grep -oP '(?<=^VERSION_CODENAME=).+' /etc/os-release) \
    && RPROFILE="$(R RHOME)/etc/Rprofile.site" \
    && echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/${CODENAME}/latest'))" >> "${RPROFILE}"

RUN R -e "install.packages(c('shiny','tidyverse','janitor','readxl','sf','leaflet','plotly','RColorBrewer')); \
    missing <- setdiff(c('shiny','tidyverse','janitor','readxl','sf','leaflet','plotly','RColorBrewer'), rownames(installed.packages())); \
    if(length(missing) > 0) stop('Faltan paquetes: ', paste(missing, collapse=', '))"

COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server

EXPOSE 3838

# Corre la app directamente (sin shiny-server), escuchando en todas las interfaces
ENV SHINY_HOST=0.0.0.0

CMD ["R", "-e", "options(shiny.host=Sys.getenv('SHINY_HOST'), shiny.port=as.numeric(Sys.getenv('PORT', 3838))); shiny::runApp('/srv/shiny-server')"]