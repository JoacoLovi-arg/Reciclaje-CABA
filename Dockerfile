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

# Detecta el codename de la distro (ej: jammy, noble, focal) y arma el repo binario de Posit
RUN CODENAME=$(. /etc/os-release && echo "$VERSION_CODENAME") \
    && echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/${CODENAME}/latest'))" >> /usr/lib/R/etc/Rprofile.site \
    && echo "Usando repo: https://packagemanager.posit.co/cran/__linux__/${CODENAME}/latest"

# Instala paquetes y aborta el build si alguno falla
RUN R -e "install.packages(c('shiny','tidyverse','janitor','readxl','sf','leaflet','plotly','RColorBrewer')); \
    missing <- setdiff(c('shiny','tidyverse','janitor','readxl','sf','leaflet','plotly','RColorBrewer'), rownames(installed.packages())); \
    if(length(missing) > 0) stop('Faltan paquetes: ', paste(missing, collapse=', '))"

COPY . /srv/shiny-server/
EXPOSE 3838