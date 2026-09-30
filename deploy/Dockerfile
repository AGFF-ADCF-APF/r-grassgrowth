# rocker/geospatial bringt sf/terra/tidyverse inkl. gdal/geos/proj bereits
# fertig kompiliert mit - erspart lange Bauzeiten fuer diese Pakete.
FROM rocker/geospatial:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    cron \
    && rm -rf /var/lib/apt/lists/*

COPY deploy/install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R

WORKDIR /app
COPY app/ /app/
COPY deploy/automate.R /app/automate.R
COPY deploy/upload_svgs.R /app/upload_svgs.R

# Persistente Verzeichnisse - werden im Betrieb per docker-compose.yml als
# Volumes ueberlagert (siehe dort), hier nur als Fallback/fuer den
# allerersten Start ohne Volume.
RUN mkdir -p /app/outputs /app/ebenen_cache /geodata/meteoschweiz
ENV GRASSGROWTH_GEODATA_DIR=/geodata/meteoschweiz
ENV GRASSGROWTH_ELEVATION_CACHE_FILE=/app/ebenen_cache/standorte_elevation.csv

COPY deploy/nginx.conf /etc/nginx/nginx.conf
COPY deploy/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 80
ENTRYPOINT ["/entrypoint.sh"]
