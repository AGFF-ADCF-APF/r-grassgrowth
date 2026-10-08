# rocker/geospatial bringt sf/terra/tidyverse inkl. gdal/geos/proj bereits
# fertig kompiliert mit - erspart lange Bauzeiten fuer diese Pakete.
FROM rocker/geospatial:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
    nginx \
    cron \
    && rm -rf /var/lib/apt/lists/*

COPY deploy/install_packages.R /tmp/install_packages.R
RUN Rscript /tmp/install_packages.R

# cwebp: Kartenbilder als WebP statt PNG (27_plot_datenexplorer.R) - eigene
# Schicht nach den R-Paketen, damit deren Installation im Cache bleibt.
RUN apt-get update && apt-get install -y --no-install-recommends webp \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
# Code (../app) und Deploy-Skripte (automate.R, upload_svgs.R, lauf.sh,
# entrypoint.sh, nginx.conf) werden per docker-compose.yml eingebunden, nicht
# ins Image kopiert - ein Code-Deploy braucht so keinen Image-Neubau. Neu
# bauen nur noch bei neuen R-/System-Paketen.
# Persistente Verzeichnisse - im Betrieb per docker-compose.yml als Volumes
# ueberlagert, hier nur als Fallback fuer den allerersten Start.
RUN mkdir -p /app/outputs /app/ebenen_cache /geodata/meteoschweiz /opt/deploy
ENV GRASSGROWTH_GEODATA_DIR=/geodata/meteoschweiz
ENV GRASSGROWTH_ELEVATION_CACHE_FILE=/app/ebenen_cache/standorte_elevation.csv
ENV GRASSGROWTH_DEPLOY_DIR=/opt/deploy

EXPOSE 80
ENTRYPOINT ["/bin/bash", "/opt/deploy/entrypoint.sh"]
