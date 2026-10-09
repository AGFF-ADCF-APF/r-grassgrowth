#!/bin/bash
# Code-Deploy auf den Server, ohne Image-Neubau und ohne Neustart:
#   1. die genannten Dateien kopieren (R-Skripte nach app/, deploy/... nach deploy/)
#   2. nur frontend/*: JS/CSS im fertigen Datenexplorer austauschen
#      (frontend_tauschen.R) - keine Datenabfrage, kein Neuaufbau, Sekunden.
#      Sonst: den Datenexplorer im Schnellmodus neu erzeugen (lauf.sh seite) -
#      das laufende Jahr kommt aus dem Cache, sofern Daten und R-Rechencode
#      (ohne die @darstellung-Abschnitte) seit dem letzten Lauf unveraendert
#      sind.
#
# Aufruf aus dem Repo:  deploy/deploy.sh [Datei ...]
#   ohne Angabe: 27_plot_datenexplorer.R und frontend/* (Datenexplorer-JS/CSS)
#   Beispiel:    deploy/deploy.sh frontend/datenexplorer.js
#   Pfade mit Unterordner (z.B. frontend/) bleiben auf dem Server erhalten.
#
# Nach Aenderungen an deploy/entrypoint.sh: Container neu starten
# (docker compose up -d --force-recreate). An deploy/nginx.conf:
# docker exec agff-grassgrowth nginx -s reload. An Paketen
# (install_packages.R, Dockerfile): docker compose build && up -d.
set -euo pipefail
SERVER="${GRASSGROWTH_SERVER:-192.168.0.73}"
ZIEL="${GRASSGROWTH_ZIEL:-/opt/stacks/agff-apps/r-grassgrowth}"
CONTAINER="${GRASSGROWTH_CONTAINER_NAME:-agff-grassgrowth}"
cd "$(dirname "$0")/.."

if [ "$#" -eq 0 ]; then set -- 27_plot_datenexplorer.R frontend/*; fi
nur_frontend=1
for f in "$@"; do case "$f" in frontend/*) ;; *) nur_frontend=0 ;; esac; done
for f in "$@"; do
  [ -f "$f" ] || { echo "Datei fehlt: $f" >&2; exit 1; }
  case "$f" in
    deploy/*) scp -q "$f" "$SERVER:$ZIEL/deploy/" ;;
    */*)      ssh "$SERVER" "mkdir -p '$ZIEL/app/$(dirname "$f")'" && scp -q "$f" "$SERVER:$ZIEL/app/$f" ;;
    *)        scp -q "$f" "$SERVER:$ZIEL/app/" ;;
  esac
  echo "kopiert: $f"
done

if [ "$nur_frontend" -eq 1 ]; then
  echo "Nur Frontend: wird im fertigen Datenexplorer ausgetauscht (wartet, falls gerade ein Lauf laeuft) ..."
  ssh "$SERVER" "docker exec -i -w /app $CONTAINER flock -w 7200 /tmp/grassgrowth-lauf.lock Rscript -" < deploy/frontend_tauschen.R
  exit $?
fi

echo "Datenexplorer wird neu erzeugt (wartet, falls gerade ein anderer Lauf laeuft) ..."
ssh "$SERVER" "docker exec $CONTAINER bash -c '/bin/bash /opt/deploy/lauf.sh seite >> /var/log/grassgrowth-lauf.log 2>&1; rc=\$?; grep -E \"Schnellmodus|aus Cache\" /var/log/grassgrowth-lauf.log | tail -8; tail -1 /var/log/grassgrowth-lauf.log; exit \$rc'"
