# Installiert nur, was in der rocker/geospatial-Basis (bereits sf/terra/
# tidyverse/gdal/geos/proj) noch fehlt - vermeidet ein erneutes Kompilieren
# von sf/terra (dauert sonst sehr lange).
benoetigt <- c(
  "ggrepel", "htmltools", "htmlwidgets", "jsonlite", "packcircles",
  "plotly", "png", "RColorBrewer", "base64enc", "remotes", "RCurl",
  "dplyr", "ggplot2", "tidyr", "scales", "sf", "terra"
)
fehlend <- benoetigt[!vapply(benoetigt, requireNamespace, logical(1), quietly = TRUE)]
if (length(fehlend) > 0) {
  install.packages(fehlend, repos = "https://cloud.r-project.org")
}

# ggswissmaps ist nicht auf CRAN - nur ueber GitHub verfuegbar.
if (!requireNamespace("ggswissmaps", quietly = TRUE)) {
  remotes::install_github("gibonet/ggswissmaps")
}
