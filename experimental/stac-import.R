install.packages("sf")
install.packages("rstac")
install.packages("terra")
library(rstac)

stac_source <- rstac::stac(
  "https://data.geo.admin.ch/api/stac/v1"
)
stac_source

rstac::get_request(stac_source)

rstac::stac_search(
  q = stac_source,
  collections = "usgs-lcmap-conus-v13",
  datetime = "2021-01-01/2021-12-31",
  limit = 999
)

available_collections <- rstac::get_request(collections_query)


#"https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn-precip"
#{"stac_version":"1.0.0","id":"ch.meteoschweiz.ogd-smn-precip","title":"Automatic precipitation stations – Measured values","description":"Precipitation – per station – every 10 minutes ('t'), hourly ('h'), daily ('d'), monthly ('m') and yearly ('y') – since midnight ('now'), from the current year up to yesterday ('recent'), since the beginning of the measurement in ten-year increments ('historical'). If you require hourly, daily, monthly or yearly values, we strongly recommend that you download the corresponding resolution.","summaries":{},"extent":{"spatial":{"bbox":[[6.258281,45.853914,10.463031,47.751944]]},"temporal":{"interval":[["2025-06-25T04:00:18.057143Z","2025-06-25T04:00:18.097279Z"]]}},"providers":[{"name":"Federal Office of Meteorology and Climatology MeteoSwiss","roles":["producer","licensor"],"url":"https://www.meteoswiss.admin.ch/"}],"license":"CC-BY","created":"2025-03-10T06:28:10.215701Z","updated":"2025-06-25T09:07:36.233283Z","links":[{"rel":"self","href":"https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn-precip"},{"rel":"root","href":"https://data.geo.admin.ch/api/stac/v1/"},{"rel":"parent","href":"https://data.geo.admin.ch/api/stac/v1/"},{"rel":"items","href":"https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn-precip/items"},{"rel":"assets","href":"https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn-precip/assets"},{"rel":"alternate","title":"STAC Browser","type":"text/html","href":"https://data.geo.admin.ch/browser/index.html#/collections/ch.meteoschweiz.ogd-smn-precip"},{"href":"https://www.geocat.ch/geonetwork/srv/eng/catalog.search#/metadata/45f279d5-0289-47b5-8762-a610365e55b6","rel":"describedby","title":"Metadata"},{"href":"https://opendatadocs.meteoswiss.ch/a-data-groundbased/a2-automatic-precipitation-stations","rel":"about","title":"Further information"}],"crs":["http://www.opengis.net/def/crs/OGC/1.3/CRS84"],"itemType":"Feature","assets":{"ogd-smn-precip_meta_parameters.csv":{"type":"text/csv","href":"https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_parameters.csv","description":"This a collection asset for ogd-smn-precip_meta_parameters.csv","created":"2025-03-28T12:16:07.661880Z","updated":"2025-03-28T12:16:08.333807Z","file:checksum":"1220bb448e04668485d4b2f5290639099d2d32e885e61309612decf22f2295dbae24"},"ogd-smn-precip_meta_datainventory.csv":{"type":"text/csv","href":"https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_datainventory.csv","description":"This a collection asset for ogd-smn-precip_meta_datainventory.csv","created":"2025-03-28T12:16:07.661904Z","updated":"2025-06-25T04:13:33.565022Z","file:checksum":"1220c9ac2538ef1e127122e5b6e277a1b7707c46e38a966d2499126709f00b0b7d55"},"ogd-smn-precip_meta_stations.csv":{"type":"text/csv","href":"https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_stations.csv","description":"This a collection asset for ogd-smn-precip_meta_stations.csv","created":"2025-03-28T12:16:09.061382Z","updated":"2025-06-18T04:13:36.137898Z","file:checksum":"122058dd79f8e819df840e144f76f3cbfce95e6427c627b067563f3793f370ec76b8"}},"type":"Collection"}

library(ggswissmaps)
library(dplyr)
data(shp_sf)

chmap

chmap <- shp_sf[["g1k15"]] %>%
  filter(KTNR %in% c(2)) |>
  st_as_sfc() |>
  sf::st_sfc(crs = 21781) |>
  sf::st_transform(crs = 'WGS84')
str(chmap)

sf::st_geometry(chmap) |> plot()

chmap_bbox <- chmap |>
  sf::st_transform(4326) |>
  sf::st_bbox()

stac_query <- rstac::stac_search(
  q = stac_source,
  collections = "ch.meteoschweiz.ogd-smn-precip",
  bbox = chmap_bbox,
  datetime = "2025-01-01/2025-06-25"
)
executed_stac_query <- rstac::get_request(stac_query)
executed_stac_query
