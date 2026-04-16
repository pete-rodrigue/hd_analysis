library(sf)
library(leaflet)
library(dplyr)

# ── 1. Fetch data directly from the DC GIS REST API as GeoJSON ────────────────
url <- paste0(
  "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/",
  "Demographic_WebMercator/MapServer/15/query",
  "?where=1%3D1&outFields=*&f=geojson"
)

tracts <- st_read(url, quiet = TRUE)

# ── 2. Calculate % white (the API already has BUQ001P, but let's derive it
#       ourselves from BUQ001 / BUB01 to be explicit) ─────────────────────────
tracts <- tracts |>
  mutate(
    pct_white = case_when(
      BUB01 > 0 ~ round((BUQ001 / BUB01) * 100, 1),
      TRUE      ~ NA_real_
    )
  )

# ── 3. Reproject to WGS84 for leaflet ─────────────────────────────────────────
tracts <- st_transform(tracts, 4326)

# ── 4. Build color palette ────────────────────────────────────────────────────
pal <- colorNumeric(
  palette = "Blues",
  domain  = tracts$pct_white,
  na.color = "#cccccc"
)

# ── 5. Interactive map ────────────────────────────────────────────────────────
leaflet(tracts) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor   = ~pal(pct_white),
    fillOpacity = 0.3,
    color       = "white",
    weight      = 0.8,
    label       = ~paste0(
      "Tract ", TRACT_NAME, "<br>",
      "White: ", BUQ001, "<br>",
      "Total: ", BUB01, "<br>",
      "% White: ", pct_white, "%"
    ) |> lapply(htmltools::HTML),
    highlight = highlightOptions(
      weight      = 2,
      color       = "#333",
      fillOpacity = 0.95,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    pal      = pal,
    values   = ~pct_white,
    title    = "% White (1940)",
    position = "bottomright",
    labFormat = labelFormat(suffix = "%")
  )
