# Want to make sure we are in soils and not random forrest or roadways...


dir <- getwd()

source(paste0(dir,"/Scripts/0_functions.r"))

packages <- c("tidyverse", "janitor", "sf", "tigris", "maptiles", "tidyterra", "terra")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) install.packages(packages[!installed_packages])
invisible(lapply(packages, library, character.only = TRUE))

options(tigris_use_cache = TRUE)

site_coords <- read.csv(paste0(dir, "/LocationCoords.csv")) %>%
  clean_names() %>%
  mutate(loc = str_trim(loc)) %>%
  filter(loc != "Shenandoah Valley")   # adjust column names if these differ

site_coords   # confirm loc/lat/long look right before mapping


## Figure 1: State-level overview map --------------------------------------

va_counties <- tigris::counties(state = "VA", cb = TRUE, year = 2022) %>%
  st_transform(4326)

site_sf <- st_as_sf(site_coords, coords = c("long", "lat"), crs = 4326)

fig1 <- ggplot() +
  geom_sf(data = va_counties, fill = "grey95", color = "grey70", linewidth = 0.2) +
  geom_sf(data = site_sf, color = "firebrick", size = 3) +
  ggrepel::geom_label_repel(
    data = site_coords,
    aes(x = long, y = lat, label = loc),
    size = 3.2, min.segment.length = 0
  ) +
  labs(title = "Virginia wheat variety trial locations") +
  theme_minimal() +
  theme(axis.title = element_blank(), panel.grid = element_blank())

fig1

ggsave(paste0(dir, "/Figures/site_overview_map.png"), fig1,
       width = 7, height = 6, dpi = 300)


## Figure 2: Zoomed satellite imagery per site -----------------------------
# maptiles pulls basemap tiles (ESRI World Imagery, no API key needed)
# for a small bounding box around each point, so you can visually
# confirm the coordinate actually falls on a field.

ZOOM_LEVEL <- 17   # higher = more zoomed in; 16-18 is usually field-scale
BUFFER_DEG <- 0.003   # ~300m bounding box around each point; shrink/grow as needed

get_site_satellite <- function(site_name, lat, lon, buffer = BUFFER_DEG, zoom = ZOOM_LEVEL) {
  bbox <- st_bbox(c(xmin = lon - buffer, xmax = lon + buffer,
                    ymin = lat - buffer, ymax = lat + buffer), crs = st_crs(4326))
  bbox_sf <- st_as_sfc(bbox)
  
  tiles <- get_tiles(bbox_sf, provider = "Esri.WorldImagery", zoom = zoom, crop = TRUE)
  tiles
}

satellite_tiles <- pmap(
  list(site_coords$loc, site_coords$lat, site_coords$long),
  ~ get_site_satellite(..1, ..2, ..3)
)
names(satellite_tiles) <- site_coords$loc


## Build a faceted-style figure -- one panel per site, point overlaid ------
# ggplot doesn't directly facet raster tiles from a list, so build each
# panel separately and combine with patchwork

# install.packages("patchwork")
library(patchwork)

site_panels <- map(seq_len(nrow(site_coords)), function(i) {
  site_name <- site_coords$loc[i]
  pt <- site_sf[i, ]
  
  ggplot() +
    geom_spatraster_rgb(data = satellite_tiles[[site_name]]) +
    geom_sf(data = pt, color = "red", size = 3, shape = 4, stroke = 1.5) +
    labs(title = site_name) +
    theme_void() +
    theme(plot.title = element_text(size = 10, hjust = 0.5))
})

fig2 <- wrap_plots(site_panels, ncol = 3) +
  plot_annotation(title = "Trial site locations: satellite imagery check")

fig2

ggsave(paste0(dir, "/Figures/site_satellite_check.png"), fig2,
       width = 10, height = 7, dpi = 300)
