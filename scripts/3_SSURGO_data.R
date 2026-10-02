
dir <- getwd()

source(paste0(dir,"/Scripts/1_data_setup.r"))


# Import SSURGO Data for AWC ----------------------------------------------


ROOTING_DEPTH_CM <- 120 #1.2 m matches Lollato et al 2020

## Get MUKEY for each point ----

get_mukey <- function(lat,long){
  pt <- st_sfc(st_point(c(long,lat)), crs = 4326)
  res <- SDA_spatialQuery(pt, what = 'mukey')
  res$mukey[1]
}

site_coords <- coordinates %>%
  rowwise() %>%
  mutate(mukey = get_mukey(lat, long)) %>%
  ungroup()

get_awhc <- function(mukey, rooting_depth_cm = ROOTING_DEPTH_CM) {
  
  q <- sprintf("
    SELECT c.cokey, c.comppct_r, h.hzdept_r, h.hzdepb_r, h.awc_r
    FROM component c
    INNER JOIN chorizon h ON c.cokey = h.cokey
    WHERE c.mukey = '%s'
    ORDER BY c.comppct_r DESC
  ", mukey) #517092 == blacksburg
  
  df <- SDA_query(q)
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  
  dom_cokey <- df$cokey[which.max(df$comppct_r)]
  
  df %>%
    filter(cokey == dom_cokey, hzdept_r < rooting_depth_cm, !is.na(awc_r)) %>%
    mutate(
      hz_bottom_clipped = pmin(hzdepb_r, rooting_depth_cm),
      thickness_cm = hz_bottom_clipped - hzdept_r
    ) %>%
    filter(thickness_cm > 0) %>%
    summarise(awhc_mm = sum(awc_r * thickness_cm * 10, na.rm = TRUE)) %>%
    pull(awhc_mm)
}

site_coords <- site_coords %>%
  rowwise() %>%
  mutate(awhc_mm = get_awhc(mukey)) %>%
  ungroup()

site_coords   ### looks reasonable enough for now... will have to double check later


obsclim <- obsclim %>%
  left_join(site_coords %>% dplyr::select(loc, awhc_mm), by = c("site" = 'loc'))

obsclim %>% distinct(site, awhc_mm) # may consider taking several points and averaging within each site

