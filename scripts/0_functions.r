# Reference evapotranspiration (ETo) via Hargreaves-Samani ---------------
# FAO-56 Hargreaves equation (Allen et al., 1998; Hargreaves & Samani,
# 1985). Needs only Tmax, Tmin, and extraterrestrial radiation (Ra) --
# Ra is calculated from latitude and day-of-year astronomically, so no
# measured solar radiation or wind speed input is required. This keeps
# the whole pipeline on Daymet, avoiding cross-product inconsistency
# with gridMET/PRISM.
#
# ETo (mm/day) = 0.0023 * (Tmean + 17.8) * sqrt(Tmax - Tmin) * Ra_mm
# where Ra_mm = 0.408 * Ra_MJ (converts extraterrestrial radiation from
# MJ m-2 day-1 to mm/day of equivalent evaporation)

#' Extraterrestrial radiation (Ra), FAO-56 Eq. 21-25
#' @param doy day of year (1-365/366)
#' @param lat_deg latitude in decimal degrees (positive = N hemisphere)
#' @return Ra in MJ m-2 day-1
calc_Ra <- function(doy, lat_deg) {
  lat_rad <- lat_deg * pi / 180
  
  dr    <- 1 + 0.033 * cos(2 * pi / 365 * doy)              # inverse rel. Earth-Sun distance
  delta <- 0.409 * sin(2 * pi / 365 * doy - 1.39)            # solar declination
  ws    <- acos(-tan(lat_rad) * tan(delta))                  # sunset hour angle
  Gsc   <- 0.0820                                             # solar constant, MJ m-2 min-1
  
  Ra <- (24 * 60 / pi) * Gsc * dr *
    (ws * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(ws) * 0 +
       cos(lat_rad) * cos(delta) * sin(ws))
  # (written with the standard grouping below for clarity/verification)
  Ra <- (24 * 60 / pi) * Gsc * dr *
    (ws * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(delta) * sin(ws))
  
  Ra
}

#' Hargreaves-Samani reference ETo
#' @param tmax_c,tmin_c daily max/min temperature, deg C
#' @param doy day of year
#' @param lat_deg site latitude, decimal degrees
#' @return ETo in mm/day
calc_ETo_hargreaves <- function(tmax_c, tmin_c, doy, lat_deg) {
  tmean_c <- (tmax_c + tmin_c) / 2
  trange  <- tmax_c - tmin_c
  trange[trange < 0] <- NA   # guards against bad data (Tmax < Tmin)
  
  Ra_MJ <- calc_Ra(doy, lat_deg)
  Ra_mm <- 0.408 * Ra_MJ
  
  0.0023 * (tmean_c + 17.8) * sqrt(trange) * Ra_mm
}

