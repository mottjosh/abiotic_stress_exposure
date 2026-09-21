
## sinusodial interpolation from daily min to max temp

sine.approx2 <- function(tmin, tmax, yl = 48) {
  if(!length(tmin) == length(tmax)) stop("length tmin must equal length tmax")

  b <- (2*pi)/24  # period = 24 hours
  phi <- pi/2 # horizontal shift
  xout1 <- seq(0,12,length.out = yl)                        #temp rising
  xout2 <- seq(12.5,24,length.out = yl)                   #temp falling
  minmax <- cbind(tmin, tmax)
  maxmin <- cbind(tmax[-length(tmax)],tmin[-1])
  ydayL <- list()
  xdayL <- list()
  day <- 0
  
  if(length(tmin) >= 2){
    for(i in 2:length(tmin)){
      #i = 2
      up <- c(tmin[i-1], tmax[i-1])
      down <- c(tmax[i-1], tmin[i])
      xup <- xout1 + day * 24
      xdown <- xout2 + day * 24
      yhatup <- diff(up)/2 * sin(b*xup-phi) + mean(up) #up
      yhatdown <- diff(down)/2 * sin(b*xdown+phi) + mean(down) #down
      
      xdayL[[i-1]] <- c(xup, xdown)
      ydayL[[i-1]] <- c(yhatup, yhatdown)
      day <- 1 + day
    }
    # last up addtion
    up <- c(tmin[i], tmax[i])
    xup <- xout1 + day * 24
    yhatup <- diff(up)/2 * sin(b*xup-phi) + mean(up) #up
    xdayL[[i]] <- c(xup)
    ydayL[[i]] <- c(yhatup)
    
    return(list(x = unlist(xdayL), y = unlist(ydayL)))
  }
  else
  {
    
    up <- c(tmin, tmax)
    xup <- xout1 + day * 24
    yhatup <- diff(up)/2 * sin(b*xup-phi) + mean(up) #up
    xdayL <- c(xup)
    ydayL <- c(yhatup)
    
    return(list(x = unlist(xdayL), y = unlist(ydayL)))
  }
}

point_to_dday <- function(x)(
  (x * 15)/1440
) 


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

