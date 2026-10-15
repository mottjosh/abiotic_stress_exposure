dir <- getwd()

source(paste0(dir,"/Scripts/0_functions.r"))

# Packages --------------------------------------------------------------
packages <- c(
  # general
  "tidyverse", 'data.table', 'janitor', 
  # ssurgo and mapping
  'daymetr', "sf", "tigris", "maptiles", "tidyterra", "terra", 'soilDB',
  # plotting
  'ggthemes', 'patchwork', 'latex2exp', 'viridis', 'GGally', 'ggpubr', 'scico',
  'geomtextpath')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Weather Data import --------------------------------------------------------------

## `daymetr` to collect weather data 

coordinates <- data.frame(loc = c("Blacksburg", "Blackstone", "Holland",
                                  "Painter", "Orange", "Warsaw"),
                          lat = c(37.199312, 37.087035, 36.683618, 37.588189,
                                  38.224127, 37.987932),
                          long = c(-80.567631, -77.972754, -76.764003, -75.822092,
                                   -78.119875, -76.779704))

 # write.csv(coordinates, file="LocationCoords.csv", row.names=FALSE)

cat('checking for checking for daymet data (Daily Weather)')

if (!file.exists("Data/daymet_data.RDS")) {
daymet_data <- download_daymet_batch(
  file_location = 'LocationCoords.csv',
  start = 1990,
  end = 2023,
  internal = TRUE,
  simplify=TRUE)

saveRDS(daymet_data, "Data/daymet_data.RDS")

message("Weather data pulled")
} else {
  
  message("Weather data exists. Skipping...")
  
  daymet_data <- readRDS("Data/daymet_data.RDS")
}

str(daymet_data)

# Every column except measurement/value identifies a row 
id_cols <- setdiff(names(daymet_data), c("measurement", "value"))

clim_data <- reshape(as.data.frame(daymet_data),
                     direction = "wide",
                     idvar     = id_cols,
                     timevar   = "measurement",
                     v.names   = "value")

# reshape() names the new columns "value.<measurement>", so strip that prefix
names(clim_data) <- sub("^value\\.", "", names(clim_data))

# Rename using a lookup vector (old = new)
new_names <- c(tmax..deg.c.  = "tmax_c",
               tmin..deg.c.  = "tmin_c",
               prcp..mm.day. = "prcp_mm",
               yday          = "doy")
hit <- names(clim_data) %in% names(new_names)
names(clim_data)[hit] <- new_names[names(clim_data)[hit]]

# Select columns 1:7 plus the three you want (drops vapor pressure and solar radiation)
clim_data <- clim_data[, c(names(clim_data)[1:7], "prcp_mm", "tmax_c", "tmin_c")]
rownames(clim_data) <- NULL   # reshape() leaves odd row names behind


# adding dates and season

clim_data$origin <- as.Date(paste0(clim_data$year, "-01-01"),tz = "UTC") - days(1)

clim_data$date <- as.Date(clim_data$doy, format="%j", origin = clim_data$origin)

# may not be necessary but parsing dates to month and day to have more accurate season starts

clim_data$day <- as.numeric(format(clim_data$date, format = "%d"))
clim_data$month <- as.numeric(format(clim_data$date, format = "%m"))


# x_test <- c(1:400) # give it a selected vector, do they have to be in sequence? 
# findInterval(x_test, c(4, 5, 66))

md <- clim_data$month * 100 + clim_data$day   # e.g., March 22 -> 322, Dec 5 -> 1205
clim_data$season <- c("winter", "spring", "summer", "autumn", "winter")[
  findInterval(md, c(101, 322, 622, 922, 1222))
]

# clean data by planting and harvest dates "observed climate".. we can add two week prior to planting as well

#adds harvest year
clim_data$harvyr <- ifelse(clim_data$month %in% c(9,10,11,12), 
                           clim_data$year + 1, clim_data$year)


#test line to make sure date was added correctly
clim_data[clim_data$date == "1999-01-01",] # works
clim_data[clim_data$date == "1998-11-01",] # works


fulldate <- read.csv("Data/LocPlantHarvDate.csv") # manually inserted missing harvest dates NEED to go back and change


fulldate$Planting.Date <- as.Date(fulldate$Planting.Date, "%m/%d/%Y")
fulldate$Harvest.Date <- as.Date(fulldate$Harvest.Date, "%m/%d/%Y")

str(fulldate)
tail(fulldate)

## need to clean up the names, remove Shenandoah and remove space after warsaw
unique(fulldate$Location) # extra space in Warsaw

final_locs <- c("Blacksburg", "Blackstone", "Holland", "Orange" ,"Painter", "Warsaw")

fulldate$Location <- gsub("\\s+$", "", fulldate$Location)
fulldate <- fulldate[fulldate$Location %in% final_locs, ] 

unique(fulldate$Location)

locdate <- list()

# loop trims data by planting and harvest based on location.
# updated to include the two weeks prior to planting
for (i in 1:length(fulldate$Location)){
  # i = 1
  loc <- fulldate$Location[[i]]
  locnum <- fulldate$locnum[[i]]
  
  a <- fulldate$Planting.Date[[i]]
  a2 <- fulldate$Planting.Date[[i]] - 14
  b <- fulldate$Harvest.Date[[i]]
  dates <- c(seq(a2, b, by = "days"))
  
  x <- list()
  x$date <- paste0(as.Date(dates)) 
  x$plant_date <- rep(as.Date(a), length(dates)) 
  # x$preplant_date <- rep(as.Date(a2), length(dates))
  x$site <- rep(loc, length(dates))
  x$locnum <- rep(locnum, length(dates))
  
  locdate[[i]] <- cbind.data.frame(x)
  
}

locplantdate <- as.data.frame(do.call(rbind, locdate))
locplantdate$date <- as.Date(locplantdate$date)

# remove days with no wheat by location from full weather data, except for preplant date

obsclim <- merge(locplantdate, clim_data, by.x = c('site','date'), 
                 by.y = c('site', 'date'), all.x = TRUE)

obsclim <- obsclim[order(obsclim$locnum, obsclim$date, obsclim$season),]  

obsclim$gdd <- with(obsclim, ifelse(((tmax_c + tmin_c) / 2) - 0 < 0, 0, 
                      (((tmax_c + tmin_c) / 2) - 0)))

gdd_after_planting <- with(obsclim, ifelse(date >= plant_date, gdd, 0))  

obsclim$cumu_gdd <- ave(gdd_after_planting, obsclim$site, obsclim$harvyr, FUN = cumsum) 

obsclim$season <- as.factor(obsclim$season)
obsclim$locnum <- as.numeric(obsclim$locnum)

# write.csv(obsclim, "Data/climbyloc.csv") ###
obsclim <- read_csv("Data/climbyloc.csv")

# Wheat Variety Data Import --------------------------------------------------------------

wht_data <- read.csv("Data/whtbyloc.csv")

wht_data %>% colnames()  

# site names need to match for merge
unique(wht_data$LOC)

codes <- list(
  Blacksburg = c("BLAVA", "BLACKSBURG", "BB"),
  Blackstone = c("BLSVA", "BLACKSTONE", "BS"),
  Holland    = c("HOLVA", "HOLLAND", "HO"),
  Orange     = c("ORGVA", "ORANGE", "OR"),
  Painter    = c("PNTVA", "PAINTER", "PT"),
  Warsaw     = c("WARVA", "WARSAW", "WR")
)

lookup <- setNames(rep(names(codes), lengths(codes)), unlist(codes))

clean_wht_data <- wht_data
clean_wht_data$LOC <- unname(lookup[trimws(clean_wht_data$LOC)])

unique(wht_data$LOC[is.na(clean_wht_data$LOC)]) # to be removed... 

clean_wht_data <- clean_wht_data[!is.na(clean_wht_data$LOC), ]
names(clean_wht_data)[names(clean_wht_data) == "LOC"] <- "site"
names(clean_wht_data)[names(clean_wht_data) == "year"] <- "harvyr"

str(clean_wht_data)
# quick check for random stuff in the data
yld <- clean_wht_data$yield
unique(yld[is.na(suppressWarnings(as.numeric(yld))) & !is.na(yld)]) # need to check on these '#VALUE' and #REF

#test weight
tswt <- clean_wht_data$tstwt
unique(tswt[is.na(suppressWarnings(as.numeric(tswt))) & !is.na(tswt)])
# tswt[is.na(suppressWarnings(as.numeric(tswt)))] # force the value to become a numeric and count the NAs

#heading date
hd <- clean_wht_data$HD
unique(hd[is.na(suppressWarnings(as.numeric(hd))) & !is.na(hd)])


yld_cols <- c('yield', 'tstwt', 'HD', 'ht')
clean_wht_data[yld_cols] <- lapply(clean_wht_data[yld_cols], as.numeric)

# TODO: Clean up yield

# filter out varieties in the trials for less than 3 years

yearsN <- tapply(clean_wht_data$harvyr, clean_wht_data$ID,
                 function(z) length(unique(z)))
years_in_trial <- data.frame(ID = names(yearsN), yearsN = as.vector(yearsN))
years_in_trial <- years_in_trial[years_in_trial$yearsN >= 3, ]

keep_ids <- names(yearsN)[yearsN >= 3]
clean_wht_data <- clean_wht_data[clean_wht_data$ID %in% keep_ids, ]
clean_wht_data$ID <- as.factor(clean_wht_data$ID)
clean_wht_data$site <- as.factor(clean_wht_data$site)
clean_wht_data <- clean_wht_data[clean_wht_data$yield > 0 & clean_wht_data$yield < 200, ]
clean_wht_data <- subset(clean_wht_data, select = -c(X.1, X))

summary(clean_wht_data)
# str(clean_wht_data)


# Import SSURGO Data for AWC ----------------------------------------------


ROOTING_DEPTH_CM <- 120 #1.2 m matches Lollato et al 2020

## Get MUKEY for each point ----

get_mukey <- function(lat,long){
  pt <- st_sfc(st_point(c(long,lat)), crs = 4326)
  res <- SDA_spatialQuery(pt, what = 'mukey')
  res$mukey[1]
}

get_awhc <- function(mukey, rooting_depth_cm = ROOTING_DEPTH_CM) {
  
  # example 517092, blacksburg 
  # mukey <- 517092
  q <- sprintf("
    SELECT c.cokey, c.comppct_r, h.hzdept_r, h.hzdepb_r, h.awc_r
    FROM component c
    INNER JOIN chorizon h ON c.cokey = h.cokey
    WHERE c.mukey = '%s'
    ORDER BY c.comppct_r DESC
  ", mukey)
  
  df <- SDA_query(q)
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  
  dom_cokey <- df$cokey[which.max(df$comppct_r)] # select the dominant soil series
  
  df <- df[df$cokey == dom_cokey & df$hzdept_r < rooting_depth_cm & !is.na(df$awc_r), ]
  df$hz_bottom_clipped <- pmin(df$hzdepb_r, rooting_depth_cm)
  df$thickness_cm <- df$hz_bottom_clipped - df$hzdept_r
  
  pos <- which(df$thickness_cm > 0)
  awhc_mm <- sum(df$awc_r[pos] * df$thickness_cm[pos] * 10, na.rm = TRUE)
  
  return(awhc_mm)
}


site_coords <- coordinates
site_coords$mukey <- mapply(get_mukey, site_coords$lat, site_coords$long)
site_coords$awhc_mm <- mapply(get_awhc, site_coords$mukey)


site_coords   ### looks reasonable enough for now... will have to double check later


obsclim <- merge(obsclim, site_coords[, c("loc", "awhc_mm")],
                 by.x = "site", by.y = "loc", all.x = TRUE)


# Evapotranspiration (Hargreaves-Samani) -----------------------------------------------------


## we will start with this equation but sure there are better options
## need to consult with the plant physiology people

## https://www.fao.org/4/x0490e/x0490e07.htm#solar%20radiation

##https://www.fao.org/4/x0490e/x0490e00.htm

obsclim$eto_mm <- mapply(calc_ETo_hargreaves, obsclim$tmax_c, obsclim$tmin_c, obsclim$doy, obsclim$latitude)

summary(obsclim$eto_mm)   # should be roughly 0-8 mm/day for this region/season range,
# negative or NA values indicate a join/data problem


str(obsclim)

# ggplot(obsclim[obsclim$harvyr == 2021 & !is.na(obsclim$eto_mm),], 
#        aes(x = date, y = eto_mm)) +
#   geom_line() +
#   facet_wrap(~site) +
#   labs(title = "Daily ETo (Hargreaves), 2021",
#        y = "ETo (mm/day)") +
#   theme_classic()

# ggsave("Figures/ET0.png", plot = last_plot(), width = 8, height = 5, unit = "in")

# Atmospheric Water Deficit (AWD) -----------------------------------------
# 15-day running sums of ETo and precipitation; AWD = ETo_15d - precip_15d.
# A "deficit day" occurs when AWD exceeds 40% of AWHC (Lollato et al. 2020).

AWHC_THRESHOLD <- c(0.4, 0.3, 0.5)

## also adding in a AWHC_EXCESS_THRESHOLD

AWHC_EXCESS_THRESHOLD <- c(.4, .2) # 40% is a good start


daily_stress <- obsclim[order(obsclim$site, obsclim$harvyr, obsclim$date), ]

for (k in c(15, 10, 7)) {
  eto <- ave(daily_stress$eto_mm,  daily_stress$site, daily_stress$harvyr,
             FUN = function(v) zoo::rollsumr(v, k = k, fill = NA))
  pr  <- ave(daily_stress$prcp_mm, daily_stress$site, daily_stress$harvyr,
             FUN = function(v) zoo::rollsumr(v, k = k, fill = NA))
  
  daily_stress[[paste0("eto_", k, "d")]]    <- eto
  daily_stress[[paste0("precip_", k, "d")]] <- pr
  daily_stress[[paste0("awd_mm_", k)]]      <- eto - pr
  daily_stress[[paste0("wd_day_", k)]]      <- (eto - pr) > AWHC_THRESHOLD[1] * daily_stress$awhc_mm
}

daily_stress$excess_day <- daily_stress$awd_mm_15 < 
  -(AWHC_EXCESS_THRESHOLD[1] * daily_stress$awhc_mm)

prob_by_site_doy <- function(data, flag, name) {
  # data <- daily_stress; flag <- "excess_day"; name <- "p_excess"
  d <- data[!is.na(data[[flag]]), ]
  grp <- list(site = d$site, doy = d$doy)
  
  out <- aggregate(d[[flag]], by = grp, FUN = mean)
  out$n_years <- aggregate(d[[flag]], by = grp, FUN = length)$x
  names(out)[names(out) == "x"] <- name
  out
}

daily_stress$freeze_day <- daily_stress$tmin_c < 0 
daily_stress$heat_day <- daily_stress$tmax_c > 27

# test <- c(TRUE, TRUE, FALSE, FALSE)
# mean(test)
# length(test)

excess_prob <- prob_by_site_doy(daily_stress, "excess_day", "p_excess")
wd_prob     <- prob_by_site_doy(daily_stress, "wd_day_15",  "p_wd")
freeze_prob <- prob_by_site_doy(daily_stress, "freeze_day", "p_freeze")
heat_prob     <- prob_by_site_doy(daily_stress, "heat_day",  "p_heat")

# #water deficit probability DOY
# ggplot(wd_prob, aes(doy, p_wd)) +
#   geom_point(alpha = 0.4) +
#   facet_wrap(~site) +
#   labs(title = "Probability of 15-d AWD > 40% AWHC, by day of year",
#        x = "Day of year", y = "P(water deficit)")
# ggsave("Figures/prob_wd.png", plot = last_plot(), width = 8, height = 4, unit = 'in')
# 
# #water excess probability DOY
# ggplot(excess_prob, aes(doy, p_excess)) +
#   geom_point(alpha = 0.4) +
#   facet_wrap(~site) +
#   labs(title = "Probability of 15-d AWD < -40% AWHC (excess water), by day of year",
#        x = "Day of year", y = "P(excess water)")
# ggsave("Figures/prob_we.png", plot = last_plot(), width = 8, height = 4, unit = 'in')
# 
# #freeze day probability DOY
# ggplot(freeze_prob, aes(doy, p_freeze)) +
#   geom_point(alpha = 0.4) +
#   facet_wrap(~site) +
#   labs(title = "Probability of freeze day",
#        x = "Day of year", y = "P(T min < 0)")
# ggsave("Figures/prob_freeze.png", plot = last_plot(), width = 8, height = 4, unit = 'in')
# 
# #heat excess probability DOY
# ggplot(heat_prob, aes(doy, p_heat)) +
#   geom_point(alpha = 0.4) +
#   facet_wrap(~site) +
#   labs(title = "Probability of High heat by day of year",
#        x = "Day of year", y = "P(T max > 27)")
# ggsave("Figures/prob_heat.png", plot = last_plot(), width = 8, height = 4, unit = 'in')

count_stress_days <- function(data, flag_col, out_name, season = NULL) {
  if (!is.null(season)) {
    data <- data[data$season == season, ]
  }

  counts <- aggregate(
    list(tmp_col = data[[flag_col]]),
    by = list(site = data$site, harvyr = data$harvyr),
    FUN = function(x) sum(x, na.rm =T)
  )
  names(counts)[names(counts) == "tmp_col"] <- out_name
  counts
}

## full-season counts 
freeze_counts <- count_stress_days(daily_stress, "freeze_day",  "freeze_day")
heat_counts   <- count_stress_days(daily_stress, "heat_day",    "heat_day")
wd_counts     <- count_stress_days(daily_stress, "wd_day_15",   "wd_day_15")
excess_counts <- count_stress_days(daily_stress, "excess_day",  "excess_day")

## spring-only counts 
spring_freeze_count <- count_stress_days(daily_stress, "freeze_day", "spr_freeze_day", season = "spring")
spring_heat_count   <- count_stress_days(daily_stress, "heat_day",   "spr_heat_day",   season = "spring")
spring_excess_count <- count_stress_days(daily_stress, "excess_day", "spr_excess_day", season = "spring")
spring_wd_count   <- count_stress_days(daily_stress, "wd_day_15",   "spr_wd_day_15",   season = "spring")

## fall (planting-window) counts 
fall_wd_count     <- count_stress_days(daily_stress, "wd_day_15",  "fall_wd_day_15",  season = "autumn")
fall_excess_count <- count_stress_days(daily_stress, "excess_day", "fall_excess_day", season = "autumn")
fall_freeze_count <- count_stress_days(daily_stress, "freeze_day", "fall_freeze_day", season = "autumn")
fall_heat_count <- count_stress_days(daily_stress, "heat_day", "fall_heat_day", season = "autumn")

## combine 
df_list <- list(freeze_counts, heat_counts, wd_counts, excess_counts,
                spring_freeze_count, spring_heat_count,spring_excess_count,
                spring_wd_count, fall_wd_count, fall_excess_count, fall_freeze_count,
                fall_heat_count)


stress_counts <- Reduce(function(x,y) merge(x,y, all = TRUE), df_list)


# whole dataset

full_data <- merge(stress_counts, clean_wht_data, sort = TRUE)
