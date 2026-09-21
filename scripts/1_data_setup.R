dir <- getwd()

source(paste0(dir,"/Scripts/0_functions.r"))

# Packages --------------------------------------------------------------

# All packages used throughout analysis. 
packages <- c(
  # general
  "tidyverse", 'daymetr', 'hydroTSM', 'data.table', 'janitor',
  # stats
  'mgcv', 'gratia', 
  # plotting
  'ggthemes', 'patchwork', 'latex2exp', 'viridis', 'GGally', 'ggpubr', 'scico')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Weather Data import --------------------------------------------------------------

## use `daymetr` to collect weather data \\ `hydroTSM` allows for quick season allocations

# coordinates <- data.frame(loc = c("Blacksburg", "Blackstone", "Holland",
#                                   "Painter", "Orange", "Warsaw"),
#                           lat = c(37.199312, 37.087035, 36.683618, 37.588189,
#                                   38.224127, 37.987932),
#                           long = c(-80.567631, -77.972754, -76.764003, -75.822092,
#                                    -78.119875, -76.779704))

 # write.csv(coordinates, file="LocationCoords.csv", row.names=FALSE)

daymet_data <- download_daymet_batch(
  file_location = 'LocationCoords.csv',
  start = 1990,
  end = 2022,
  internal = TRUE,
  simplify=TRUE)

# saveRDS(daymet_data, "Data/daymet_data.RDS")
daymet_data <- readRDS("Data/daymet_data.RDS")

clim_data <- daymet_data %>%
  pivot_wider(names_from = measurement, values_from = value) %>%
  rename(tmax_c = tmax..deg.c., tmin_c = tmin..deg.c., prcp_mm = prcp..mm.day.,
         doy = yday) %>%
  dplyr::select(1:7, prcp_mm, tmax_c, tmin_c) # dropping vapor pressure and solar radiation


# adding dates and season

clim_data$origin <- as.Date(paste0(clim_data$year, "-01-01"),tz = "UTC") - days(1)

clim_data$date <- as.Date(clim_data$doy, format="%j", origin = clim_data$origin)

# may not be necessary but parsing dates to month and day to have more accurate season starts

clim_data$day <- as.numeric(format(clim_data$date, format = "%d"))
clim_data$month <- as.numeric(format(clim_data$date, format = "%m"))

clim_data <- clim_data %>%
  mutate(season = time2season(date, out.fmt = 'seasons')) %>%
  mutate(season = case_when(
    #time2season breaks up season by month not by the actual day so this is necessary
    month == 6 & day <= 21 & day >= 1 ~ 'spring',
    month == 3 & day <= 21 & day >= 1 ~ 'winter',
    month == 12 & day <= 21 & day >= 1 ~ 'autumn',
    month == 9 & day <= 21 & day >= 1 ~ 'summer',
    TRUE ~ season
  )) 


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

locdate <- list()

# loop trims data by planting and harvest based on location.
# updated to include the two weeks prior to planting
for (i in 1:length(fulldate$Location)){
  # i = 1
  loc <- fulldate$Location[[i]]
  locnum <- fulldate$locnum[[i]]
  
  a <- fulldate$Planting.Date[[i]]
  b <- fulldate$Harvest.Date[[i]]
  dates <- c(seq(a, b, by = "days"))
  
  x <- list()
  x$plant_date <- paste0(as.Date(dates)) 
  x$preplant_date <- paste0(as.Date(dates) - 14) 
  x$site <- rep(loc, length(dates))
  x$locnum <- rep(locnum, length(dates))
  
  locdate[[i]] <- do.call(cbind, x)
  
}

locplantdate <- as.data.frame(do.call(rbind, locdate))
locplantdate$preplant_date <- as.Date(locplantdate$preplant_date)

# remove days with no wheat by location from full weather data

obsclim <- merge(locplantdate, clim_data, by.x = c('site',"preplant_date" ), 
                 by.y = c("site", 'date'), all.x = TRUE) %>%
  # add growing degree days 
  mutate(gdd_1 =  ifelse(((tmax_c + tmin_c) / 2) - 0 < 0, 0, 
                         (((tmax_c + tmin_c) / 2) - 0))) %>% # Traditional GDD calc for wheat
  group_by(site, harvyr) %>%
  mutate(c_gdd_1 = cumsum(gdd_1)) %>%
  ungroup() %>% arrange(site, preplant_date) %>%
  # add in assumed growth stage based on growth stage
  mutate(growth_stage_1 = case_when( ## based on base temp of 0
    c_gdd_1 <= 70 ~ "germ",
    c_gdd_1 <= 400 ~ "emergence",
    c_gdd_1 <= 685 ~ "tillering",
    c_gdd_1 <= 875 ~ "boot",
    c_gdd_1 <= 1075 ~ "flowering",
    c_gdd_1 <= 1575 ~ "grain_fill",
    c_gdd_1 > 1576  ~ "maturity"
  ))


obsclim$season <- as.factor(obsclim$season)
obsclim$locnum <- as.numeric(obsclim$locnum)
obsclim <- obsclim[order(obsclim$locnum, obsclim$preplant_date, obsclim$season),]


# write.csv(obsclim, "Data/climbyloc.csv") ###
obsclim <- read_csv("Data/climbyloc.csv")


# Import SSURGO Data for AWC ----------------------------------------------


