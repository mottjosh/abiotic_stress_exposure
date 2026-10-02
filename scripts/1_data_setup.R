dir <- getwd()

source(paste0(dir,"/Scripts/0_functions.r"))

# Packages --------------------------------------------------------------

# All packages used throughout analysis. 
packages <- c(
  # general
  "tidyverse", 'daymetr', 'data.table', 'janitor', 'soilDB', 'sf',
  # stats
  'mgcv', 'gratia', 'lme4', 'lmerTest',
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
  end = 2022,
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

# summary(clean_wht_data)
# str(clean_wht_data)


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
  ", mukey)
  
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


# Evapotranspiratio (Hargreaves-Samani) -----------------------------------------------------


## we will start with this equation but sure there are better options
## need to consult with the plant physiology people

## https://www.fao.org/4/x0490e/x0490e07.htm#solar%20radiation

##https://www.fao.org/4/x0490e/x0490e00.htm


obsclim <- obsclim %>%
  mutate(eto_mm = calc_ETo_hargreaves(tmax_c, tmin_c, doy, latitude ))

summary(obsclim$eto_mm)   # should be roughly 0-8 mm/day for this region/season range,
# negative or NA values indicate a join/data problem

obsclim %>%
  filter(is.na(eto_mm)) %>%
  count(site)   # any site with all-NA ETo likely failed the latitude join


str(obsclim)

ggplot(obsclim %>% filter(harvyr == max(harvyr, na.rm = TRUE)), aes(x = date, y = eto_mm)) +
  geom_line() +
  facet_wrap(~site) +
  labs(title = "Daily ETo (Hargreaves), 2021",
       y = "ETo (mm/day)") +
  theme_classic()

ggsave("Figures/ET0.png", plot = last_plot(), width = 8, height = 5, unit = "in")

# Atmospheric Water Deficit (AWD) -----------------------------------------
# 15-day running sums of ETo and precipitation; AWD = ETo_15d - precip_15d.
# A "deficit day" occurs when AWD exceeds 40% of AWHC (Lollato et al. 2020).

AWHC_THRESHOLD <- c(0.4, 0.3, 0.5)

## also adding in a AWHC_EXCESS_THRESHOLD

AWHC_EXCESS_THRESHOLD <- c(.4, .2) # 40% is a good start


daily_stress <- obsclim %>%
  arrange(site, harvyr, date) %>%
  group_by(site, harvyr) %>%
  mutate(
    # 15-day rolling sum
    eto_15d    = zoo::rollsumr(eto_mm, k = 15, fill = NA),
    precip_15d = zoo::rollsumr(prcp_mm, k = 15, fill = NA),
    awd_mm_15     = eto_15d - precip_15d,
    # 10-day rolling sum
    eto_10d    = zoo::rollsumr(eto_mm, k = 10, fill = NA),
    precip_10d = zoo::rollsumr(prcp_mm, k = 10, fill = NA),
    awd_mm_10     = eto_10d - precip_10d,
    # 7-Day rolling Sum
    eto_7d    = zoo::rollsumr(eto_mm, k = 7, fill = NA),
    precip_7d = zoo::rollsumr(prcp_mm, k = 7, fill = NA),
    awd_mm_7 = eto_7d - precip_7d,
    # Can think about changing this over time
    wd_day_15   = awd_mm_15 > AWHC_THRESHOLD[1] * awhc_mm,
    wd_day_10   = awd_mm_10 > AWHC_THRESHOLD[1] * awhc_mm,
    wd_day_7   = awd_mm_7  > AWHC_THRESHOLD[1] * awhc_mm,
    #excess days (starting with 15 day rolling)
    excess_day = awd_mm_15 < -(AWHC_EXCESS_THRESHOLD[1] * awhc_mm)) %>%
  ungroup()


excess_prob <- daily_stress %>%
  filter(!is.na(excess_day)) %>%
  group_by(site, doy) %>%
  summarise(p_excess = mean(excess_day), n_years = n(), .groups = "drop")

wd_prob <- daily_stress %>%
  filter(!is.na(wd_day_15)) %>%
  group_by(site, doy) %>%
  summarise(
    p_wd = mean(wd_day_15),
    n_years = n(),
    .groups = "drop"
  )


TARGET_P <- 0.1
SPRING_DOY_RANGE <- c(100, 200)   # restrict the regression to the spring
# transition window -- adjust based on
# what the plot above actually shows;
# this range should bracket where p_wd
# visibly rises from near 0 toward 1

find_onset_doy <- function(df, target_p = TARGET_P, doy_range = SPRING_DOY_RANGE) {
  fit_data <- df %>% filter(doy >= doy_range[1], doy <= doy_range[2])
  if (nrow(fit_data) < 3 || sd(fit_data$p_wd, na.rm = TRUE) == 0) return(NA_real_)
  
  m <- lm(p_wd ~ doy, data = fit_data)
  intercept <- coef(m)[1]
  slope <- coef(m)[2]
  if (slope == 0) return(NA_real_)
  
  (target_p - intercept) / slope
}

wd_onset <- wd_prob %>%
  group_by(site) %>%
  group_modify(~ tibble(onset_doy = find_onset_doy(.x))) %>%
  ungroup()

wd_onset   # DOY per site when spring AWD risk crosses p = .2 --
# compare this against each line's actual heading date (hd_all)
# to get "days before/after heading with elevated AWD risk"

# sanity check: does onset_doy fall inside SPRING_DOY_RANGE and look
# consistent with the rising-limb pattern visible in the plot above?
# if a site's onset_doy looks implausible, its p_wd curve may not have
# a clean single rising limb in that window -- worth plotting that
# site individually before trusting the regression



# Heading date exploration  -----------------------------------------------

## 2 objectives, 
### - Explore ability to impute heading date based on GDD
### - find reference heading date for each location

## Q: is the total thermal time a line needs to reach heading roughly constant


CAL_SITES <- c("Blacksburg", "Warsaw")
MIN_YEARS_LINE <- 3      # matches the project's existing >=3-year rule
TBASE <- 0                # McMaster & Wilhelm (1997) / Lollato et al. (2020)
# use Tbase = 0C for the Jan-1-anchored GDD calc --
# NOTE: this is a different GDD definition than the
# DAP-anchored, base-4.5C c_gdd_2 used in the earlier
# random-regression script. Keep these separate.


## 1. Build cumulative GDD from 1 January, per site-year --------------------

obsclim_jan1 <- obsclim %>%
  mutate(
    doy = yday(date),
    gdd_daily = pmax(((tmax_c + tmin_c) / 2) - TBASE, 0)
  ) %>%
  # restart the cumulative sum at Jan 1 of the *calendar* year, not the
  # harvyr-anchored planting date used elsewhere -- filter to Jan-Jul,
  # since heading occurs well before harvyr's fall planting starts again
  filter(month(date) <= 7) %>%
  arrange(site, harvyr, date) %>%
  group_by(site, harvyr) %>%
  mutate(cum_gdd_jan1 = cumsum(gdd_daily)) %>%
  ungroup()


## 2. Get each line's GDD-at-heading, at the 2 calibration sites ------------

hd_cal <- cleanYldData %>% clean_names() %>%
  mutate(harvyr = as.numeric(as.character(harvyr)), site = str_trim(site)) %>%
  dplyr::select(id, site, harvyr, hd) %>%
  filter(site %in% CAL_SITES, !is.na(hd)) %>% #drop NA's
  mutate(HD = as.Date(hd - 1, origin = paste0(harvyr, "-01-01"))) %>%  # DOY -> Date
  # rename(HD = hd) %>%
  left_join(
    obsclim_jan1 %>% dplyr::select(site, harvyr, date, cum_gdd_jan1),
    by = c("site", "harvyr", "HD" = "date")
  ) 
years_per_line <- hd_cal %>% distinct(id, harvyr) %>% count(id, name = "n_years")
keep_lines <- years_per_line %>% filter(n_years >= MIN_YEARS_LINE) %>% pull(id)

hd_cal <- hd_cal %>% filter(id %in% keep_lines, !is.na(cum_gdd_jan1))

obsclim_jan1 %>%
  count(site, harvyr, date) %>%
  filter(n > 1)

# does the duplication already exist in the base obsclim, or did it
# get introduced somewhere in this script's pipeline?
obsclim %>% count(site, harvyr, date) %>% filter(n > 1) %>% nrow()

# if duplicates exist, look at one specific case to understand the shape
dup_keys <- obsclim_jan1 %>% count(site, harvyr, date) %>% filter(n > 1) %>% slice(1)
obsclim_jan1 %>% semi_join(dup_keys, by = c("site", "harvyr", "date"))

fulldate %>%
  filter(Location == "Warsaw") %>%
  arrange(Harvest.Year) %>%   # or whatever the year column is called there
  print(n = Inf)

site_consistency <- hd_cal %>%
  group_by(id, site) %>%
  summarise(mean_gdd_req = mean(cum_gdd_jan1, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  pivot_wider(names_from = site, values_from = c(mean_gdd_req, n))

## some of the Warsaw GDD's are much higher than Blacksburg for the same ID


# only meaningful for lines actually observed at both calibration sites
site_consistency_paired <- site_consistency %>%
  filter(!is.na(mean_gdd_req_Blacksburg), !is.na(mean_gdd_req_Warsaw))


nrow(site_consistency_paired)   # how many lines can this check even run on

line_diffs <- site_consistency_paired %>%
  mutate(gdd_diff = mean_gdd_req_Warsaw - mean_gdd_req_Blacksburg)

mean_diff <- mean(line_diffs$gdd_diff, na.rm = TRUE)
sd_diff   <- sd(line_diffs$gdd_diff, na.rm = TRUE)

line_diffs <- line_diffs %>%
  mutate(z_score = (gdd_diff - mean_diff) / sd_diff,
         flag_outlier = abs(z_score) > 2.5)   # or 2.5/3, depending how conservative you want

line_diffs %>% filter(flag_outlier) %>% arrange(desc(abs(z_score)))

ggplot(line_diffs, aes(gdd_diff)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = mean_diff, color = "red") +
  labs(title = "Distribution of Warsaw - Blacksburg GDD requirement difference",
       x = "GDD difference (Warsaw - Blacksburg)")


# paired comparison: is a line's GDD requirement the same at both sites?
cor.test(site_consistency_paired$mean_gdd_req_Blacksburg,
         site_consistency_paired$mean_gdd_req_Warsaw)

t.test(site_consistency_paired$mean_gdd_req_Blacksburg,
       site_consistency_paired$mean_gdd_req_Warsaw, paired = TRUE)

ggplot(site_consistency_paired, aes(mean_gdd_req_Blacksburg, mean_gdd_req_Warsaw)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Per-line GDD-to-heading requirement: Blacksburg vs. Warsaw",
       x = "Mean GDD requirement, Blacksburg", y = "Mean GDD requirement, Warsaw")

# 3b. Flag and remove outlier lines (site-to-site requirement wildly
##     inconsistent, beyond the typical systematic offset) ------------------
# z-score rule on the Warsaw-Blacksburg difference, not on the raw
# requirement itself, since a modest systematic offset between the two
# sites is expected and not itself a problem -- only large deviations
# FROM that typical offset get flagged

OUTLIER_Z_THRESHOLD <- 2   # adjust if this flags too many/few lines;
# 2-3 is a common range for this kind of rule

line_diffs <- site_consistency_paired %>%
  mutate(gdd_diff = mean_gdd_req_Warsaw - mean_gdd_req_Blacksburg)

mean_diff <- mean(line_diffs$gdd_diff, na.rm = TRUE)
sd_diff   <- sd(line_diffs$gdd_diff, na.rm = TRUE)

line_diffs <- line_diffs %>%
  mutate(z_score = (gdd_diff - mean_diff) / sd_diff,
         flag_outlier = abs(z_score) > OUTLIER_Z_THRESHOLD)

ggplot(line_diffs, aes(gdd_diff)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = mean_diff, color = "red") +
  labs(title = "Distribution of Warsaw - Blacksburg GDD requirement difference",
       subtitle = paste0(sum(line_diffs$flag_outlier), " lines flagged as outliers (|z| > ",
                         OUTLIER_Z_THRESHOLD, ")"),
       x = "GDD difference (Warsaw - Blacksburg)")

outlier_ids <- line_diffs %>% filter(flag_outlier) %>% pull(id)
outlier_ids   # keep a record of exactly which lines were dropped, and why,
# for the methods write-up

line_diffs %>% filter(flag_outlier) %>% arrange(desc(abs(z_score)))

# remove outlier lines from hd_cal before the pooled requirement is fit,
# so they don't influence m_req at all (not just excluded from output)
hd_cal <- hd_cal %>% filter(!id %in% outlier_ids)


## 4. Final per-line GDD requirement (pooled across calibration sites) ------
# using a mixed model with line random intercept lets lines with fewer
# observations borrow strength, same logic as the earlier random-regression
# approach, but now just for a single scalar per line rather than a slope

m_req <- lmer(hd ~ cum_gdd_jan1 + 1 + (1 | id), data = hd_cal)

line_req <- ranef(m_req)$id %>%
  as.data.frame() %>%
  rownames_to_column("id") %>%
  rename(dev = `(Intercept)`) %>%
  mutate(gdd_requirement = fixef(m_req)[1] + dev) %>%
  dplyr::select(id, gdd_requirement)

line_req   # each line's final calibrated GDD-to-heading requirement


## 5. Validate: how well does this reproduce known HD at the calibration
##    sites themselves? (in-sample check, comparable to the paper's own
##    RMSD/r2 validation of their phenology model) ---------------------------

find_heading_doy <- function(site_i, harvyr_i, gdd_target) {
  traj <- obsclim_jan1 %>% filter(site == site_i, harvyr == harvyr_i) %>% arrange(date)
  hit <- traj %>% filter(cum_gdd_jan1 >= gdd_target) %>% slice(1)
  if (nrow(hit) == 0) return(as.Date(NA))
  hit$date
}

validation <- hd_cal %>%
  left_join(line_req, by = "id") %>%
  rowwise() %>%
  mutate(HD_predicted = find_heading_doy(site, harvyr, gdd_requirement)) %>%
  ungroup() %>%
  mutate(resid_days = as.numeric(HD - HD_predicted))

validation %>%
  summarise(
    RMSD = sqrt(mean(resid_days^2, na.rm = TRUE)),
    MAD  = mean(abs(resid_days), na.rm = TRUE),
    bias = mean(resid_days, na.rm = TRUE),
    r2   = cor(as.numeric(HD), as.numeric(HD_predicted), use = "complete.obs")^2
  )

ggplot(validation, aes(as.numeric(HD_predicted), as.numeric(HD))) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Validation: predicted vs. actual HD at calibration sites",
       x = "Predicted HD (thermal-time model)", y = "Actual HD")

# compare this RMSD/r2 directly against the paper's own reported values
# (RMSD = 8 d, r2 = .67) as a benchmark for whether this simpler,
# per-line approach is performing comparably
loyo_validation <- hd_cal %>%
  distinct(id, harvyr) %>%
  pmap_dfr(function(id, harvyr) {
    train <- hd_cal %>% filter(id == !!id, harvyr != !!harvyr)
    if (nrow(train) == 0) return(NULL)
    
    req_loyo <- mean(train$cum_gdd_jan1, na.rm = TRUE)
    
    test_row <- hd_cal %>% filter(id == !!id, harvyr == !!harvyr)
    
    test_row %>%
      mutate(
        gdd_requirement_loyo = req_loyo,
        HD_predicted = pmap(list(site, harvyr, gdd_requirement_loyo),
                            ~ find_heading_doy(..1, ..2, ..3))
      )
  }) %>%
  mutate(HD_predicted = as.Date(unlist(HD_predicted), origin = "1970-01-01"),
         resid_days = as.numeric(HD - HD_predicted))

loyo_validation %>%
  summarise(
    RMSD = sqrt(mean(resid_days^2, na.rm = TRUE)),
    MAD  = mean(abs(resid_days), na.rm = TRUE),
    bias = mean(resid_days, na.rm = TRUE)
  )

ggplot(loyo_validation, aes(HD_predicted, HD)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  scale_x_date(date_labels = "%b %d") +
  scale_y_date(date_labels = "%b %d") +
  labs(title = "Leave-one-year-out validation: predicted vs. actual HD",
       x = "Predicted HD (LOYO)", y = "Actual HD")


## 6. Impute heading date at every other site-year --------------------------

impute_target <- cleanYldData %>%
  mutate(harvyr = as.numeric(as.character(harvyr)), site = str_trim(site)) %>%
  distinct(id, site, harvyr) %>%
  filter(id %in% line_req$id, !id %in% outlier_ids) %>%   # drop flagged outliers here too
  left_join(line_req, by = "id")

impute_target <- impute_target %>%
  rowwise() %>%
  mutate(HD_imputed = find_heading_doy(site, harvyr, gdd_requirement)) %>%
  ungroup()

impute_target %>% filter(is.na(HD_imputed)) %>% count(site)
# NAs likely mean that site-year's weather trajectory never reached the
# line's GDD requirement within the Jan-Jul window kept above -- worth
# checking whether that's a real agronomic signal (a line that's too
# late-maturing for that site) or a window-length problem


## 7. Sanity check against Massey --------------------------------------------

massey_check <- impute_target %>%
  filter(id == "Massey") %>%
  arrange(harvyr, site) %>%
  left_join(cleanYldData %>% mutate(harvyr = as.numeric(as.character(harvyr))) %>%
              dplyr::select(id, site, harvyr, hd), by = c("id", "site", "harvyr"))

massey_check
# compare HD_imputed against hd (actual) at Blacksburg/Warsaw specifically
# -- these rows should match closely since Massey's own requirement was
# calibrated partly from its own data at those sites (in-sample check)

