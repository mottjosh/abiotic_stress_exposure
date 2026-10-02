dir <- getwd()

source(paste0(dir,"/Scripts/0_functions.r"))
source(paste0(dir,"/Scripts/1_data_setup.r"))

# Packages --------------------------------------------------------------

# All packages used throughout analysis. 
packages <- c(
  # general
  "tidyverse", 'daymetr', 'hydroTSM', 'data.table', 'janitor', 'soilDB', 'sf',
  # stats
  'mgcv', 'gratia', 
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

# Water Deficit Figures #### 
daily_stress %>%
  filter(harvyr == max(harvyr, na.rm = TRUE)) %>%
  pivot_longer(cols = c(awd_mm_15, awd_mm_10, awd_mm_7), names_to = "awd_window",
               values_to = "awd_mm") %>%
  ggplot(aes(date, awd_mm)) +
  geom_line(aes(group = awd_window, color = awd_window)) +
  geom_texthline(aes(yintercept = AWHC_THRESHOLD[1] * awhc_mm),
                 linetype = "dashed", label = "40%") +
  geom_texthline(aes(yintercept = AWHC_THRESHOLD[2] * awhc_mm),
                 linetype = "dashed", label = "30%") +
  geom_texthline(aes(yintercept = AWHC_THRESHOLD[3] * awhc_mm),
                 linetype = "dashed", label = "50%") +
  facet_wrap(~site) +
  labs(title = "AWD vs. AWHC thresholds (dashed), most recent harvyr (2021)",
       y = "AWD (mm)") +
  theme_minimal()

# Cleaner Figure (both deficit and excess)

daily_stress %>%
  filter(harvyr == max(harvyr, na.rm = TRUE)) %>%
  pivot_longer(cols = c(awd_mm_15, awd_mm_10, awd_mm_7), names_to = "awd_window",
               values_to = "awd_mm") %>%
  filter(awd_window == "awd_mm_15") %>% # sticking with 15 day rolling window
  ggplot(aes(date, awd_mm)) +
  geom_line(color = 'blue') +
  geom_texthline(aes(yintercept = AWHC_THRESHOLD[1] * awhc_mm),
                 linetype = "dashed", label = "40%, Deficit",hjust = 1) +
  geom_texthline(aes(yintercept = -(AWHC_EXCESS_THRESHOLD[1] * awhc_mm)),
                 linetype = "dashed", label = "-40%, Excess") +
  geom_texthline(aes(yintercept = -(AWHC_EXCESS_THRESHOLD[2] * awhc_mm)),
                 linetype = "dashed", label = "-20%, Excess")+
  facet_wrap(~site) +
  labs(title = "AWD vs. AWHC thresholds (dashed), most recent harvyr (2021)",
       y = "AWD (mm)") +
  theme_minimal()

#another try
bands_df <- daily_stress %>%
  distinct(site, awhc_mm) %>%
  transmute(#superseded, just use mutate and .keep = 'none'
    site,
    Deficit_ymin = AWHC_THRESHOLD * awhc_mm,
    Deficit_ymax = Inf,
    `Excess (moderate)_ymin` = -(AWHC_EXCESS_THRESHOLD[1] * awhc_mm),
    `Excess (moderate)_ymax` = -(AWHC_EXCESS_THRESHOLD[2] * awhc_mm),
    `Excess (severe)_ymin`   = -Inf,
    `Excess (severe)_ymax`   = -(AWHC_EXCESS_THRESHOLD[1] * awhc_mm)
  ) %>%
  pivot_longer(
    -site,
    names_to = c("band", ".value"),
    names_pattern = "(.*)_(ymin|ymax)"
  )

label_df <- bands_df %>%
  group_by(site) %>%
  mutate(x_pos = min(daily_stress$date[daily_stress$site == site[1]]) + 10) %>%
  ungroup() %>%
  mutate(
    y_pos = case_when(
      band == "Deficit"          ~ ymin + 15,
      band == "Excess (severe)"  ~ ymax - 15,
      TRUE                        ~ (ymin + ymax) / 2
    )
  )

daily_stress %>%
  filter(harvyr == max(harvyr, na.rm = TRUE)) %>%
  pivot_longer(cols = c(awd_mm_15, awd_mm_10, awd_mm_7), names_to = "awd_window",
               values_to = "awd_mm") %>%
  filter(awd_window == "awd_mm_15") %>%
  ggplot(aes(date, awd_mm)) +
  geom_rect(
    data = bands_df, inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = band),
    alpha = 0.15
  ) +
  geom_line(color = "black", linewidth = .5) +
  geom_text(
    data = label_df, inherit.aes = FALSE,
    aes(x = as_date("2021-9-10"), y = y_pos, label = band, color = band),
    hjust = 0, size = 3, show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Deficit" = "firebrick", "Excess (moderate)" = "steelblue", "Excess (severe)" = "navy"
  )) +
  scale_color_manual(values = c(
    "Deficit" = "firebrick", "Excess (moderate)" = "steelblue4", "Excess (severe)" = "navy"
  )) +
  facet_wrap(~site) +
  labs(title = "AWD vs. AWHC thresholds (shaded), most recent harvyr (2021)",
       y = "AWD (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("Figures/water_deficit_excess.png", plot = last_plot(), width = 8, height = 4, unit = 'in')

daily_stress %>%
  filter(harvyr == max(harvyr, na.rm = TRUE)) %>%
  pivot_longer(cols = c(awd_mm_15, awd_mm_10, awd_mm_7), names_to = "awd_window",
               values_to = "awd_mm") %>%
  filter(awd_window == "awd_mm_15") %>%
  ggplot(aes(date, awd_mm)) +
  geom_rect(
    data = bands_df, inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = band),
    alpha = 0.15
  ) +
  geom_line(color = "blue") +
  geom_text(
    data = label_df, inherit.aes = FALSE,
    aes(x = x_pos, y = y_pos, label = band, color = band),
    hjust = 0, size = 3, show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Deficit" = "firebrick", "Excess (moderate)" = "steelblue", "Excess (severe)" = "navy"
  )) +
  scale_color_manual(values = c(
    "Deficit" = "firebrick", "Excess (moderate)" = "steelblue4", "Excess (severe)" = "navy"
  )) +
  facet_wrap(~site) +
  labs(title = "AWD vs. AWHC thresholds (shaded), most recent harvyr (2021)",
       y = "AWD (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

#water deficit probability DOY
ggplot(wd_prob, aes(doy, p_wd)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~site) +
  labs(title = "Probability of 15-d AWD > 40% AWHC, by day of year",
       x = "Day of year", y = "P(water deficit)")
ggsave("Figures/prob_wd.png", plot = last_plot(), width = 8, height = 4, unit = 'in')

#water excess probability DOY
ggplot(excess_prob, aes(doy, p_excess)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~site) +
  labs(title = "Probability of 15-d AWD < -40% AWHC (excess water), by day of year",
       x = "Day of year", y = "P(excess water)")
ggsave("Figures/prob_we.png", plot = last_plot(), width = 8, height = 4, unit = 'in')

