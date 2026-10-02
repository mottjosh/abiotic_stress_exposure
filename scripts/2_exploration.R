# Looking at some trends and interesting things

dir <- getwd()

source(paste0(dir,"/Scripts/1_data_setup.r"))


# What are we seeing? -----------------------------------------------------

# top_5_per_year <- cleanYldData %>%
#   group_by(site, harvyr) %>% 
#   slice_max(order_by = yield, n = 5, with_ties = FALSE) %>%
#   ungroup()
# 
# # Massey only shows up once as top 5 yielding varieties
# top_5_per_year %>% filter(ID == '17') 
# 
# # add massey to top 5 dataset 
# massey_data <- cleanYldData %>% filter(ID == '17')
# 
# 
# top_5_per_year %>%
#   bind_rows(massey_data) %>%
#   mutate(group = case_when(
#     ID == "17" ~ 'Check',
#     TRUE~ 'Top 5'
#   )) %>% 
#   ggplot(aes(x = harvyr, y = yield, group = group)) +
#   geom_point(aes(color = group), alpha = .2) +
#   geom_smooth(aes(color = group, linetype = group), method = 'lm') +
#   scale_color_scico_d(palette = 'roma' ) +
#   theme_classic() +
#   labs(y = "Yield (bu/ac)",
#        x = "Harvest Year",
#        color = "",
#        linetype = '')
# 
# ggsave("Figures/top_5_vs_massey.png", plot = last_plot(), width = 8, height = 4, unit = "in")
# 
# 
# # looking at the trend over each location
# 
# top_5_per_year %>%
#   bind_rows(massey_data) %>%
#   mutate(group = case_when(
#     ID == "17" ~ 'Check',
#     TRUE~ 'Top 5'
#   )) %>% 
#   ggplot(aes(x = harvyr, y = yield, group = group)) +
#   geom_point(aes(color = group), alpha = .2) +
#   geom_smooth(aes(color = group, linetype = group), method = 'lm') +
#   scale_color_scico_d(palette = 'roma' ) +
#   facet_wrap(~site) +
#   theme_classic() +
#   labs(y = "Yield (bu/ac)",
#        x = "Harvest Year",
#        color = "",
#        linetype = "")
# 
# ggsave("Figures/top_5_vs_massey_by_site.png",plot = last_plot(), width = 8, height = 4, unit = "in")

top5_lines_by_year <- cleanYldData %>%
  group_by(harvyr, ID) %>%
  summarise(mean_yield_all_sites = mean(yield, na.rm = TRUE),
            n_sites = n_distinct(site), .groups = "drop") %>%
  group_by(harvyr) %>%
  slice_max(mean_yield_all_sites, n = 5, with_ties = FALSE) %>%
  ungroup()

# pull those 5 lines actual yields at every site
top5_across_sites <- cleanYldData %>%
  semi_join(top5_lines_by_year, by = c("harvyr", "ID")) %>%
  bind_rows(massey_data) %>%
  mutate(group = case_when(
    ID == "17" ~ 'Check',
    TRUE~ 'Top 5'
  ))

top5_across_sites %>%
  ggplot(aes(x = harvyr, y = yield, group = group)) +
  geom_point(aes(color = group), alpha = .2) +
  geom_smooth(aes(color = group, linetype = group), method = 'lm') +
  scale_color_scico_d(palette = 'roma' ) +
  theme_classic() +
  labs(y = "Yield (bu/ac)",
       x = "Harvest Year",
       color = "",
       linetype = '')

ggsave("Figures/top_5_vs_massey.png", plot = last_plot(), width = 8, height = 6, unit = "in")

top5_across_sites %>%
  ggplot(aes(x = harvyr, y = yield, group = group)) +
  geom_point(aes(color = group), alpha = .2) +
  geom_smooth(aes(color = group, linetype = group), method = 'lm') +
  scale_color_scico_d(palette = 'roma' ) +
  facet_wrap(~site) +
  theme_classic() +
  labs(y = "Yield (bu/ac)",
       x = "Harvest Year",
       color = "",
       linetype = "")

ggsave("Figures/top_5_vs_massey_by_site.png",plot = last_plot(), width = 8, height = 6, unit = "in")

# values for each of the trends
top5_across_sites %>%
  group_by(site, group) %>%
  do(tidy(lm(yield ~ harvyr, data = .))) %>%
  filter(term == "harvyr") %>%
  select(site, slope_bu_per_yr = estimate, se = std.error, p = p.value)


gap_df <- top5_across_sites %>%
  group_by(site, harvyr) %>%
  summarise(top5_mean = mean(yield, na.rm = TRUE), .groups = "drop") %>%
  left_join(massey_data %>% group_by(site, harvyr) %>%
              summarise(check = mean(yield, na.rm = TRUE), .groups = "drop"),
            by = c("site", "harvyr")) %>%
  mutate(gap = top5_mean - check, pct_of_check = 100 * top5_mean / check)

gap_df %>%
  ggplot(aes(x = harvyr, y = gap)) +
    geom_point() + 
    geom_smooth(method = "lm") +
    facet_wrap(~site) +
  theme_classic() +
  labs(y = "Yield gap between check (Massey) and top 5",
       x = "Harvest Year")

ggsave("Figures/top_5_vs_massey_ygap.png",plot = last_plot(), width = 8, height = 6, unit = "in")

gap_df %>%
  group_by(site) %>%
  do(tidy(lm(gap ~ harvyr, data = .))) %>%
  filter(term == "harvyr") %>%
  select(site, slope_bu_per_yr = estimate, se = std.error, p = p.value)

# does the trend differ by site?
anova(lm(gap ~ harvyr + site, gap_df), lm(gap ~ harvyr * site, gap_df))

# weather variables -------------------------------------------------------

# max/min/means

weather_summ_cols <- obsclim %>%
  select(prcp_mm, tmax_c, tmin_c) %>% colnames()
  
annual_weather <- obsclim %>%
  distinct(site, harvyr, date, .keep_all = TRUE) %>%   # drops the duplicated planting-date rows
  group_by(site, harvyr) %>%
  summarise(
    tmax_c_mean  = mean(tmax_c, na.rm = TRUE),
    tmin_c_mean  = mean(tmin_c, na.rm = TRUE),
    prcp_mm_total = sum(prcp_mm, na.rm = TRUE),   # annual precip is a total, not a mean of daily values
    n_days = n(),
    .groups = "drop"
  )

# one row per site: average across years, with the year-to-year spread
site_annual_summary <- annual_weather %>%
  group_by(site) %>%
  summarise(
    across(c(tmax_c_mean, tmin_c_mean, prcp_mm_total),
           list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE))),
    n_years = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = everything(),
               names_to = "stat",
               values_to = 'value')
  mutate(across(where(is.numeric), signif, 3)) %>%
  mutate(mean = paste(median, paste0("(",min, "-", max,")"))) %>%
  select(CEAP_name,variable, med_min_max)

site_annual_summary




