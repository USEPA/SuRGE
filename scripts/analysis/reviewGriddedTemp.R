
# SHALLOW WATER TEMPERATURE---------------
# plot predicted vs observed
dat %>%
  mutate(era5_shallow_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_mix_layer_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_mix_layer_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_mix_layer_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_mix_layer_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_mix_layer_temp)) %>%
  ggplot(aes(shallow_temp, era5_shallow_temp)) +
  geom_point() +
  xlab("measured shallow temp") +
  ylab("ERA5 shallow temp") +
  ggtitle("Shallow Water Temp") +
  geom_abline(slope=1, intercept = 0, linetype = "longdash") +
  stat_smooth(method = "lm")

# calculate bias per temp bin
dat %>%
  mutate(era5_shallow_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_mix_layer_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_mix_layer_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_mix_layer_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_mix_layer_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_mix_layer_temp),
    temp_bias = era5_shallow_temp - shallow_temp) %>%
  mutate(shallow_temp_bin = cut(shallow_temp, breaks=c(0, 20, 25, 30, 40))) %>%
  group_by(shallow_temp_bin) %>%
  summarize(era5_shallow_temp_bias = mean(temp_bias, na.rm = TRUE))

# plot bias corrected values
dat %>%
  mutate(era5_shallow_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_mix_layer_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_mix_layer_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_mix_layer_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_mix_layer_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_mix_layer_temp),
    era5_shallow_temp_corrected = case_when(
      shallow_temp < 20 ~ era5_shallow_temp + 4.02,
      shallow_temp >= 20 & shallow_temp < 25 ~ era5_shallow_temp + 3.17,
      shallow_temp >= 25 & shallow_temp < 30 ~ era5_shallow_temp + 6.22,
      shallow_temp >= 30 ~ era5_shallow_temp + 6.5)) %>%
  ggplot(aes(shallow_temp, era5_shallow_temp_corrected)) +
  geom_point() +
  xlab("measured shallow temp") +
  ylab("ERA5 mixed layer temp bias corrected") +
  ggtitle("Shallow Water Temp Corrected") +
  geom_abline(slope=1, intercept = 0, linetype = "longdash") +
  stat_smooth(method = "lm")



# DEEP WATER TEMPERATURE---------------
# plot predicted vs observed
dat %>%
  mutate(era5_deep_water_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_bottom_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_bottom_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_bottom_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_bottom_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_bottom_temp)) %>%
  ggplot(aes(deep_temp, era5_deep_water_temp)) +
  geom_point() +
  xlab("measured deep temp") +
  ylab("ERA5 deep water temp") +
  ggtitle("Deep Water Temp") +
  geom_abline(slope=1, intercept = 0, linetype = "longdash") +
  stat_smooth(method = "lm")

# calculate bias per temp bin
dat %>%
  mutate(era5_deep_water_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_bottom_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_bottom_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_bottom_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_bottom_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_bottom_temp),
    temp_bias = era5_deep_water_temp - deep_temp) %>%
  mutate(deep_temp_bin = cut(deep_temp, breaks=c(0, 10, 20, 35))) %>%
  group_by(deep_temp_bin) %>%
  summarize(era5_deep_temp_bias = mean(temp_bias, na.rm = TRUE))

# plot bias corrected values
dat %>%
  mutate(era5_deep_temp = case_when(
    format(sample_date,"%B") == "May" ~ mean_may_lake_bottom_temp,
    format(sample_date,"%B") == "June" ~ mean_june_lake_bottom_temp,
    format(sample_date,"%B") == "July" ~ mean_july_lake_bottom_temp,
    format(sample_date,"%B") == "August" ~ mean_aug_lake_bottom_temp,
    format(sample_date,"%B") == "September" ~ mean_may_lake_bottom_temp),
    era5_deep_temp_corrected = case_when(
      deep_temp < 10 ~ era5_deep_temp - 4.36,
      deep_temp >= 10 & deep_temp < 2 ~ era5_deep_temp + 5.55,
      deep_temp >= 20 ~ era5_deep_temp + 9.29)) %>%
  ggplot(aes(deep_temp, era5_deep_temp_corrected)) +
  geom_point() +
  xlab("measured deep temp") +
  ylab("ERA5 deep temp bias corrected") +
  ggtitle("Deep Water Temp Corrected") +
  geom_abline(slope=1, intercept = 0, linetype = "longdash") +
  stat_smooth(method = "lm")
