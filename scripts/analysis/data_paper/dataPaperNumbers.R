# 1. SURVEY DESIGN DESCRIPTION------------------------------
# 2 hand picked site excluding Falls Lake and 2016 study
lake.list.all %>% 
  filter(site_type == "HAND", # hand picked
         !(lake_id %in% as.character(1001:1033))) # not in 2016 study

# 112 probability sites
lake.list.all %>%
  # deal with subsampled missouri river impoundments
  mutate(
    lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                      lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                      TRUE ~ lake_id)) %>%
  distinct(lake_id, site_type) %>%
  filter(site_type == "PROB")


# 2. SUMMARIZE EMISSIONS--------------

mes<-dat %>%
  filter(!is.na(ch4_diffusion_best))%>%
  mutate(ch4diff=ch4_diffusion_best*24)
#2150 individual measurements
mez<-mes %>%
  filter(ch4_diffusion_best==0)
#38 are zero values

cas<-dat %>%
  filter(!is.na(co2_diffusion_best))%>%
  mutate(co2diff=co2_diffusion_best*24)
#2083 individual measurements
caz<-cas %>%
  filter(co2_diffusion_best==0)
#264 are zero values


# 3. K600 TECHNICAL VALIDATION
# 3.1 CO2 direction and gas under/supersaturation
dissolved_gas_k %>%
  # only observations with dissolved co2 and co2 diffusion
  filter(!if_any(c(co2_sat_ratio, co2_diffusion_best), ~ is.na(.x))) %>% 
  # only those with detectable CO2 diffusion and CO2 > 0
  filter(co2_diffusion_best != 0) %>%
  #select(lake_id, site_id, visit, co2_sat_ratio, co2_diffusion_best) %>% print(n=Inf)
  mutate(co2_direction_check = case_when(co2_sat_ratio > 1 & co2_diffusion_best > 0 ~ TRUE, # if supersatured and positive flux
                                         co2_sat_ratio < 1 & co2_diffusion_best < 0 ~ TRUE, # if undersatured and negative flux
                                         TRUE ~ FALSE)) %>% # if dg concentration and flux direction disagree, then FALSE
  summarize(co2_direction_check_true = sum(co2_direction_check),
            co2_direction_check_false = sum(!co2_direction_check),
            co2_direction_check_n = n())

# exploratory plot
dissolved_gas_k %>%
  # only observations with dissolved co2 and co2 diffusion
  filter(!if_any(c(sat_co2, co2_diffusion_best), ~ is.na(.x))) %>% 
  # only those with detectable CO2 diffusion and CO2 > 0
  filter(co2_diffusion_best != 0) %>%
  #select(lake_id, site_id, visit, co2_sat_ratio, co2_diffusion_best) %>% print(n=Inf)
  mutate(co2_direction_check = case_when(co2_sat_ratio > 1 & co2_diffusion_best > 0 ~ TRUE, # if supersatured and positive flux
                                         co2_sat_ratio < 1 & co2_diffusion_best < 0 ~ TRUE, # if undersatured and negative flux
                                         TRUE ~ FALSE)) %>% # if dg concentration and flux direction disagree, then FALSE
  arrange(co2_direction_check) %>%
  ggplot(aes(co2_direction_check, co2_diffusion_best)) +
  #ggplot(aes(co2_direction_check, co2_sat_ratio)) +
  geom_point()

# 3.2 k600
# 103 observations
dissolved_gas_k %>%
  filter(!is.na(k_co2_600) | !is.na(k_ch4_600)) %>%
  summarize(n = n())


dissolved_gas_k %>%
  select(lake_id, site_id, visit, 
         ch4_sat_ratio, ch4_diffusion_best, k_ch4_600,
         co2_sat_ratio, co2_diffusion_best, k_co2_600) %>%
  filter(!is.na(k_co2_600) | !is.na(k_ch4_600)) %>%
  summarize(n = n())

# lit values
read_xlsx(paste0("scripts/analysis/data_paper/",
                          "27_2020_729_MOESM5_ESM.xlsx"), skip = 1) %>%
  janitor::clean_names() %>%
  rename(method = method_g_gas_injection_c_chamber_e_eddy_covariance_m_mass_balance) %>%
  filter(method == "C") %>%
  mutate(k600_reported_or_calculated = as.numeric(k600_reported_or_calculated)) %>%
  #select(lake_name, k600_reported_or_calculated) %>% print(n=Inf)
  group_by(lake_name) %>%
  summarize(k_mean = mean(k600_reported_or_calculated, na.rm = TRUE)) %>% print(n=Inf)





