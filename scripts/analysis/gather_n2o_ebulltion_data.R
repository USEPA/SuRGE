# BACKGROUND----
## Dissolved gas
# Data file "7_site_data.csv" in the SuRGE data paper package
# contains deep and shallow n2o, but is missing the 2016 data. 

## k600
# Data file "4_emission_rate_points.csv" contains k600, but not for 2016.
# We did not include n2o_ebullition or trap gas data in the data paper 
# files. 



# 2016 DISSOLVED GAS AND K DATA----
# Inadvertently omitted from data paper files. 
# Dissolved N2O is needed to calculate n2o diffusion in n2o_eb RStudio project
# merge_data.R.
# Dissolved ch4, dissolved co2, shallow water temperature are needed to 
# calculate k600

load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData


eqAreaData <- eqAreaData %>%
  mutate(
    baro =  BrPrssr * (101.325 / 760) # mm Hg to KPa 
  )

dat_2016_dissolved <- with(
  eqAreaData, 
  def.calc.sdg(
    inputFile = eqAreaData, 
    volGas = HeVol, volH2O = H2O_vol, 
    baro = baro, # KPa 
    waterTemp = Tmp_C_S, # lake temp
    headspaceTemp = Tmp_C_S, # use lake temp
    eqCO2 = dissolved_co2.ppm, 
    sourceCO2 = 0, # Used He
    airCO2 = 405, # global mean
    eqCH4 = dissolved_ch4.ppm, 
    sourceCH4 = 0, # Used He 
    airCH4 = 1.85, # global mean
    eqN2O = dissolved_n2o.ppm, 
    sourceN2O = 0, # Used He 
    airN2O = 0.33 # global mean
  ) # close def.calc.sdg
) %>% # close `with`
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(
    # remove extra Acton Lake observations
    !(lake_name %in% c("Acton Lake Aug", "Acton Lake July", "Acton Lake Oct")),
    !is.na(dissolved_n2o) # must have dissolved n2o
  ) %>% 
  select(
    lake_name, site_id,
    tmp_c_s, # water temperature
    dissolved_co2, dissolved_ch4, dissolved_n2o,
    co2_sat_ratio, ch4_sat_ratio, n2o_sat_ratio,
    sat_co2, sat_ch4, sat_n2o
  ) %>%
  rename(
    shallow_water_temperature = tmp_c_s,
    # dissolved gas. Only shallow samples in 2016
    dissolved_co2_shallow = dissolved_co2,
    dissolved_ch4_shallow = dissolved_ch4,
    dissolved_n2o_shallow = dissolved_n2o,
    co2_sat_ratio_shallow = co2_sat_ratio,
    ch4_sat_ratio_shallow = ch4_sat_ratio,
    n2o_sat_ratio_shallow = n2o_sat_ratio
  ) %>%
  mutate(
    # identifiers
    visit = 1,
    site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
    site_id = as.character(site_id),
    across(everything(), ~ifelse(is.nan(.x), NA, .x)),
    # Units
    # dissolved gas units
    dissolved_co2_shallow_units = "mol co2 L-1",
    dissolved_ch4_shallow_units = "mol ch4 L-1",
    dissolved_n2o_shallow_units = "mol n2o L-1",
    co2_sat_ratio_shallow_units = "dimensionless",
    ch4_sat_ratio_shallow_units = "dimensionless",
    n2o_sat_ratio_shallow_units = "dimensionless"
  ) %>%
  # convert lake_name to lake_id
  left_join(
    lake.list.2016 %>% select(lake_id, eval_status_code_comment), 
    by = c("lake_name" = "eval_status_code_comment")
  ) %>%
  # restrict to fields present in SuRGE data
  select(-lake_name) %>%
  # join with dat$ch4_diffusion_best and dat$co2_diffusion_best
  left_join(
    dat %>% select(
      lake_id, site_id, visit,
      ch4_diffusion_best, co2_diffusion_best,
      ch4_diffusion_units, co2_diffusion_units
    )
  ) %>%
  # calculate k600
  mutate(
    # dissolved and saturated in mol L^-1
    # 1000L = 1m3, 44g = 1 mol co2, 16g ch4 = 1mol ch4, 1000mg = 1g
    co2_star = (dissolved_co2_shallow - sat_co2) * (1000 * 1000 * 44), # mg/m3
    ch4_star = (dissolved_ch4_shallow - sat_ch4) * (1000 * 1000 * 16), # mg/m3

    k_co2 = case_when(
      # no k if no emission (either 0 or NA)
      co2_diffusion_best == 0 | is.na(co2_diffusion_best) ~ NA_real_, 
      # if positive emissions but undersaturated co2 (co2_star < 0), then don't calculate k
      co2_diffusion_best > 0 & co2_star < 0 ~ NA_real_,
      # if negative emissions but supersaturated co2 (co2_star > 0), then don't calculate k
      co2_diffusion_best < 0 & co2_star > 0 ~ NA_real_,
      TRUE  ~ (co2_diffusion_best / co2_star) * 24 # m/d
      ), 
    
    k_ch4 = case_when(
      # no k if no emission (either 0 or NA)
      ch4_diffusion_best == 0 | is.na(ch4_diffusion_best) ~ NA_real_,
      ch4_diffusion_best != 0 ~ (ch4_diffusion_best / ch4_star) * 24 # m/d
      ), 
    
    k_co2_units = "m d-1",
    k_ch4_units = "m d-1",
    sc_co2 = 1923.6 - 125.06*shallow_water_temperature + 4.3773*shallow_water_temperature^2 - 0.085681*shallow_water_temperature^3 + 0.00070284*shallow_water_temperature^4, #schmidt number (Wanninkhof 2014)
    sc_ch4 = 1909.4 - 120.78*shallow_water_temperature + 4.1555*shallow_water_temperature^2 - 0.080578*shallow_water_temperature^3 + 0.00065777*shallow_water_temperature^4, #schmidt number (Wanninkhof 2014)   
    # sc_co2 = 1911.1 - 118.11*shallow_water_temperature + 3.4527*shallow_water_temperature^2 - 0.04132*shallow_water_temperature^3, # schmidt number (Wanninkhof et al 1992)
    # sc_ch4 = 1897.8 - 114.28*shallow_water_temperature + 3.2902*shallow_water_temperature^2 - 0.039061*shallow_water_temperature^3, # schmidt number (Wanninkhof et al 1992)
    k_co2_600 = k_co2 * (1 / (sc_co2 / 600)^-(2/3)), # m/d
    k_ch4_600 = k_ch4 * (1 / (sc_ch4 / 600)^-(2/3)), # m/d
    k_co2_600_units = "m d-1",
    k_ch4_600_units = "m d-1"
  ) %>% 
  # remove unneeded columns.
  select(
    -shallow_water_temperature,
    -ch4_diffusion_best, -co2_diffusion_best,
    -ch4_diffusion_units, -co2_diffusion_units,
    -co2_star, -ch4_star, -sc_co2, -sc_ch4,
    -sat_co2, -sat_ch4, -sat_n2o,
    -k_co2, -k_ch4,
    -k_co2_units, -k_ch4_units,
    -sc_co2, -sc_ch4
  ) %>%
# move lake_id, site_id, and visit to first columns
relocate(lake_id, site_id, visit)

janitor::get_dupes(dat_2016_dissolved, lake_id, site_id, visit) # no duplicates
dim(dat_2016_dissolved) # 62, 2 sites per lake.





