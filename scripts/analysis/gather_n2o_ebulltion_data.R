# BACKGROUND----
## Dissolved gas
# Data file "7_site_data.csv" in the SuRGE data paper package
# contains deep and shallow n2o, but is missing the 2016 data. The
# 2016 dissolved gas numbers in eqAreaData were calculated in the 
# mulitressurvey RStudio project using the wrong BP. Below we strip
# those data from eqAreaData and recalculate. The corrected values
# are added to data file "7_site_data.csv" in writeDataFiles.R.

## k600
# Data file "4_emission_rate_points.csv" contains k600, but not for 2016.
# Will calculate below and write to data file 4 in writeDataFiles.R.

## n2o ebullition
# Not included in data paper files. n2o_ebullition in the dat object
# (surge repo) was calculated using (ebullitionMassFluxFunction.R) which 
# permutes missing N2O ppm values from other sites in the lake, or if no 
# other data from the lake, it uses the mean  of all trap samples in the 
# project. Here we will recalculate n2o_ebullition using the trap_n2o_ppm 
# values in the trap_gas data frame. 

## trap gas composition
# Not included in data paper files. Will retrieve 2016 data from eqAreaData.
# The SuRGE gas trap data are in gc_lakeid_agg.

## new files
# will write a new file "n2o_ebullition_trap_gas.csv" with the trap gas 
# composition and n2o_ebullition for all sites with trap gas data. This will
# be used in n2o_ebullition RStudio project.

 



# MODIFY N2O EBULLITION FUNCTION----
# modify the mass.rate function to use the trap_n2o_ppm values in the trap_gas data frame
# Function for calculating mass flux rate--                  
mass.rate.n2o <- function(X1, choice1){
  # extract trap gas n2o
  trap_n2o.ppm <- X1 %>%
    pull(trap_n2o_ppm)
  
  # barometric pressure needed: n=PV/RT
  bp <- ifelse(is.na(mean(X1$atm_pressure, na.rm=TRUE)),
               1,
               mean(X1$atm_pressure, na.rm=TRUE)/760)
  
  # temperature needed
  gas.temp <- ifelse(is.na(X1$air_temperature),
                     273.15 + 20, # assume 20C if not measured
                     273.15 + X1$air_temperature)
  
  # convert 1mL to moles
  mL.to.mmoles <- ((bp*0.001)/(0.082058 * gas.temp)) * 1000      #1mL = 0.001L; *1000 to convt to mmol       
  
  # convert mmoles to mg
  if(choice1 == "n2o") {mg.gas <- mL.to.mmoles * 44 * (trap_n2o.ppm/1000000)}  #44mg/mmole
  
  # calculate rate
  mass.flux.rate <- mg.gas * X1$eb_ml_hr_m2 #bubble rate in mg ch4-co2-n2o /hour/m2
  
  # return mass flux rate in mg ch4-co2-n2o /hour/m2
  mass.flux.rate
}

# we need trap_n2o_ppm, eb_ml_hr_m2, atm_pressure, and air_temperature to 
# recalculate n2o_ebullition. We also want the trap composition data in the df.

# N2O EBULLITION AND TRAP GAS COMPOSITION DATA----
## 2016 DATA----
# for 2016 we can pull these input data from eqAreaData
load(paste0(userPath, "data/CIN/2016_survey/eqAreaData.RData")) # loads eqAreaData

dat_2016_n2o <- left_join(
  eqAreaData, air_temp_2016
) %>%  
  as_tibble() %>%
  janitor::clean_names() %>%
  filter(
    # remove extra Acton Lake observations
    !(lake_name %in% c("Acton Lake Aug", "Acton Lake July", "Acton Lake Oct")),
    # only sampled sites
    eval_status == "sampled",
    # only sites with n2o concentration and volumetric eb data
    # 1 site with N2O but not volumetric eb, weird.
    !is.na(trap_n2o_ppm), 
    !is.na(eb_ml_hr_m2) 
  ) %>% 
  select(
    lake_name, site_id, visit, # identifiers
    eb_ml_hr_m2, # volumetric ebullition
    br_prssr, # barometric pressure (mm Hg)
    air_temp, air_temp_units,
    contains("trap"), -contains("extn"),
    -trap_deply_dt_tm, -trap_rtrv_dt_tm 
  ) %>%
  rename(
    atm_pressure = br_prssr, # (mm Hg)
    air_temperature = air_temp,
    air_temperature_units = air_temp_units
  ) %>%
  mutate(
    # identifiers
    visit = 1,
    site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id)),
    site_id = as.character(site_id),
    across(everything(), ~ifelse(is.nan(.x), NA, .x))
  ) %>%
  # convert lake_name to lake_id
  left_join(
    lake.list.2016 %>% select(lake_id, eval_status_code_comment), 
    by = c("lake_name" = "eval_status_code_comment")
  ) %>%
  # restrict to fields present in SuRGE data
  select(-lake_name) %>%
  # move lake_id, site_id, and vist to first columns
  relocate(lake_id, site_id, visit, contains("trap_n2o"))

janitor::get_dupes(dat_2016_n2o, lake_id, site_id, visit) # no duplicates
dim(dat_2016_n2o) # 236



## SURGE DATA----
# for the surge data, we can pull this from the eb_data object
dat_surge_n2o <- eb_data %>%
  # some records with no data outside identifiers, these
  # probably reflect unsampled sites. Also have sites with no N2O
  # data. Maybe bad standard curve or sample volume too small for analyses.
  # remove these
  filter(!is.na(n2o_ppm)) %>% 
  # we also have 5 cases of n2o_ppm == 0. Replace these with min reported value
  # excluding 0 from the calculation of min, but then replacing 0 with min value.
  # probably need to consider N2O MDL
  mutate(
    n2o_ppm = case_when(
      n2o_ppm == 0 ~ min(n2o_ppm[n2o_ppm != 0], na.rm = TRUE),
      TRUE ~ n2o_ppm
    )
    ) %>%
  select(
    -sample_depth_m, -type,
         -contains("trap")
  ) %>%
  # deal with lacustrine etc from Missouri river
  mutate( # move transitional, lacustrine, riverine from lake_id to site_id
    site_id = case_when(grepl("lacustrine", lake_id) ~ paste0(site_id, "_lacustrine"),
                        grepl("transitional", lake_id) ~ paste0(site_id, "_transitional"),
                        grepl("riverine", lake_id) ~ paste0(site_id, "_riverine"),
                        TRUE ~ as.character(site_id)),
    # remove transitional, lacustrine, riverine from lake_id
    # retain character class initially, then convert to numeric.
    lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                        lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                        TRUE ~ lake_id),
    lake_id = as.numeric(lake_id)) %>%
  # append "trap_" to all columns except lake_id, site_id, visit, atm_pressure, and air_temperature
  # air_temperature and eb_ml_hr_m2
  rename_with(
    ~paste0("trap_", .), 
    -c(lake_id, site_id, visit, atm_pressure, air_temperature, eb_ml_hr_m2)
    ) 

janitor::get_dupes(dat_surge_n2o, lake_id, site_id, visit) # no dups
dim(dat_surge_n2o) # 775


## BIND DATA AND RECALCULATE N2O EBULLITION----
dat_n2o <- bind_rows(dat_2016_n2o, dat_surge_n2o) %>%
  mutate(
    n2o_ebullition = mass.rate.n2o(., choice1 = "n2o")
  ) %>%
  mutate(
    n2o_ebullition_units = "mg n2o m-2 h-1"
  ) %>%
  select(
    -eb_ml_hr_m2, 
    -atm_pressure, 
    -air_temperature,
    -air_temperature_units
    )

janitor::get_dupes(dat_n2o, lake_id, site_id, visit) # no dups
dim(dat_n2o) # 1011

# 2016 DISSOLVED GAS AND K DATA----
# Inadvertently omitted from data paper files. 
# Dissolved N2O is needed to calculate n2o diffusion in n2o_eb RStudio project
# merge_data.R.
# Dissolved ch4, dissolved co2, shallow water temperature are needed to 
# calculate k600

# The dissolved gas concentrations were calculated using wrong barometric
# pressure units in the 2016 study. Recalculation requires:

# barometric pressure (kPa) == BrPrssr (check units)
# headspace N2O concentration == dissolved_n2o.ppm
# air N2O == air_n2o.ppm
# source N2O = 0 # used helium
# headspace equilibration temperature == Tmp_C_S same as lake water
# water volume == "H2O_vol"
# headspace volume == "HeVol"



eqAreaData <- eqAreaData %>%
  tibble %>%
  mutate(
    baro =  BrPrssr * (101.325 / 760) # mm Hg to KPa 
  ) %>%
  # eliminate results from original dissolved gas concentrations
  # in the mulitressurvey R Studio project where the wrong BP
  # units were used.
  select(
    -c(dissolved.ch4, dissolved.co2, dissolved.n2o,
       sat.ch4, sat.co2, sat.n2o)
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



# WRITE DATA----
# data file 1: n2o ebullition and trap gas composition
write_csv(
  dat_n2o, 
  "output/n2o_ebullition_trap_gas.csv"
  )



