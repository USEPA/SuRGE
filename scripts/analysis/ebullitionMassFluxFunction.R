
# EBULLITION MASS FLUX FUNCTION------------------------

# Function for calculating mass flux rate--                  
mass.rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4.ppm <- X1 %>%
    mutate(ch4_ppm = case_when(is.na(ch4_ppm) & any(!is.na(ch4_ppm)) ~ mean(ch4_ppm, na.rm=TRUE), # if ch4_ppm is missing for the site, but any others sites in lake have ch4, use lake mean
                               is.na(ch4_ppm) & all(is.na(ch4_ppm)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(ch4_ppm), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ ch4_ppm)) %>% # else use measured value
    pull(ch4_ppm)
  
  trap_co2.ppm <- X1 %>%
    mutate(co2_ppm = case_when(is.na(co2_ppm) & any(!is.na(co2_ppm)) ~ mean(co2_ppm, na.rm=TRUE), # if co2_ppm is missing for the site, but any others sites in lake have ch4, use lake mean
                               is.na(co2_ppm) & all(is.na(co2_ppm)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(co2_ppm), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ co2_ppm)) %>% # else use measured value
    pull(co2_ppm)
  
  trap_n2o.ppm <- X1 %>%
    mutate(n2o_ppm = case_when(is.na(n2o_ppm) & any(!is.na(n2o_ppm)) ~ mean(n2o_ppm, na.rm=TRUE), # if n2o_ppm is missing for the site, but any others sites in lake have ch4, use lake mean
                               is.na(n2o_ppm) & all(is.na(n2o_ppm)) ~ mean(gc_lakeid %>% filter(type == "trap") %>% pull(n2o_ppm), na.rm = TRUE), # if no trap samples collected from the lake, use project mean
                               TRUE ~ n2o_ppm)) %>% # else use measured value
    pull(n2o_ppm)
  
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
  if(choice1 == "ch4") {mg.gas <- mL.to.mmoles * 16 * (trap_ch4.ppm/1000000)}  #16mg/mmole
  if(choice1 == "co2") {mg.gas <- mL.to.mmoles * 44 * (trap_co2.ppm/1000000)}  #44mg/mmole
  if(choice1 == "n2o") {mg.gas <- mL.to.mmoles * 44 * (trap_n2o.ppm/1000000)}  #44mg/mmole
  
  # calculate rate
  mass.flux.rate <- mg.gas * X1$eb_ml_hr_m2 #bubble rate in mg ch4-co2-n2o /hour/m2
  
  # return mass flux rate in mg ch4-co2-n2o /hour/m2
  mass.flux.rate
}
