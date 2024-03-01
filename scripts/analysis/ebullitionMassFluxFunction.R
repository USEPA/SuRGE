
# EBULLITION MASS FLUX FUNCTION------------------------

# Function for calculating mass flux rate--                  
mass.rate <- function(X1, choice1){
  # trap gas data to use if measured values aren't available
  trap_ch4.ppm <- ifelse(is.na(X1$ch4_ppm), mean(X1$ch4_ppm, na.rm=TRUE), X1$ch4_ppm) 
  trap_co2.ppm <- ifelse(is.na(X1$co2_ppm), mean(X1$co2_ppm, na.rm=TRUE), X1$co2_ppm)
  trap_n2o.ppm <- ifelse(is.na(X1$n2o_ppm), mean(X1$n2o_ppm, na.rm=TRUE), X1$n2o_ppm)
  
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
