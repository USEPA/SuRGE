
dissolved_gas_k <- 
  left_join(
    # dissolved gas first. Can only calculate k if data on dissolved gas + emission
    dissolved_gas %>%
      filter(sample_depth == "shallow") %>% # only shallow for k
      mutate(
        # 1000L = 1m3, 44g = 1 mol co2, 1000mg = 1g
        co2_star = (dissolved_co2 - sat_co2) * (1000 * 1000 * 44), # mg/m3
        ch4_star = (dissolved_ch4 - sat_ch4) * (1000 * 1000 * 16)), # mg/m3
    
    # join with emission rate estimates
    emissions %>% 
      select(lake_id, site_id, visit, 
             co2_diffusion_best, # mg co2 m-2 h-1
             ch4_diffusion_best) # mg co2 m-2 h-1
  ) %>% # complete left_join
  # calculate k and k600
  mutate(k_co2 = case_when(
    # no k if no emission (either 0 or NA)
    co2_diffusion_best == 0 | is.na(co2_diffusion_best) ~ NA_real_, 
    # if positive emissions but undersaturated co2 (co2_star < 0), then don't calculate k
    co2_diffusion_best > 0 & co2_star < 0 ~ NA_real_,
    # if negative emissions but supersaturated co2 (co2_star > 0), then don't calculate k
    co2_diffusion_best < 0 & co2_star > 0 ~ NA_real_,
    TRUE  ~ (co2_diffusion_best / co2_star) * 24), # m/d
    
    k_ch4 = case_when(
      # no k if no emission (either 0 or NA)
      ch4_diffusion_best == 0 | is.na(ch4_diffusion_best) ~ NA_real_,
      ch4_diffusion_best != 0 ~ (ch4_diffusion_best / ch4_star) * 24), # m/d
    k_co2_units = "m d-1",
    k_ch4_units = "m d-1",
    sc_co2 = 1923.6 - 125.06*water_temperature + 4.3773*water_temperature^2 - 0.085681*water_temperature^3 + 0.00070284*water_temperature^4, #schmidt number (Wanninkhof 2014)
    sc_ch4 = 1909.4 - 120.78*water_temperature + 4.1555*water_temperature^2 - 0.080578*water_temperature^3 + 0.00065777*water_temperature^4, #schmidt number (Wanninkhof 2014)   
    # sc_co2 = 1911.1 - 118.11*water_temperature + 3.4527*water_temperature^2 - 0.04132*water_temperature^3, # schmidt number (Wanninkhof et al 1992)
    # sc_ch4 = 1897.8 - 114.28*water_temperature + 3.2902*water_temperature^2 - 0.039061*water_temperature^3, # schmidt number (Wanninkhof et al 1992)
    k_co2_600 = k_co2 * (1 / (sc_co2 / 600)^-(2/3)), # m/d
    k_ch4_600 = k_ch4 * (1 / (sc_ch4 / 600)^-(2/3)), # m/d
    k_co2_600_units = "m d-1",
    k_ch4_600_units = "m d-1")


p <- ggplot(dissolved_gas_k, 
            # labels specified here are for ggplotly tooltip
            aes(label = lake_id, label2 = site_id, label3 = visit)) + 
  geom_point(aes(k_ch4, k_co2))

p <- ggplot(dissolved_gas_k, 
            aes(label = lake_id, label2 = site_id, label3 = visit)) +
  geom_point(aes(k_ch4_600, k_co2_600))



ggplotly(p)

# inspect some outliers
gc_lakeid %>% filter(lake_id == "188", site_id == 9)

# check what the k would be for the high diffusion site at Comins
k_ch4_comins=48.04/916.4949
k_ch4_600= k_ch4_comins * (1/(562.2937/600)^-2/3)
