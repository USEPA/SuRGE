inner_join(
  # emissions aggregated to lake scale and annualized
  emissions_agg_annual %>%
    filter(temp_source == "mixed_layer") %>%
    select(lake_id, visit, contains("total")),
  # IPCC climate zones
  inner_join(surge_climate,
             # create table of IPCC default emission factors taken from Lovelock et al
             # tables 7.9 and 7.13
             tribble(~climate, 
                     ~ch4_total_lake_annual_ipcc, ~ch4_total_lake_annual_ipcc_lwr, ~ch4_total_lake_annual_ipcc_upr , ~ch4_total_lake_annual_ipcc_units, 
                     ~co2_total_lake_annual_ipcc, ~co2_total_lake_annual_ipcc_lwr, ~co2_total_lake_annual_ipcc_upr, ~co2_total_lake_annual_ipcc_units,
                     
                     "boreal", 
                     13.6, 7.3, 19.9, "kg ch4 ha-1 y-1", 
                     0.94, 0.84, 1.05, "tonnes co2-c ha-1 y-1",
                     
                     "cool temperate", 
                     54, 48.3, 59.5, "kg ch4 ha-1 y-1", 
                     1.02, 1, 1.04, "tonnes co2-c ha-1 y-1",
                     
                     "warm temperate dry", 
                     150.9, 133.3, 168.1, "kg ch4 ha-1 y-1", 
                     1.7, 1.66, 1.75, "tonnes co2-c ha-1 y-1",
                     
                     "warm temperate moist", 
                     80.3, 74, 86, "kg ch4 ha-1 y-1", 
                     1.46, 1.44, 1.48, "tonnes co2-c ha-1 y-1",
                     
                     "tropical dry/montane", 
                     283.7, 261.9, 305.8, "kg ch4 ha-1 y-1", 
                     2.95, 2.86, 3.04, "tonnes co2-c ha-1 y-1",
                     
                     "tropical moist/wet", 
                     141.1, 131.1, 152.7,"kg ch4 ha-1 y-1", 
                     2.77, 2.71, 2.84, "tonnes co2-c ha-1 y-1") %>% # close tribble
               # now convert units to mg ch4|co2 m-2 h-1, same as throughout this project
               mutate(across(contains("ch4") & !contains("units"), ~.x * ((1000 * 1000) / (365 * 24 * 10000))),
                      ch4_total_lake_annual_ipcc_units = "mg ch4 m-2 h-1",
                      across(contains("co2") & !contains("units"), ~.x * ((1000 * 1000 * 1000) / (10000 * 365 * 24))),
                      co2_total_lake_annual_ipcc_units = "mg co2 m-2 h-1")
             ) # close inner join climate + tribble
  ) %>% # close inner join emissions_ag + IPCC data
  select(-contains("unit"), -lake_id, -visit) %>% # everything in mg ch4|co2 m-2 h-1
  # 2/27/2025 SuRGE CO2 not yet annualized. Not sure what activation energy to use
  select(-contains("co2")) %>%
  mutate(source = "IPCC") %>% # non-sensical variable used for legend below
  # dot plot
  # pivot_longer(!climate) %>%
  # mutate(source = case_when(grepl("ipcc", name) ~ "IPCC",
  #                           TRUE ~ "SuRGE"),
  #        name = gsub("_ipcc", "", name)) %>%
  # ggplot(aes(climate, value)) +
  # geom_point(aes(colour = source))
  ggplot(aes(climate, ch4_total_lake_annual)) +
  geom_boxplot() +
  #geom_point(aes(climate, ch4_total_lake_annual_ipcc, color = source)) + 
  geom_pointrange(aes(x = climate, y = ch4_total_lake_annual_ipcc, 
                      ymin = ch4_total_lake_annual_ipcc_lwr, ymax = ch4_total_lake_annual_ipcc_upr,
                      color = source)) +
  scale_color_manual(values = "red") +
  scale_y_log10() +
  ylab("Total CH4 emissions (mg CH4 m-2 h-1)") +
  ggtitle("Annualized SuRGE emissions vs. IPCC default") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))
  ggsave("output/figures/SuRGE_vs_IPCC.tiff")

  
