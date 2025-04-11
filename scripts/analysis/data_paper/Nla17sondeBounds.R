source("scripts/analysis/readNla17.R")


nla17_profiles = read.csv(paste0(userPath, "/data/nla17/nla_2017_profile-data.csv")) %>%
  select(SITE_ID, DEPTH, OXYGEN, PH, TEMPERATURE) %>%
  mutate(type = "PROFILE")


summary(nla17_profiles)



nla17_chem_clean = nla17_chemmerge %>%
  filter(!is.na(nla17_result),
         nla17_analyte %in% c("CHLA", "COND", "PH", "TURB")) %>%
  mutate(type = "CHEMISTRY")


table(nla17_chem_clean$nla17_analyte)


nla17_chem_clean %>%
  pivot_wider(values_from = nla17_result, names_from = c(nla17_analyte, nla17_result_units)) %>%
  summary(nla17_chem_clean)



nla17_chem_profiles = nla17_chem_clean %>%
  full_join(nla17_profiles %>%
              pivot_longer(values_to = "nla17_result", names_to = "nla17_analyte",
                           cols = c(OXYGEN, PH, TEMPERATURE)) %>%
              mutate(nla17_result_units = case_when(nla17_analyte == "OXYGEN" ~ "MG/L",
                                                    nla17_analyte == "PH" ~ "STD. UNITS",
                                                    nla17_analyte == "TEMPERATURE" ~ "C")) %>%
              filter(!is.na(nla17_result))) %>%
  mutate(result_log = case_when(nla17_analyte %in% c("CHLA", "COND", "TURB") ~ log1p(nla17_result),
                                TRUE ~ nla17_result),
         analyte_log = case_when(nla17_analyte %in% c("CHLA", "COND", "TURB") ~ paste0("log(", nla17_analyte, ")"),
                                TRUE ~ nla17_analyte))



nla17_summaries = nla17_chem_profiles %>%
  group_by(nla17_analyte, nla17_result_units) %>%
  summarize(min = min(nla17_result, na.rm = T),
            median = median(nla17_result, na.rm = T),
            max = max(nla17_result, na.rm = T))


print(nla17_summaries)


logged_density_vars = ggplot() +
  geom_density(data = nla17_chem_profiles %>% filter(nla17_analyte %in% c("CHLA", "COND", "TURB")), 
               aes(nla17_result, fill = type), alpha = 0.5) +
  geom_text(data = nla17_summaries %>% filter(nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("min =", min, " ")), hjust = 1, vjust = 1.5) +
  geom_text(data = nla17_summaries %>% filter(nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("median =", median, " ")), hjust = 1, vjust = 3) +
  geom_text(data = nla17_summaries %>% filter(nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("max =", max, " ")), hjust = 1, vjust = 4.5) +
  facet_wrap(~paste0(nla17_analyte, " (", nla17_result_units, ")"), scales = "free", nrow = 1) +
  scale_x_continuous(trans = c("log1p"), breaks = c(0, 10, 100, 1000, 10000),
                     minor_breaks = c(seq(0, 10, by = 1), seq(10, 100, by = 10), seq(100, 1000, by = 100), 
                                      seq(1000, 10000, by = 1000), seq(10000, 100000, by = 10000))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme(legend.position = "none")



unlog_density_vars = ggplot() +
  geom_density(data = nla17_chem_profiles %>% filter(!nla17_analyte %in% c("CHLA", "COND", "TURB")), 
               aes(nla17_result, fill = type), alpha = 0.5) +
  geom_text(data = nla17_summaries %>% filter(!nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("min =", min, " ")), hjust = 1, vjust = 1.5) +
  geom_text(data = nla17_summaries %>% filter(!nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("median =", median, " ")), hjust = 1, vjust = 3) +
  geom_text(data = nla17_summaries %>% filter(!nla17_analyte %in% c("CHLA", "COND", "TURB")), 
            aes(x = Inf, y = Inf, label = paste("max =", max, " ")), hjust = 1, vjust = 4.5) +
  facet_wrap(~paste0(nla17_analyte, " (", nla17_result_units, ")"), scales = "free", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw()



ggpubr::ggarrange(logged_density_vars, unlog_density_vars,
                  align ="h", common.legend = F, nrow = 2, ncol = 1)
