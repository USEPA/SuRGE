# Prepare 2020 and 2021 data sets for RAPID reporting



chem_fld_2020 <- chem_fld %>%
  filter(format(sample_date, "%Y") == "2020") %>%
  select(-shipping_notes, -phycocyanin) # as of 6/2/22, phycocyanin not available
write.table(x = chem_fld_2020, "output/SuRGE_2020.txt", row.names = FALSE)


chem_fld_2021 <- chem_fld %>% 
  filter(format(sample_date, "%Y") == "2021") %>%
  select(-shipping_notes, -phycocyanin)
write.table(x = chem_fld_2021, "output/SuRGE_2021.txt", row.names = FALSE)
