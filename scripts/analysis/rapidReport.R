# Prepare 2020 and 2021 data sets for RAPID reporting
names(chem_fld)

# outstanding issues:  'fluoride' is sneaking through as column name.
# find and switch to f.


chem_fld_2020 <- chem_fld %>% 
  filter(format(trap_deply_date, "%Y") == "2020") %>%
  select(-cin_shipping_notes, -comment, -eval_status, 
         -contains("trap"),
         -contains("chamb"),
         -contains("air"),
         -contains("check"))
write.table(x = chem_fld_2020, "output/SuRGE_2020.txt", row.names = FALSE)


chem_fld_2021 <- chem_fld %>% 
  filter(format(trap_deply_date, "%Y") == "2021") %>%
  select(-cin_shipping_notes, -comment, -eval_status, 
         -contains("trap"),
         -contains("chamb"),
         -contains("air"),
         -contains("check"))
write.table(x = chem_fld_2021, "output/SuRGE_2021.txt", row.names = FALSE)
