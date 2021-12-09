# SCRIPT FOR READING TAXONOMY DATA FROM GULF BREEZE

# No data as of 12/8/2021, however GB compiled a list of samples received.


# Sample Inventory Audit.
# Compare samples received to those in comprehensive sample list
gb.samples <- read_excel(paste0(userPath,
                                "data/sampleTrackingSheets//GB taxonomy//",
                                "taxonomyPhysiologySampleReceiptList.xlsx"))

# print rows in gb.samples not in chem.samples
# all samples record by GB are in comprehensive sample list. Good.
setdiff(gb.samples[c("lake_id", "analyte")],
        chem.samples.foo[c("lake_id", "analyte")]) %>% print(n=Inf)

# Have all GB algae samples in comprehensive sample list been delivered to GB?
# Print rows from comprehensive sample list not in GB sample received list.
# Missing a bunch, but GB sample receipt list doesn't contain any 2020
# samples.  Have GB update file, then revisit.  
setdiff(chem.samples.foo %>% filter(analyte_group == "algae.gb", 
                                    sample_year != 2018) %>% # 2018 R10 not sent to GB
          select(lake_id, analyte),
        gb.samples[c("lake_id", "analyte")]) %>%
  arrange(lake_id) %>% print(n=Inf)
