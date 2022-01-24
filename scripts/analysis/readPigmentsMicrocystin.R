# script for reading chlorophyll, phycocyanin, and microcystin measured
# at Narragansett laboratory.

# Jake needs to locate data and provide additional background information.

# As on 1/20/2022, do data have been uploaded; however, NAR compiled a file
# listing all the samples they have received.  Lets read those in and compare
# to the samples expected.

nar.samples <- read_excel(paste0(userPath,
                                 "data/sampleTrackingSheets//NAR algal indicator//",
                                 "narSampleReceiptList.xlsx"))

# print rows in nar.samples not in chem.samples
# all good (1/21/2022)
setdiff(nar.samples[c("lake_id", "analyte", "sample_type")],
        chem.samples.foo[c("lake_id", "analyte", "sample_type")]) %>% print(n=Inf)


# Have all NAR algae samples in comprehensive sample list been delivered to NAR?
# 1/20/2022. Missing lake 204 samples.  Confirmed missing by NAR.  Checking
# with Raghu
setdiff(chem.samples.foo %>% filter(analyte_group == "algae.nar", 
                                    sample_year != 2018) %>% # 2018 R10 not sent to NAR
          select(lake_id, analyte, sample_type),
        nar.samples[c("lake_id", "analyte", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)

