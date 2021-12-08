# script for reading chlorophyll, phycocyanin, and microcystin measured
# at Narragansett laboratory.

# Jake needs to locate data and provide additional background information.

# As on 12/7/2021, do data have been uploaded; however, NAR compiled a file
# listing all the samples they have received.  Lets read those in and compare
# to the samples expected.

nar.samples <- read_excel(paste0(userPath,
                                 "data/sampleTrackingSheets//NAR algal indicator//",
                                 "narSampleReceiptList.xlsx"))

# print rows in nar.samples not in chem.samples
# all samples record by NAR are in comprehensive sample list. Good.
setdiff(nar.samples[c("lake_id", "analyte", "sample_type")],
        chem.samples.foo[c("lake_id", "analyte", "sample_type")]) %>% print(n=Inf)


# Have all NAR algae samples in comprehensive sample list been delivered to NAR?
# Print rows from comprehensive sample list not in NAR sample received list.
# Missing a bunch, but I see nar sample receipt list doesn't contain any 2020
# samples.  Have NAR update file, then revisit.  I inspected the 2021 samples
# not in NAR inventory and communicated to Stephen Shivers.  Stephen confirmed
# that he analyzed the samples and will update the Excel file.
setdiff(chem.samples.foo %>% filter(analyte_group == "algae.nar", 
                                    sample_year != 2018) %>% # 2018 R10 not sent to NAR
          select(lake_id, analyte, sample_type),
        nar.samples[c("lake_id", "analyte", "sample_type")]) %>%
  arrange(lake_id) %>% print(n=Inf)

# 148, ADA, 2021, missing
# 291, USGS, 2021, 
# 296,  USGS, 2021
# 297, USGS, 2021
# 317,  USGS, 2021
