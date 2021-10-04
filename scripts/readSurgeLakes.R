# this script reads in the SuRGE survey design, including site weights.

lake.list <- readxl::read_excel(paste0(userPath, 
                                       "surgeDsn/SuRGE_design_20191206_eval_status.xlsx")) %>%
  janitor::clean_names()


# How many chem samples should CIN lab expect for 2021 field season
lake.list %>% filter(sample_year == 2021,
                     lab != "ADA") %>% #ADA running their own chemistry
  summarize(lakes.2021 = n())

# 31 lakes, but Oahe and Francis-Case split into three lakes, therefore
# should have samples from 35 lakes.  Each lake has a minimum of two samples,
# therefore have a minimum of 70 samples.  Assume 1/3 of lakes have dups and 
# blanks for another (35*0.3*2 = 21) 21 samples.  Sample totals should be
# close to 90.

