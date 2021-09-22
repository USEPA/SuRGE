chemCoc <- read_excel(paste0(userPath, 
                             "data/chemistry/mailySampleIds.xlsx")) %>%
  clean_names(.)

chemCoc

janitor::get_dupes(chemCoc, lab_id) # no duplicates
