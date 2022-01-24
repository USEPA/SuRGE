# merge chem21, chem18, and ada.nutrients

all.nutrients <- full_join(chem18, chem21) %>%
  full_join(ada.nutrients)

# do we want a column(s) that indicates the year and/or lab?

colnames(ada.nutrients)
colnames(chem18)
colnames(chem21)
# chem21 and chem18 are identical; ada.nutrients has 4 add. columns for no3

