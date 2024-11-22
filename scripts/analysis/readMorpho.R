# Jeff Hollister used lakeMorpho to generate morpho indices for SuRGE lakes.
# Read in data.  All units are in m, m2, m3, or unitless  (i.e. ratio)

morpho <- read.csv(paste0(userPath, "data/siteDescriptors/all_lakes_lakemorpho.csv")) %>%
  janitor::clean_names() %>% # lake_id is integer
  as_tibble() %>% 
  filter(!(lake_id %in% c(15, 183, 194, 212, 213, 56, 185, 325, 9, 244, 285))) %>% # exclude lakes not sampled
  # calculate additional metrics. Cass Ruiz et al 2021
  mutate(circularity = case_when(
    # major_axis  is defined as the longest line intersecting the convex hull formed 
    # around its polygon while passing through its center. In contrast to lakeMaxLength, 
    # its value represents the distance across a lake without regard to land-water 
    # configuration.
    !is.na(major_axis) ~ surface_area / (pi * (major_axis/2)^2), # use major_axis if available
    # lakeMaxLength is the longest open water distance within a lake 
    is.na(major_axis) ~ surface_area / (pi * (max_length/2)^2), # max_length if needed
    TRUE ~ 9999999999999), # error flag
  dynamic_ratio = shoreline_length / mean_depth,
  littoral_fraction = 1 - ((1 - (3/max_depth))^((max_depth/mean_depth) - 1))) 

# check for error flag
morpho %>% filter(circularity == 9999999999999)
  
# need to add depth estimates for 2016 lakes missing this measurement (1001, 1002, 1012, 1013, 1016, 1017)
measured.depths <- 
  # bring in depth data from fld_sheet as a check/substitute for lake_morpho data
  bind_rows(fld_sheet %>% # convert character lake_id to numeric for Missouri River
              mutate(lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                                         lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                                         TRUE ~ lake_id),
                     lake_id = as.numeric(lake_id)),
            dat_2016) %>% 
  # warning because no depth entered to 253, a R10 lake.
  # this can be fixed.
  summarise(mean_depth_measured = mean(site_depth, na.rm = TRUE),
            max_depth_measured = max(site_depth, na.rm = TRUE),
            .by = lake_id) %>%
  select(lake_id, mean_depth_measured, max_depth_measured) %>%
  # remove columns where both depth columns are NA, Inf, NaN, or combo thereof
  rowwise() %>%
  filter(any(is.finite(c_across(c(mean_depth_measured, max_depth_measured))))) 

# warnings are ok. the final filter addresses the problem.

# join morpho and field measurements
dim(morpho) # 145
dim(measured.depths) # 140
morpho <- left_join(morpho, measured.depths)
dim(morpho) # 145


# Inspect for correlations.  There are lots.  
morpho %>%
  select(where(is.numeric), -lake_id) %>%
cor(use = "pairwise.complete.obs") %>% 
  corrplot(method = "number") 

# discussion from Cass Ruiz et al.2021 below
# littoral fraction and mean_depth were strongly correlated (rho = 0.99)
# and littoral fraction was omitted.
#Lake area and volume were also included in
#the elastic net models. Lake area was included because
#there is a size-scaling of lake morphometry, such that
#larger lakes are more fractal, less circular and more
#dish-like (figure 2). Thus, by including lake area in the
#models we can separately assess the influence of lake
#size and morphometry on the concentration of the
#different C species. Lake volume was included because
#larger lakes have a higher dilution capacity and generally
#present longer water residence times.

# here are the morpho variables used in Cass Ruiz
# surface_area and volume are correlated at rho=0.79
morpho %>%
  select(surface_area, volume, shoreline_development, circularity,
         dynamic_ratio) %>%
  cor(use = "pairwise.complete.obs") %>% 
  corrplot(method = "number") 
