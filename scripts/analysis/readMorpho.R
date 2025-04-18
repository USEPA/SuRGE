# Jeff Hollister used lakeMorpho to generate morpho indices for SuRGE lakes.
# Read in data.  All units are in m, m2, m3, or unitless  (i.e. ratio)


# Read from surge_morpho github repo
morpho <- read.csv(paste0(userPath, "data/siteDescriptors/all_lakes_lakemorpho.csv")) %>% # do not use outdated spreadsheet
  janitor::clean_names() %>% # lake_id is integer
  as_tibble() %>% 
  # calculate additional metrics. Casas-Ruiz et al 2021
  mutate(circularity = case_when(
    # major_axis  is defined as the longest line intersecting the convex hull formed 
    # around its polygon while passing through its center. In contrast to max_length, 
    # its value represents the distance across a lake without regard to land-water 
    # configuration. max_length is the longest open water distance within a lake 
    !is.na(major_axis) ~ surface_area / (pi * (major_axis/2)^2), # use major_axis if available
    !is.na(major_axis) & is.na(max_length) ~ surface_area / (pi * (max_length/2)^2), # max_length if no major axis
    TRUE ~ NA_real_),
    dynamic_ratio = shoreline_length / mean_depth,
    littoral_fraction = 1 - ((1 - (3/max_depth))^((max_depth/mean_depth) - 1))) %>%
  # replace NaN, Inf with NA
  mutate(across(where(is.numeric), ~na_if(., is.infinite(.))))


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
