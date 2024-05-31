names(emissions)

# summarize all emissions by site_id
dat %>% 
  select(lake_id, site_id, visit, 
         ch4_ebullition, ch4_diffusion_best, ch4_total,
         co2_ebullition, co2_diffusion_best, co2_total) %>%
  pivot_longer(-c(lake_id, site_id, visit)) %>%
  filter(!is.na(value)) %>% # omit rates that couldn't be calculated
  group_by(name) %>% 
  summarize(n_obs = n(),
            n_lake = length(unique(lake_id)),
            min = min(value), # 0.00001 mg CH4/m2/hr
            max = max(value), # 92 mg CH4/m2/hr
            mean = mean(value), # 3.3 mg CH4/m2/hr
            median = median(value),
            iqr25_prop_diff_ch4 = quantile(value, 1/4, na.rm = TRUE),
            iqr75_prop_diff_ch4 = quantile(value, 3/4, na.rm = TRUE),
            n_positive = sum(value >=0)) %>%
  mutate(prop_positive = n_positive / n_obs)

# name                n_obs   n_lake       min      max     mean    median
# ch4_drate_mg_h_best  1299    105        0.0169    1098.   4.32    1.27   
# ch4_erate_mg_h       1376     90        0         92.1    2.64    0.155  
# ch4_trate_mg_h       1012     86        0.0169    358.    5.64    2.09   
# co2_drate_mg_h_best   556     81        -282.     1702.   120.    55.5    
# co2_erate_mg_h       1374     91        0         10.2    0.117   0.00549
# co2_trate_mg_h        408     64        -282.     1702.   121.    54.6 

# summarize all emissions by lake_id
dat %>% 
  select(lake_id, site_id, visit, 
         ch4_total) %>%
  group_by(lake_id) %>%
  summarize(mean_t_ch4 = mean(ch4_total)) %>%
  arrange(mean_t_ch4)


poo <- dat %>%
  select(lake_id, visit, 
         ch4_ebullition, ch4_diffusion_best, ch4_total,
         co2_ebullition, co2_diffusion_best, co2_total) %>%
  pivot_longer(!(c(lake_id, visit))) %>%
  group_by(lake_id, visit, name) %>%
  summarize(mean_e = mean(value, na.rm = T),
            median_e = median(value, na.rm = T)) %>%
  mutate(lake_id = gsub("_.*$", "", lake_id) %>% as.numeric) %>%
  left_join(., 
          lake.list %>% select(lake_id, lat_dd83, lon_dd83), #fld_sheet %>% select(lake_id, long, lat)
          relationship = "many-to-many") %>%  
  st_as_sf(coords=c("lon_dd83","lat_dd83"),crs=4269) %>%
  ungroup()

plot(st_geometry(poo))

states <- USAboundaries::us_states() %>%
  dplyr::filter(!state_name %in% c("Alaska", "District of Columbia", "Hawaii")) %>% # , "Puerto Rico"
  st_transform(4269) # convert to CONUS Albers

ggplot() + 
  geom_sf(data = states) +
  geom_sf(data = poo %>% filter(name == "ch4_total"), aes(size = mean_e))

  
  
  
  # what are main nutrient values?
  names(dat)
  dat %>%
    select(-matches("units|flags|comment")) %>%
    select(lake_id, matches(paste0(c("ch4|co2|oc", nutrients), collapse = "|"))) %>%
    select(-shoreline_development) %>%
    summarise(across(everything(), ~sum(!is.na(.)))) %>%
    pivot_longer(everything()) %>%
    ggplot(., aes(name, value)) +
    geom_bar(stat = "identity")
  # op, no2_3, tp, tn, nh4, doc, toc
  
  # CORRELATION PLOTS----------------
  # ch4_diff
  # 1.  nutrients.  a little, maybe
  list(dat, dat_agg) %>%
    map(function(x) {
      x %>% select(ch4_diffusion_best, matches("op|no2_3|tp|tn|nh4|doc|toc|so4") & where(is.numeric), -shoreline_development) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  # 2.  algal indicators.  some stuff with dat_agg
  list(dat, dat_agg) %>%
    map(function(x) { 
      x %>% select(ch4_diffusion_best, matches("chla|do_mg") & !matches("comment") & where(is.numeric)) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
    # 3.  morpho and ch4_diff.  weak correlations
  list(dat, dat_agg) %>%
    map(function(x) {  
      x %>% select(ch4_diffusion_best, surface_area, volume, shoreline_development, 
                   circularity, dynamic_ratio) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number")  
      readline(prompt="Press [enter] to proceed")
    })
  
  
  # ch4_ebullition
  # 1.  nutrients and ch4_ebullition.  OK, something here with dat and dat_agg
  list(dat, dat_agg) %>%
    map(function(x) {  
      x %>% select(ch4_ebullition, matches("op|no2_3|tp|tn|nh4|doc|toc|so4") & where(is.numeric), -shoreline_development) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number")
      readline(prompt="Press [enter] to proceed")
    })
  
  # 2.  algal indicators.  ok, yes, dat and dat_agg
  list(dat, dat_agg) %>%
    map(function(x) { 
      x %>% select(ch4_ebullition, matches("chla|do_mg") & !matches("comment") & where(is.numeric)) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  # 3.  morpho.  nothing here.
  list(dat, dat_agg) %>%
    map(function(x) {  
      x %>% select(ch4_ebullition, surface_area, volume, shoreline_development, 
                   circularity, dynamic_ratio) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  

# PLOTS OF PROMISING RELATIONSHIPS
  # 1. emission ~ nutrients
  # hit enter to scroll through figures
  foo_nutrients <- dat %>% select(matches("op|no2_3|tp|tn|nh4|doc|toc|so4") & where(is.numeric), -shoreline_development) %>%
    names
  em_names <- dat %>% select(matches("ch4|co2|n2o") & where(is.numeric)) %>% names
  list(dat, dat_agg) %>%
    map(function(x) {
      for (i in 1:length(em_names)){ # for each emission metric
        for(j in 1:length(foo)) { # for each explanatory variable
          p1 <- ggplot(x, aes(.data[[foo_nutrients[j]]], .data[[em_names[i]]])) +
            geom_point()
          print(p1)
          readline(prompt="Press [enter] to proceed")
        }
      }
    })
  
  # 2. emission ~ algal indicators
  # hit enter to scroll through figures
  foo_algae <- dat %>% select(matches("chla|do_mg") & !matches("comment") & where(is.numeric)) %>%
    names
  list(dat, dat_agg) %>%
    map(function(x) {
      for (i in 1:length(foo)){
        p1 <- ggplot(x, aes(.data[[foo_algae[i]]], ch4_ebullition)) +
          geom_point() 
        print(p1)
        readline(prompt="Press [enter] to proceed")
      }
    }) 
  
  # 3. emission ~ morpho indicators
  # hit enter to scroll through figures
  foo_morpho <- c(surface_area, volume, shoreline_development, 
                               circularity, dynamic_ratio)
  list(dat, dat_agg) %>%
    map(function(x) {
      for (i in 1:length(foo)){
        p1 <- ggplot(x, aes(.data[[foo_algae[i]]], ch4_ebullition)) +
          geom_point() 
        print(p1)
        readline(prompt="Press [enter] to proceed")
      }
    }) 

  
# Assess data availability----
  
# missing a fair number of deep chem values, mostly using shallow in stats
# no DOC or anions in 2020/2018
  dat_agg %>% select(lake_id, visit,
                     ch4_ebullition, ch4_diffusion_best,
                     # algae
                     deep_do_mg, shallow_chla_lab, shallow_do_mg, 
                     # chemistry
                     deep_doc, deep_nh4, deep_no2_3, deep_op, deep_so4, 
                     deep_tn, deep_toc, deep_tp, shallow_doc, shallow_nh4, shallow_no2_3, 
                     shallow_op, shallow_so4, shallow_tn, shallow_toc, shallow_tp, 
                     # morpho
                     surface_area, volume, shoreline_development, 
                     circularity, dynamic_ratio) %>%
    summarize(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(-c(lake_id, visit)) %>%
    arrange(value) %>%
    ggplot(aes(name, value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle =45))
  
  
# MODELS----------
# prediction variables
  algae_vars <- c("deep_do_mg", "shallow_chla_lab", "shallow_do_mg")
  # missing a fair number of deep chem values, mostly using shallow here
  chem_vars <- c("shallow_doc", "shallow_nh4", "shallow_no2_3", 
                 "shallow_op", "shallow_so4", "shallow_tn", "shallow_toc", "shallow_tp")
  morpho_vars <- c("surface_area", "volume", "shoreline_development", # see inspectMeasurementValues.R for correlations
                   "circularity", "dynamic_ratio")
  
# ch4.ebullition------
# select data with no missing values to enable stepwise model selection
  ch4.ebullition.dat <- dat_agg %>%
    select(ch4_ebullition, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
    na.omit
  nrow(ch4.ebullition.dat) # 104 observations
  
  m1 <- lm(as.formula(paste("ch4_ebullition ~ ", paste(c(algae_vars, chem_vars, morpho_vars), collapse = " + "))), data = ch4.ebullition.dat) %>%
    step
summary(m1)
anova(m1)
plot(m1)
effect_plot(m1, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shallow_so4)

  
m1.log <- lm(as.formula(paste("ch4_ebullition ~ ", paste0("log(",c(algae_vars, chem_vars, morpho_vars),")", collapse = " + "))), data = ch4.ebullition.dat) %>%
  step
summary(m1.log)
anova(m1.log)
plot(m1.log) # not so good
effect_plot(m1.log, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shallow_so4)

m1.log.log <- lm(as.formula(paste("log(ch4_ebullition) ~ ", paste0("log(",c(algae_vars, chem_vars, morpho_vars),")", collapse = " + "))), data = ch4.ebullition.dat) %>%
  step
summary(m1.log.log)
anova(m1.log.log)
plot(m1.log.log) # not so good
effect_plot(m1.log.log, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1.log.log, pred = shallow_so4, interval = TRUE, rug = TRUE, plot.points = TRUE)

# with p-value selection
m_e_ch4 <- as.formula(paste("ch4_ebullition ~ ", paste(c(algae_vars, chem_vars, morpho_vars), collapse = " + ")))
res <- stepwise(formula = m_e_ch4,
                data = ch4.ebullition.dat,
                type = "linear",
                strategy = "bidirection",
                metric = c("SL"),
                sls = 0.05)
res # shallow_chla and shallow_so4
# refit with lm for plotting
m1 <- lm(m_e_ch4, data = ch4.ebullition.dat) %>% step
effect_plot(m1, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)



# ch4.diffusion------
# select data with no missing values to enable stepwise model selection
ch4.diffusion.dat <- dat_agg %>%
  select(ch4_diffusion_best, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
  na.omit
nrow(ch4.diffusion.dat) # 102 observations

m_d_ch4 <- as.formula(paste("ch4_diffusion_best ~ ", paste(c(algae_vars, chem_vars, morpho_vars), collapse = " + ")))
res <- stepwise(formula = m_d_ch4,
                data = ch4.diffusion.dat,
                type = "linear",
                strategy = "bidirection",
                metric = c("SL"),
                sls = 0.05)
res # shallow_chla and shoreline development
# refit with lm for plotting
m1 <- lm(as.formula(paste("ch4_diffusion_best ~ ", paste(c("shallow_chla_lab", "shoreline_development"), collapse = " + "))), data = ch4.diffusion.dat) %>%
  step
summary(m1)
anova(m1)
plot(m1)
effect_plot(m1, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shoreline_development, interval = TRUE, rug = TRUE, plot.points = TRUE)


m1.log <- lm(as.formula(paste("ch4_diffusion_best ~ ", paste0("log(",c(algae_vars, chem_vars, morpho_vars),")", collapse = " + "))), data = ch4.diffusion.dat) %>%
  step
summary(m1.log)
anova(m1.log)
plot(m1.log) # not so good
effect_plot(m1.log, pred = shallow_chla_lab, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shallow_so4)



# CO2 diffusion-------
co2.diffusion.dat <- dat_agg %>%
  select(co2_diffusion_best, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
  na.omit
nrow(co2.diffusion.dat) # 103 observations

m_d_co2 <- as.formula(paste("co2_diffusion_best ~ ", paste(c(algae_vars, chem_vars, morpho_vars), collapse = " + ")))
res <- stepwise(formula = m_d_co2,
                data = co2.diffusion.dat,
                type = "linear",
                strategy = "bidirection",
                metric = c("SL"),
                sls = 0.05)
res # shallow_chla and shoreline development
# refit with lm for plotting
m1 <- lm(as.formula(paste("co2_diffusion_best ~ ", paste(c("shallow_do_mg", "shallow_no2_3", "dynamic_ratio", "surface_area", "circularity"), collapse = " + "))), data = co2.diffusion.dat) %>%
  step
summary(m1)
anova(m1)
plot(m1)
effect_plot(m1, pred = shallow_do_mg, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = dynamic_ratio, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = surface_area, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = circularity, interval = TRUE, rug = TRUE, plot.points = TRUE)


# map

