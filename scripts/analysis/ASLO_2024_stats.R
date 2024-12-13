


# DESCRIPTIVE STATISTICS---------------------------

## summarize all emissions by site_id----
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

## summarize all emissions by lake_id, then across all lakes----
dat %>% 
  select(lake_id, matches("co2|ch4") & where(is.numeric)) %>%
  group_by(lake_id) %>%
  summarize(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% 
  ungroup() %>%
  select(-lake_id) %>%
  mutate(across(-contains("ebullition"), \(x) na_if(x, 0))) %>%
  summarize(mean = across(everything(), \(x) mean(x, na.rm = TRUE)),
            min = across(everything(), \(x) min(x, na.rm = TRUE)),
            max = across(everything(), \(x) max(x, na.rm = TRUE))) %>% 
  pivot_longer(everything())

## proportion of diffusion and ebullition-------

dat_agg %>% 
  mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
  ggplot(aes("", ch4_prop_diff)) + 
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax=0.5), fill = "lightskyblue4") +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0.5, ymax=1), fill = "deepskyblue4") +
  #geom_point()
  #geom_histogram(binwidth = 0.05)
  geom_boxplot() +
  #geom_violin() +
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 0.01) +
  #geom_jitter(position = position_jitter(0.2)) +
  ylab(expression(diffusive~fraction~of~total~CH[4]~emissions)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 20)) 

ggsave("output/figures/propDiffusionCh4.tiff",
       width=6,height=14, units="in",
       dpi=800,compression="lzw")

dat_agg %>% 
  mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
  ggplot(aes(deep_so4, ch4_prop_diff)) + 
  geom_rect(aes(xmin=550, xmax = 2500, ymin=0.75, ymax = 1), fill = "lightskyblue4") +
  geom_point() +
  geom_vline(xintercept = 550) +
  xlab(expression(sulfate~(mg~L^{-1}))) +
  ylab(expression(diffusive~fraction~of~total~CH[4]~emissions)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))



ggsave("output/figures/propDiffusionCh4BySulfate.tiff",
       width=5, height=5, units="in",
       dpi=800, compression="lzw")

# EMISSION BAR AND WHISKER PLOTS-------------------
## ch4 diffusion-----
dat_agg %>%
  filter(ch4_diffusion_best > 0) %>%
  ggplot(aes(x = ch4_diffusion_best)) + 
  geom_boxplot(notch = TRUE) + 
  scale_x_log10(limits = c(0.00001, 50), 
                labels = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  xlab(expression(diffusive~CH[4]~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("output/figures/ch4diffusionBarWhisker.tiff",
       width=4,height=1, units="in",
       dpi=800,compression="lzw")

## ch4_ebullition------
dat_agg %>%
  ggplot(aes(x = ch4_ebullition)) + 
  geom_boxplot(notch = TRUE) + 
  scale_x_log10(limits = c(0.00001, 50), 
                labels = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  xlab(expression(CH[4]~ebullition~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("output/figures/ch4ebullitionBarWhisker.tiff",
       width=4,height=1, units="in",
       dpi=800,compression="lzw")


## ch4 total-----
dat_agg %>%
  ggplot(aes(x = ch4_total)) + 
  geom_boxplot(notch = TRUE) + 
  scale_x_log10(limits = c(0.00001, 50), 
                labels = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100), 
                breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)) +
  xlab(expression(CH[4]~total~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("output/figures/ch4totalBarWhisker.tiff",
       width=4,height=1, units="in",
       dpi=800,compression="lzw")


## co2 diffusion------
dat_agg %>%
  ggplot(aes(x = co2_diffusion_best)) + 
  geom_boxplot(notch = TRUE) + 
  scale_x_continuous(trans = pseudolog10_trans,
                     limits = c(-200, 1200),
                     labels = c(-50, -5, 0, 5, 25, 100, 500), 
                     breaks = c(-50, -5, 0, 5, 25, 100, 500)) +
  geom_vline(xintercept = 0, color = "darkblue", linetype = "dashed") +
  annotate(geom = "text", x=5, y=0, label="source \nn=68", size = 8, lineheight = 0.8) +
  annotate(geom = "text", x=-5, y=0, label="sink \nn=40", size = 8, lineheight = 0.8) +
  xlab(expression(diffusive~CO[2]~(mg~CO[2]~m^{-2}~hr^{-1}))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave("output/figures/co2diffusionBarWhisker.tiff",
       width=8,height=1.5, units="in",
       dpi=800,compression="lzw")

dat_agg %>% 
  filter(visit == 1, 
         lake_id != 1000, # exclude PR
         co2_diffusion_best != 0,
         !is.na(co2_diffusion_best)) %>%
  # mutate(co2_status = case_when(is.na(co2_diffusion_best) ~ NA_character_,
  #                              co2_diffusion_best < 0 ~ "sink",
  #                              co2_diffusion_best > 0 ~ "source")) %>%
  summarise(source = sum(co2_diffusion_best > 0, na.rm = TRUE),
            sink = sum(co2_diffusion_best < 0, na.rm = TRUE))

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
  ## ch4_diff----
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
  
  
  ## ch4_ebullition-----
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
  
  ## diffusion and ebullition combined plots-----
  ### sulfate-------
  # retained in linear model
  f_labels <- c(ch4_diffusion_best = "diffusive emissions",
                ch4_ebullition = "ebullitive emissions")
  
  # SET UP CUSTOM COLORS FOR ECOREGIONS
  # use same colors for map and scatterplot
  # Custom color pallette for ecoregion polygons. Attempted to mirror
  # https://www.epa.gov/national-aquatic-resource-surveys/
  # ecoregional-results-national-lakes-assessment-2012
  cols <- c("Coastal Plains" = "orange1",
            "Northern Appalachians" = "lightpink1",
            "Northern Plains" = "darksalmon",
            "Southern Appalachians" = "mediumturquoise",
            "Southern Plains" = "khaki4",
            "Temperate Plains" = "forestgreen", 
            "Upper Midwest" = "deepskyblue4",
            "Western Mountains" = "saddlebrown",
            "Xeric" = "lightskyblue4")
  
  dat_agg %>%
    select(ag_eco9_nm, ch4_ebullition, ch4_diffusion_best, shallow_so4) %>%
    pivot_longer(contains("ch4")) %>%
    ggplot(aes(shallow_so4, value)) +
    geom_point(size = 3, aes(color = ag_eco9_nm)) +
    scale_color_manual(values = cols, na.translate = F) + # remove NA from legend
    geom_vline(xintercept = 60) +
    facet_wrap(~name, ncol = 1, labeller = labeller(name = f_labels)) +
    ylab(expression(mg~CH[4]~m^{2}~hr^{"-1"})) +
    xlab(expression(sulfate~(mg~L^{-1}))) +
    theme_bw() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
  ggsave("output/figures/ch4DiffusionEbulBySulfate.tiff",
                     width=10, height=8, units="in",
                     dpi=800, compression="lzw")
  
  ## diffusive prop of ch4 flux-------
  # 1.  nutrients and prop_diffusion.  SO4
  list(dat_agg) %>%
    map(function(x) {  
      x %>% mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
        select(ch4_prop_diff, matches("op|no2_3|tp|tn|nh4|doc|toc|so4") & where(is.numeric), -contains("pct"), -shoreline_development) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number")
      readline(prompt="Press [enter] to proceed")
    })
  
  # 2.  algal indicators.  ok, yes, dat and dat_agg
  list(dat_agg) %>%
    map(function(x) { 
      x %>% mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
        select(ch4_prop_diff, matches("chla|do_mg") & !matches("comment") & where(is.numeric), -contains("pct")) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  # 3.  morpho.  nothing here.
  list(dat_agg) %>%
    map(function(x) {  
      x %>% mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
        select(ch4_ebullition, surface_area, volume, shoreline_development, 
                   circularity, dynamic_ratio, mean_depth) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  # 4. correlation with total emissions
  cor(dat_agg %>%
        mutate(ch4_prop_diff = ch4_diffusion_best / ch4_total) %>%
        select(ch4_prop_diff),
      dat_agg %>% select(ch4_total),
      use = "pairwise.complete.obs")
  
  ## ch4 tot----
  # 1. variables retained in linear model
  list(dat_agg) %>%
  map(function(x) {  
    x %>% 
      select(ch4_total, deep_do_mg, shallow_chla_lab, shallow_so4, 
             shallow_doc, shallow_op, surface_area) %>% 
      cor(use = "pairwise.complete.obs") %>% 
      corrplot(method = "number") 
    readline(prompt="Press [enter] to proceed")
  })
  
 # ch4 total vs chla and surface area
  lm.1 <- dat_agg %>%
    select(ch4_total, shallow_chla_lab) %>%
    mutate(ch4_total_log = log10(ch4_total), 
           shallow_chla_lab_log = log10(shallow_chla_lab)) %>%
  lm(ch4_total_log ~ shallow_chla_lab_log, data = .)
  
  dat.1 <- data.frame(ch4_total = lm.1$fitted,
               shallow_chla_lab = lm.1$model$shallow_chla_lab_log)
  
  
  dat_agg %>%
    select(ag_eco9_nm, ch4_total, surface_area, shallow_chla_lab) %>%
    pivot_longer(-c(ch4_total, ag_eco9_nm)) %>%
    ggplot(aes(value, ch4_total)) +
    geom_point(size = 3, aes(color = ag_eco9_nm)) +
    scale_color_manual(values = cols, na.translate = F) + # remove NA from legend
    facet_wrap(~name, ncol = 1, scales = "free",
               labeller = as_labeller(
                 c(shallow_chla_lab = "chla~(ug/L)", surface_area = "surface~area~(m^2)"),
                 label_parsed), 
               strip.position = "bottom") + 
    # log x-axis for first facet (chla) but linear for second (SA)
    ggh4x::facetted_pos_scales(x = list(scale_x_log10(),
                                        scale_x_continuous())) +
    # log y-axis for first facet (chla) but linear for second (SA)
    ggh4x::facetted_pos_scales(y = list(scale_y_log10(breaks = c(0.01, 0.10, 1, 10, 100),
                                                      labels = c(0.01, 0.10, 1, 10, 100)),
                                        scale_y_continuous())) +
    # linear trend on chl a plot.  Looks nonlinear due to log10 transformation
    geom_smooth(data =  dat_agg %>%
                  select(ag_eco9_nm, ch4_total, surface_area, shallow_chla_lab) %>%
                  pivot_longer(-c(ch4_total, ag_eco9_nm)) %>%
                  filter(name == "shallow_chla_lab") %>% # this limits the line to chla figure
                  mutate(value = (value), # tried log10 transformation to make linear line but didn't work
                         ch4_total = (ch4_total)),
                aes(value, ch4_total),
                method = "lm") +
    geom_line(data = dat.1, aes(shallow_chla_lab, ch4_total)) +
    ylab(expression(total~CH[4]~emission~rate~(mg~CH[4]~m^{2}~hr^{"-1"}))) +
    theme_bw() +
    theme(axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16),
          strip.background = element_blank(), 
          strip.placement = "outside",
          legend.title = element_blank(),
          legend.text = element_text(size = 12))
  
  ggsave("output/figures/ch4TotalByChlaSA.tiff",
         width=10, height=8, units="in",
         dpi=800, compression="lzw")
  
  
  # ch4 total x trophic state
  dat_agg %>%
    mutate(trophic = case_when(shallow_chla_lab <= 2 ~ "oligotrophic",
                               shallow_chla_lab > 2 & shallow_chla_lab <= 7 ~ "mesotrophic",
                               shallow_chla_lab > 7 & shallow_chla_lab<= 30 ~ "eutrophic",
                               shallow_chla_lab > 30 ~ "hypereutrophic")) %>%
    ggplot(aes(x = reorder(trophic, ch4_total), ch4_total)) +
    geom_boxplot() +
    scale_y_log10()
  
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

  
  ## diffusive CO2----
  # 1.  nutrients.  a little, maybe
  list(dat, dat_agg) %>%
    map(function(x) {
      x %>% select(co2_diffusion_best, 
                   matches("op|no2_3|tp|tn|nh4|doc|toc|so4|ph") & where(is.numeric) & contains("shallow"), 
                   -shoreline_development, -contains("pct"), -contains("comment")) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  # 2.  algal indicators.  some stuff with dat_agg
  list(dat, dat_agg) %>%
    map(function(x) { 
      x %>% select(co2_diffusion_best, matches("chla|do_mg") & !matches("comment") & where(is.numeric)) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number") 
      readline(prompt="Press [enter] to proceed")
    })
  
  
  
  # 3.  morpho and co2_diff.  weak correlations
  list(dat, dat_agg) %>%
    map(function(x) {  
      x %>% select(co2_diffusion_best, surface_area, volume, shoreline_development, 
                   circularity, dynamic_ratio) %>% 
        cor(use = "pairwise.complete.obs") %>% 
        corrplot(method = "number")  
      readline(prompt="Press [enter] to proceed")
    })
  

# ASSESS DATA AVAILABILITY----
  
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
  
## ch4.ebullition-----------
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
effect_plot(m1, pred = shallow_so4, interval = TRUE, rug = TRUE, plot.points = TRUE)

  
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
effect_plot(m1, pred = shallow_chla_lab, interval = TRUE, plot.points = TRUE)



## ch4.diffusion------
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

## CH4 total---------
### by lake-----
ch4.total.dat.agg <- dat_agg %>%
  select(ch4_total, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
  na.omit
nrow(ch4.total.dat.agg) # 101 observations

m_t_ch4 <- as.formula(paste("ch4_total ~ ", paste(c(algae_vars, chem_vars, morpho_vars, 
                                                    paste(c("shallow_so4", "shallow_doc"), collapse = ":")), # interaction term
                                                  collapse = " + ")))
res <- stepwise(formula = m_t_ch4, # not recognizing interaction?
                data = ch4.total.dat.agg,
                type = "linear",
                strategy = "bidirection",
                metric = c("SL"),
                sls = 0.05)
res # shallow_chla, shallow_so4, deep_do_mg
# refit with lm for plotting
m1 <- lm(as.formula(paste("ch4_total ~ ", paste(c("deep_do_mg", "shallow_chla_lab", "shallow_so4", 
                                                  "shallow_doc", "shallow_op", "surface_area", 
                                                  paste(c("shallow_so4", "shallow_doc"), collapse = ":")), # interaction term
                                                collapse = " + "))),
         data = ch4.total.dat) %>%
  step

summary(m1)
anova(m1)
plot(m1)
effect_plot(m1, pred = deep_do_mg, interval = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shallow_chla_lab, interval = TRUE, plot.points = TRUE)
effect_plot(m1, pred = shallow_so4, interval = TRUE,  plot.points = TRUE)
effect_plot(m1, pred = shallow_doc, interval = TRUE,  plot.points = TRUE)
effect_plot(m1, pred = shallow_op, interval = TRUE, plot.points = TRUE)
effect_plot(m1, pred = surface_area, interval = TRUE, plot.points = TRUE)


### by site----
library(nlme)
dat$f_lake_id <- factor(dat$lake_id)
m_t_ch4 <- as.formula(paste("ch4_total ~ ", paste(c(algae_vars, chem_vars, morpho_vars, "deep_do_mg", "deep_temp"), collapse = " + ")))
ch4.total.dat <- dat %>%
  select(ch4_total, f_lake_id, deep_temp, deep_do_mg, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
  na.omit
nrow(ch4.total.dat) # 1172
M1me1 <- lme(m_t_ch4, random = ~1 | f_lake_id, data = ch4.total.dat)
summary(M1me1) # still only so4 and chla?  temp and do not significant

F0 <- fitted(M1me1, level = 0)
F1 <- fitted(M1me1, level = 1, )
I <- order(ch4.total.dat$ch4_total); chla_s <- sort(ch4.total.dat$shallow_chla_lab)
plot(chla_s, F0[I], lwd=4,  type = "l", ylab = "total ch4", xlab = "chl a")


## CO2 diffusion-------
co2.diffusion.dat <- dat_agg %>%
  filter(!is.na(shallow_ph), shallow_ph <15) %>%
  select(co2_diffusion_best, shallow_ph, all_of(c(algae_vars, chem_vars, morpho_vars))) %>%
  na.omit
nrow(co2.diffusion.dat) # 102 observations

m_d_co2 <- as.formula(paste("co2_diffusion_best ~ ", paste(c(algae_vars, chem_vars, morpho_vars, "shallow_ph"), collapse = " + ")))
res <- stepwise(formula = m_d_co2,
                #include = "shallow_ph",
                data = co2.diffusion.dat,
                type = "linear",
                strategy = "bidirection",
                metric = c("SL"),
                sls = 0.05)
res # shallow_chla and shoreline development
summary(res)
# refit with lm for plotting
m1 <- lm(as.formula(paste("co2_diffusion_best ~ ", paste(c("shallow_do_mg", "shallow_no2_3", "dynamic_ratio", "surface_area", "circularity", "shallow_ph"), collapse = " + "))), data = co2.diffusion.dat)
summary(m1)
anova(m1)
plot(m1)


co2.p1 <- effect_plot(m1, pred = shallow_do_mg, interval = TRUE, plot.points = TRUE,
                      x.label = expression(dissolved~oxygen~(mg~ L^{-1})),
                      y.label = expression(diffusive~CO[2]~(mg~CO[2]~m^{-2}~hr^{-1}))) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

co2.p2 <- effect_plot(m1, pred = shallow_ph, interval = TRUE, plot.points = TRUE,
                      x.label = "pH",
                      y.label = element_blank()) +
  ylim(-250, 750) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

co2.p3 <- effect_plot(m1, pred = surface_area, interval = TRUE, plot.points = TRUE,
                      x.label = expression(surface~area~(m^{2})),
                      y.label = element_blank()) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

cowplot::plot_grid(co2.p1, co2.p2, co2.p3, nrow = 1)

ggsave("output/figures/co2diffusionMarginalEffects.tiff",
       width=10,height=4, units="in",
       dpi=800,compression="lzw")



effect_plot(m1, pred = dynamic_ratio, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = surface_area, interval = TRUE, rug = TRUE, plot.points = TRUE)
effect_plot(m1, pred = circularity, interval = TRUE, rug = TRUE, plot.points = TRUE)


# MAPS--------------
# sf for plotting
dat_agg_sf <- st_as_sf(dat_agg, coords = c("long", "lat"), 
                  crs = 4269) %>% # standard for lat/lon
  st_transform(5070) # project to CONUS ALBERS for plotting


# read in ecoregion polygons
ecoR <- st_read(dsn = paste0(userPath, 
                             "/data/spatial"),
                layer = "aggr_ecoregions_simple",
                quiet = TRUE)

# Check CRS
#st_crs(ecoR) # 3857
ecoR <- st_transform(ecoR, 5070) # convert to CONUS Albers
#st_crs(ecoR) # 5070

# SET UP CUSTOM COLORS FOR ECOREGIONS
# Custom color pallette for ecoregion polygons. Attempted to mirror
# https://www.epa.gov/national-aquatic-resource-surveys/
# ecoregional-results-national-lakes-assessment-2012
cols <- c("Coastal Plains" = "orange1",
          "Northern Appalachians" = "lightpink1",
          "Northern Plains" = "darksalmon",
          "Southern Appalachians" = "mediumturquoise",
          "Southern Plains" = "khaki4",
          "Temperate Plains" = "forestgreen", 
          "Upper Midwest" = "deepskyblue4",
          "Western Mountains" = "saddlebrown",
          "Xeric" = "lightskyblue4")


## Load state boundaries
states <- USAboundaries::us_states() %>%
  dplyr::filter(!state_name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) # convert to CONUS Albers

## CH4 diffusion
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_diffusion_best),
          shape = 1,
          show.legend = "point") +
  # guide argument below removes points from the boxes in the ecoregion legend
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_size(name = expression(diffusive~CH[4]~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 20, 40)) + # custom breaks
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        #plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4diffusionBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")

## CH4 ebullition
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  # guide argument below removes points from the boxes in the ecoregion legend
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide = guide_legend(override.aes = list(shape = NA))) +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_ebullition),
          shape = 1,
          show.legend = "point") +
  scale_size(name = expression(CH[4]~ebullition~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 20, 40)) + # custom breaks
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.position.inside = c(1, 1), # move legend to immediate right of plot  
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4ebullitionBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")

## CH4 total
ggplot() +
  geom_sf(data = ecoR, color = NA, aes(fill = WSA9_NAME), alpha = 0.5) +
  geom_sf(data = states, fill = NA, color = "cornsilk3", size = 0.1) +
  geom_sf(data = dat_agg_sf %>% 
            filter(visit == 1, lake_id != 1000), # exclude PR
          aes(size = ch4_total),
          shape = 1,
          show.legend = "point") +
  # guide argument below removes points from the boxes in the ecoregion legend
  # https://aosmith.rbind.io/2020/07/09/ggplot2-override-aes/
  scale_fill_manual("Ecoregion", 
                    values = cols,
                    guide = guide_legend(override.aes = list(shape = NA))) +
  scale_size(name = expression(CH[4]~total~(mg~CH[4]~m^{-2}~hr^{-1})),
             range = c(0.5, 8), # custom size range
             breaks = c(1, 10, 25, 50)) + # custom breaks
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0,40,0,0),"mm"), # extend right plot mar for legend 
        legend.position.inside = c(1, 1), # move legend to immediate right of plot  
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.key.size = unit(0.5, "cm"))

# ggsave("output/figures/ch4totalBubblePlot.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")


mod <- lm(ch4_diffusion_best ~ site_depth * shoreline_development, data = dat)
summary(mod)



