# 1. SURVEY DESIGN DESCRIPTION------------------------------
# 2 hand picked site excluding Falls Lake and 2016 study
lake.list.all %>% 
  filter(site_type == "HAND", # hand picked
         !(lake_id %in% as.character(1001:1033))) # not in 2016 study

# 112 probability sites
lake.list.all %>%
  # deal with subsampled missouri river impoundments
  mutate(
    lake_id = case_when(lake_id %in% c("69_lacustrine", "69_riverine", "69_transitional") ~ "69",
                      lake_id %in% c("70_lacustrine", "70_riverine", "70_transitional") ~ "70",
                      TRUE ~ lake_id)) %>%
  distinct(lake_id, site_type) %>%
  filter(site_type == "PROB")


# 2. SUMMARIZE EMISSIONS--------------

mes<-dat %>%
  filter(!is.na(ch4_diffusion_best))%>%
  mutate(ch4diff=ch4_diffusion_best*24)
#2188 individual methane measurements
mez<-mes %>%
  filter(ch4_diffusion_best==0)
#80 are zero values

cas<-dat %>%
  filter(!is.na(co2_diffusion_best))%>%
  mutate(co2diff=co2_diffusion_best*24)
#2141 individual measurements
caz<-cas %>%
  filter(co2_diffusion_best==0)
#317 are zero values

meb<-dat %>%
  filter(!is.na(ch4_ebullition))
ceb<-dat %>%
  filter(!is.na(co2_ebullition))
#2288 measurements of ebullition

mt<-dat %>%
  filter(!is.na(ch4_total))
#2111 measurements of total methane flux
ct<-dat %>%
  filter(!is.na(co2_total))
#2066 measurements of total carbon dioxide flux

#Proportion of diffusive emissions that were zero:
zm<-dat%>%
  filter(ch4_diffusion_best==0)
80/2188

zc<-dat%>%
  filter(co2_diffusion_best==0)
317/2141

#Smallest detectable diffusive fluxes
ms<-mes %>%
  filter(ch4_diffusion_best>0)
min(ms$ch4_diffusion_best)*24

cs<-cas %>%
  filter(co2_diffusion_best>0)
min(cs$co2_diffusion_best)*24

#deployment lengths
summary(dat$co2_deployment_length)
summary(dat$ch4_deployment_length)

#unstable starts
nrow(filter(dat, co2flag=="U"))

# 3. K600 TECHNICAL VALIDATION--------------
# 3.1 CO2 direction and gas under/supersaturation
dissolved_gas_k %>%
  # only observations with dissolved co2 and co2 diffusion
  filter(!if_any(c(co2_sat_ratio, co2_diffusion_best), ~ is.na(.x))) %>% 
  # only those with detectable CO2 diffusion and CO2 > 0
  filter(co2_diffusion_best != 0) %>%
  #select(lake_id, site_id, visit, co2_sat_ratio, co2_diffusion_best) %>% print(n=Inf)
  mutate(co2_direction_check = case_when(co2_sat_ratio > 1 & co2_diffusion_best > 0 ~ TRUE, # if supersatured and positive flux
                                         co2_sat_ratio < 1 & co2_diffusion_best < 0 ~ TRUE, # if undersatured and negative flux
                                         TRUE ~ FALSE)) %>% # if dg concentration and flux direction disagree, then FALSE
  summarize(co2_direction_check_true = sum(co2_direction_check),
            co2_direction_check_false = sum(!co2_direction_check),
            co2_direction_check_n = n())

# exploratory plot
dissolved_gas_k %>%
  # only observations with dissolved co2 and co2 diffusion
  filter(!if_any(c(sat_co2, co2_diffusion_best), ~ is.na(.x))) %>% 
  # only those with detectable CO2 diffusion and CO2 > 0
  filter(co2_diffusion_best != 0) %>%
  #select(lake_id, site_id, visit, co2_sat_ratio, co2_diffusion_best) %>% print(n=Inf)
  mutate(co2_direction_check = case_when(co2_sat_ratio > 1 & co2_diffusion_best > 0 ~ TRUE, # if supersatured and positive flux
                                         co2_sat_ratio < 1 & co2_diffusion_best < 0 ~ TRUE, # if undersatured and negative flux
                                         TRUE ~ FALSE)) %>% # if dg concentration and flux direction disagree, then FALSE
  arrange(co2_direction_check) %>%
  ggplot(aes(co2_direction_check, co2_diffusion_best)) +
  #ggplot(aes(co2_direction_check, co2_sat_ratio)) +
  geom_point()

# 3.2 k600
# 103 observations
dissolved_gas_k %>%
  filter(!is.na(k_co2_600) | !is.na(k_ch4_600)) %>%
  summarize(n = n())


dissolved_gas_k %>%
  select(lake_id, site_id, visit, 
         ch4_sat_ratio, ch4_diffusion_best, k_ch4_600,
         co2_sat_ratio, co2_diffusion_best, k_co2_600) %>%
  filter(!is.na(k_co2_600) | !is.na(k_ch4_600)) %>%
  summarize(n = n())

# lit values
k600_lit <- read_xlsx(paste0("scripts/analysis/data_paper/",
                             "27_2020_729_MOESM5_ESM.xlsx"), skip = 1) %>%
  janitor::clean_names() %>%
  rename(method = method_g_gas_injection_c_chamber_e_eddy_covariance_m_mass_balance) %>%
  mutate(k600 = as.numeric(k600_reported_or_calculated) * (24/100)) %>% # cm/h -> m/d
  filter(method == "C", k600 != 0) %>%
  select(lake_name, k600, lat_dd, long_dd) %>% 
  mutate(study = "Literature") %>%
  st_as_sf(., coords = c("long_dd", "lat_dd")) %>%
  st_set_crs(4326)

# how many lakes id dataset
k600_lit %>% distinct(lake_name) %>% summarize(n=n()) # 48  
# where are they located? Europe, Asia, N and S America
tmap::tm_shape(World) +
  tm_polygons() +
  tm_shape(k600_lit) +
  tm_symbols()

# descriptive statistics
summary(k600_lit$k600) # lit values
summary(dissolved_gas_k %>% # SuRGE data
          # rowwise needed to calculate mean across two columns, by row
          rowwise() %>% #
          mutate(k600 = mean(c(k_co2_600, k_ch4_600), na.rm = TRUE)) %>%
          pull(k600))

# k600 figure

bind_rows(
  # literature data
  k600_lit,
  
  # SuRGE data
  dissolved_gas_k %>%
    select(lake_id, k_co2_600, k_ch4_600) %>%
    rowwise() %>% # needed to calculate mean across two columns, by group
    mutate(k600 = mean(c(k_co2_600, k_ch4_600), na.rm = TRUE)) %>%
    ungroup() %>% # turn off rowwise grouping
    select(-k_co2_600, -k_ch4_600) %>%
    rename(lake_name = lake_id) %>%
    mutate(study = "SuRGE")
) %>%
  filter(is.finite(k600)) %>% # omit NaN and avoid ggplot warning
  ggplot(aes(k600, study)) +
  geom_boxplot() +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10), 
                limits = c(0.01, 50), 
                labels = c(0.01, 0.1, 1, 10)) +
  xlab(expression(k[600]~(m~d^-1))) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave(filename = "scripts/analysis/data_paper/k600.tiff", width = 3, height = 2, units = "in")
  

# 4. EBULLITION DEPLOYMENT TIMES
# deployment times in fld_sheet are UTC. Probably want to report in 
# local time zone for paper. Read in all files, but this time don't
# specify a time zone. R will retain the clock time from the spreadsheet
# and incorrectly specify UTC. For data paper, we just want to know min/max
# deployment cloct time, so tz doesn't matter.

# 1. Create a list of file paths where the data are stored.  
labs <- c("ADA", "CIN", "DOE", "NAR", "R10", "RTP", "USGS", "PR")
paths <- paste0(userPath,  "data/", labs)


# Function for reading 'data' tab of surgeData files.

get_data_sheet <- function(paths){
  #d <-  
  fs::dir_ls(path = paths, # see above
             regexp = 'surgeData', # file names containing this pattern
             recurse = TRUE) %>% # look in all subdirectories
    .[!grepl(c(".pdf|.docx"), .)] %>% # remove pdf and .docx review files
    .[!grepl(c("Falls"), .)] %>% # omit Falls Lake while data entry underway (add visit number to file name)
    .[!grepl(c("surgeData207_nlafill.xlsx"), .)] %>% # temp file Bridget is using to extrapolate sonde data
    #.[grepl("191", .)] %>%
    # map will read each file in fs_path list generated above
    # imap passes the element name (here, the filename) to the function
    purrr::imap(~read_excel(.x, skip = 1, sheet = "data", 
                            na = c("NA", "", "N/A", "n/a")) %>%
                  # Assign the filename to the visit column for now
                  mutate(visit = .y)) %>% # assign file name
    purrr::discard(~ nrow(.x) == 0) %>% 
    # format data
    map(., function(x) { 
      janitor::clean_names(x) %>%
        select(lake_id, visit, site_id, eval_status,
               trap_deply_date, trap_deply_time,
               trap_rtrvl_date, trap_rtrvl_time, 
               chamb_deply_date, chamb_deply_time) %>%
        # Assign value to visit based on the Excel file name
        mutate(visit = if_else(str_detect(visit, "visit2"),
                               2, 1, missing = 1), 
               # format lake_id and site_id.  See Wiki
               lake_id = as.character(lake_id) %>%
                 tolower(.) %>% # i.e. Lacustrine -> lacustrine
                 str_remove(., "ch4_") %>% # remove any ch4_ from lake_id
                 str_remove(., "^0+"), #remove leading zeroes i.e. 078->78
               site_id = as.numeric(gsub(".*?([0-9]+).*", "\\1", site_id))) %>% # round to nearest tenth of meter
        # remove unused sites
        filter(eval_status == "TS") %>% # keep "Target/Sampled". Exclude all others
        # Format date and time objects
        mutate(across(contains("date"), ~ as.Date(.x, format = "%m.%d.%Y")), # convert date to as.Date
               across(contains("time"), ~ format(.x, format = "%H:%M:%S")), # convert time to character
               trap_deply_date_time = as.POSIXct(x = paste0(trap_deply_date, trap_deply_time),
                                                 format = "%Y-%m-%d%H:%M:%S"),
               trap_rtrvl_date_time = as.POSIXct(x = paste0(trap_rtrvl_date, trap_rtrvl_time),
                                                 format = "%Y-%m-%d%H:%M:%S"),
               chamb_deply_date_time = as.POSIXct(x = paste0(chamb_deply_date, chamb_deply_time),
                                                  format = "%Y-%m-%d%H:%M:%S"))
      }) %>%
    map_dfr(., bind_rows) # rbinds into one df
    
}


# 2. Read 'data' tab of surgeData files.
fld_sheet_tm <- get_data_sheet(paths = paths) 

# 3. Min/max/median trap deployment/retrieval times
fld_sheet_tm %>%
  mutate(trap_duration = trap_rtrvl_date_time - trap_deply_date_time) %>%
  summarize(
    # deployment
    min_trap_deply = min(format(trap_deply_date_time, format = "%H:%M"), na.rm = TRUE),
    max_trap_deply = max(format(trap_deply_date_time, format = "%H:%M"), na.rm = TRUE),
    median_trap_deply = median(format(trap_deply_date_time, format = "%H:%M"), na.rm = TRUE),
    # retrieval
    min_trap_rtrvl = min(format(trap_rtrvl_date_time, format = "%H:%M"), na.rm = TRUE),
    max_trap_rtrvl = max(format(trap_rtrvl_date_time, format = "%H:%M"), na.rm = TRUE),
    median_trap_rtrvl = median(format(trap_rtrvl_date_time, format = "%H:%M"), na.rm = TRUE),
    # duration
    min_trap_duration = min(trap_rtrvl_date_time - trap_deply_date_time, na.rm = TRUE),
    max_trap_duration = max(trap_rtrvl_date_time - trap_deply_date_time, na.rm = TRUE),
    median_trap_duration = median(trap_rtrvl_date_time - trap_deply_date_time, na.rm = TRUE))

fld_sheet_tm %>% 
  mutate(max_trap_duration = trap_rtrvl_date_time - trap_deply_date_time,
    trap_deply_date_time = format(trap_deply_date_time, format = "%H:%M")) %>%
  filter(trap_deply_date_time == "20:17" | # double check max deployment time, confirmed
         trap_deply_date_time == "07:31" | # double check min deployment time, confirmed
         max_trap_duration  > 40) %>% # double check long deployments, confirmed
  print(n=Inf)
