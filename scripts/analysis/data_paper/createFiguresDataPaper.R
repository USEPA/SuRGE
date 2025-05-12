## Script for Data Paper Figures
## February 26 2025


#create an object for overlaid methane density plots
di<-data.frame(dat$ch4_diffusion_best,"diffusion")
colnames(di)<-c("rate","type")
eb<-data.frame(dat$ch4_ebullition,"ebullition")
colnames(eb)<-c("rate","type")
tot<-data.frame(dat$ch4_total,"total")
colnames(tot)<-c("rate","type")
dieb<-bind_rows(di,eb,tot)

options(scipen = 999)
densplot<-dieb%>%
  mutate(rt=rate*24)%>%
  mutate(rtc=rt*(12.01/16.043))%>%
  ggplot(aes(x=rt,color=type,fill=type))+
  geom_density(alpha=0.1)+
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00"))+
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        # axis.text.x = element_blank(),
        # axis.labels.x = element_blank(),
        # axis.ticks.x = element_blank(),
        legend.position="top")+
  scale_x_log10(limits=c(0.001,10000))+
  xlab(expression(paste("Methane (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  ylab("Density")
densplot

#create an object for overlaid carbon dioxide density plots
dic<-data.frame(dat$co2_diffusion_best,"diffusion")
colnames(dic)<-c("rate","type")
ebc<-data.frame(dat$co2_ebullition,"ebullition")
colnames(ebc)<-c("rate","type")
totc<-data.frame(dat$co2_total,"total")
colnames(totc)<-c("rate","type")

diebc<-bind_rows(dic,ebc,totc)
nbreaks <- 7
breaks <-c(-10^(nbreaks:1),0, 10^(nbreaks:1))

# densplotco2<-diebc%>%
#   mutate(rt=rate*24)%>%
#   mutate(rtc=rt*(12.01/44.009))%>%
#   ggplot(aes(x=rt,color=type,fill=type))+
#   geom_density(alpha=0.1)+
#   scale_color_manual(values = c("#56B4E9","#009E73","#D55E00"))+
#   scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00"))+
#   theme_bw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.line=element_line(colour="black"),legend.title=element_blank(),
#         axis.text = element_text(size = 14,angle = 90),
#         axis.title = element_text(size = 16),
#         legend.position="none")+
#   scale_x_continuous(trans = "pseudo_log",breaks=breaks)+
#   xlab(expression(paste("Carbon Dioxide (mg CO"[2]*" m"^"-2"*"d"^"-1"*")")))+
#   ylab("Density")
# densplotco2
# 
# densplotco2b<-totc%>%
#   mutate(rt=rate*24)%>%
#   mutate(rtc=rt*(12.01/44.009))%>%
#   ggplot(aes(x=rt,color=type,fill=type))+
#   geom_density(alpha=0.1)+
#   scale_color_manual(values = "#D55E00")+
#   scale_fill_manual(values = "#D55E00")+
#   geom_vline(xintercept=0)+
#   theme_bw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.line=element_line(colour="black"),legend.title=element_blank(),
#         axis.text = element_text(size = 14,angle = 90),
#         axis.title = element_text(size = 16),
#         legend.position="none")+
#   xlab(expression(paste("Carbon Dioxide (mg CO"[2]*" m"^"-2"*"d"^"-1"*")")))+
#   ylab("Density")
# densplotco2b
# 
# dens<-cowplot::plot_grid(densplot,densplotco2,ncol=1,align="v",labels=c("A","B"),rel_heights = c(1,1))
# dens




## rmp additions

dat_all = read.csv("communications/manuscript/data_paper/6_emission_rate_points.csv") %>%
  select(ch4_diffusion, ch4_ebullition, ch4_total, co2_diffusion, co2_ebullition, co2_total) %>%
  pivot_longer(values_to = "rate_hourly", names_to = "name", cols = ch4_diffusion:co2_total) %>%
  separate(name, into = c("gas_name", "type"), sep = "_") %>%
  mutate(gas_name = case_when(gas_name == "co2" ~ "CO[2]",
                              gas_name == "ch4" ~ "CH[4]"),
         rate_daily = rate_hourly * 24)


ggplot() +
  geom_density(data = dat_all, aes(x = rate_daily, y=..scaled.., fill = type, color = type), alpha = 0.3,
               trim = T) +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ gas_name, scale = "free", ncol = 1, labeller = label_parsed) +
  scale_x_continuous(trans = "pseudo_log", breaks = breaks, expand = c(0.025, 0.025),
                     labels = scales::comma_format(big.mark = ",")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.025))) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 14, color = "black"), #angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 14, color = "black"),
        legend.position = c(0.91, 0.9),
        legend.background = element_rect(color = "black"))+
  xlab(expression(paste("Emissions Rate (mg m"^"-2"~"d"^"-1"*")")))+
  ylab("Density (scaled)")

### Unstable start plot

# this code generates a 2 panel plot used to demonstrate relationship between
# CO2, and H2O times to stabilization.  we will be using this for data paper figure
# it is for lake 288 site 14

unstable_plot_data<-gga_2 %>%
  filter(lake_id == "288",
         site_id == "14",
         RDateTime > ch4DeplyDtTm - 60, # start plot 1 minute prior to deployment
         RDateTime < ch4RetDtTm + 300, # extend plot 1 minute post deployment
         CH4._ppm > 0) %>%
  select(lake_id, RDateTime, CH4._ppm, CO2._ppm, H2O._ppm,
         co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, ch4RetDtTm) %>%
  pivot_longer(!c(lake_id, RDateTime,co2DeplyDtTm, co2RetDtTm,
                  ch4DeplyDtTm, ch4RetDtTm)) 

CO2<-unstable_plot_data %>%
  filter(name == "CO2._ppm") %>%
  ggplot(aes(RDateTime, value)) +
  geom_point() +
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.labels.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="top")+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:16:22", tz = "UTC")),
                 color = "deployment"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:08", tz = "UTC")),
                 color = "CH4 stabilizes"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:43", tz = "UTC")),
                 color = "CO2 stabilizes"), key_glyph = "path") + #CO2
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:21:22", tz = "UTC")),
                 color = "retrieval"), key_glyph = "path") +
  scale_color_discrete(breaks = c("deployment", "CH4 stabilizes","CO2 stabilizes", "retrieval"), name="",
                       labels = c("deployment", expression("stabilized CH"[4]), expression("stabilized CO"[2]),"retrieval")) +
  xlab("") +
  ylab(expression(paste("CO"[2]*" (ppm)")))
CO2

CH4<-unstable_plot_data %>%
  filter(name == "CH4._ppm") %>%
  ggplot(aes(RDateTime, value)) +
  geom_point() +
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.labels.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none")+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:16:22", tz = "UTC")),
                 color = "deployment"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:08", tz = "UTC")),
                 color = "CH4 stabilizes"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:43", tz = "UTC")),
                 color = "CO2 stabilizes"), key_glyph = "path") + #CO2
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:21:22", tz = "UTC")),
                 color = "retrieval"), key_glyph = "path") +
  scale_color_discrete(breaks = c("deployment", "CH4 stabilizes","CO2 stabilizes", "retrieval"), name="",
                       labels = c("deployment", expression("stabilized CH"[4]), expression("stabilized CO"[2]),"retrieval")) +
  xlab("") +
  ylab(expression(paste("CH "[4]*" (ppm)")))
CH4

H2O<-unstable_plot_data %>%
  filter(name == "H2O._ppm") %>%
  mutate(valuet=value/1000)%>%
  ggplot(aes(RDateTime, valuet)) +
  geom_point() +
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:16:22", tz = "UTC")),
                 color = "deployment"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:08", tz = "UTC")),
                 color = "CH4 stabilizes"), key_glyph = "path") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:17:43", tz = "UTC")),
                 color = "CO2 stabilizes"), key_glyph = "path") + #CO2
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2021-06-28 17:21:22", tz = "UTC")),
                 color = "retrieval"), key_glyph = "path") +
  scale_color_discrete(breaks = c("deployment", "CH4 stabilizes","CO2 stabilizes", "retrieval"), name="",
                       labels = c("deployment", expression("stabilized CH"[4]), expression("stabilized CO"[2]),"retrieval")) +
  xlab("time (hh:mm)") +
  ylab(expression(paste("H "[2]*"O (ppt)")))
H2O

unstab<-cowplot::plot_grid(CO2,CH4,H2O,ncol=1,align="v",labels=c("A","B","C"),rel_heights = c(1.1,1,1))
unstab




## UPDATED:  Plot of Variables that Survey was Designed on

lake.list.plot = read.csv("communications/manuscript/data_paper/3_lake_scale.csv") %>%
  filter(name %in% c("ag_eco9_nm",
                     "depth_cat",
                     "chla_cat",
                     "study")) %>%
  pivot_wider(values_from = value, names_from = name) %>%
  filter(study == "SuRGE") %>%
  mutate(depth_cat = case_when(depth_cat == "GT_6m" ~ "deep",
                               depth_cat == "LE_6m" ~ "shallow"),
         chla_cat = case_when(chla_cat == "GT_7" ~ "productive",
                              chla_cat == "LE_7" ~ "unproductive")) %>%
  select(-units, -study) %>%
  pivot_longer(values_to = "value", names_to = "name",
               cols = c(ag_eco9_nm, depth_cat, chla_cat)) %>%
  left_join(read.csv("communications/manuscript/data_paper/7_emissions_lake.csv") %>%
              select(lake_id, ch4_diffusion_lake, ch4_ebullition_lake, co2_total_lake) %>%
              pivot_longer(values_to = "rate_hourly", names_to = "pathway",
                           cols = c(ch4_diffusion_lake, ch4_ebullition_lake, co2_total_lake))) %>%
  separate(pathway, into = c("gas_name", "type", "lake"), sep = "_") %>%
  select(-lake) %>%
  mutate(gas_name = case_when(gas_name == "co2" ~ "CO[2]",
                              gas_name == "ch4" ~ "CH[4]"),
         rate_daily = rate_hourly * 24)


ecoregion = ggplot() +
  geom_boxplot(data = lake.list.plot %>% filter(name == "ag_eco9_nm"), 
               aes(x = value, y = rate_daily, fill = type)) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ gas_name, scales = "free", labeller = label_parsed, ncol = 2) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
  labs(x = NULL, y = expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")), title = "C") +
  scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
                                 expression(CH[4]~ebullition), 
                                 expression(total~CO[2]~emissions))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text.x = element_text(size = 14, angle = 45, hjust=1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position="top")




depth = ggplot() +
  geom_boxplot(data = lake.list.plot %>% filter(name == "depth_cat"), 
               aes(x = value, y = rate_daily, fill = type)) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ gas_name, scales = "free", labeller = label_parsed, ncol = 2) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
  labs(x = NULL, y = expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")), title = "A") +
  scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
                                 expression(CH[4]~ebullition), 
                                 expression(total~CO[2]~emissions))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position="none")




productivity = ggplot() +
  geom_boxplot(data = lake.list.plot %>% filter(name == "chla_cat"), 
               aes(x = value, y = rate_daily, fill = type)) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ gas_name, scales = "free", labeller = label_parsed, ncol = 2) +
  scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
  labs(x = NULL, y = expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")), title = "B") +
  scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
                                 expression(CH[4]~ebullition), 
                                 expression(total~CO[2]~emissions))) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position="none")


top_row <- plot_grid(depth, productivity)

strata_fig<-plot_grid(top_row, ecoregion, ncol=1, rel_heights = c(1,1.75))
strata_fig




# lake.list.plot.methane<-left_join(
#   emissions_agg %>%
#     select(lake_id, visit,ch4_ebullition_lake, ch4_diffusion_lake)%>%
#     mutate(type="methane"),
#   
#   lake.list.all %>%
#   mutate(lake_id = case_when(lake_id %in% c( "69_riverine", "69_transitional","70_riverine", "70_transitional") ~ NA,
#                             lake_id %in% c( "69_lacustrine") ~ "69",
#                              lake_id %in% c( "70_lacustrine") ~ "70",
#                              TRUE ~ lake_id))%>%
#   mutate(lake_id=as.numeric(lake_id))%>%
#     select(lake_id,ag_eco9_nm,depth_cat,chla_cat))%>%
#   #rename(emission = "ch4_total_lake") %>%
#   pivot_longer(values_to = "emission", names_to = "pathway",
#                cols = c(ch4_ebullition_lake, ch4_diffusion_lake))
# 
# lake.list.plot.cd<-left_join(
#   emissions_agg %>%
#     select(lake_id, visit,co2_total_lake)%>%
#     mutate(type="carbon dioxide",
#            pathway = "total"),
#   
#   lake.list.all %>%
#     mutate(lake_id = case_when(lake_id %in% c( "69_riverine", "69_transitional","70_riverine", "70_transitional") ~ NA,
#                                lake_id %in% c( "69_lacustrine") ~ "69",
#                                lake_id %in% c( "70_lacustrine") ~ "70",
#                                TRUE ~ lake_id))%>%
#     mutate(lake_id=as.numeric(lake_id))%>%
#     select(lake_id,ag_eco9_nm,depth_cat,chla_cat))%>%
#   rename(emission = "co2_total_lake")
# 
# lake.list.plot<-bind_rows(lake.list.plot.methane,lake.list.plot.cd)%>%
#   filter(!is.na(ag_eco9_nm))%>%
#   filter(!is.na(emission))%>%
#   mutate(emi_mg=emission*24)%>%
#   filter(!is.na(depth_cat))%>%
#   filter(!is.na(chla_cat))
# 
# ecoregion<-lake.list.plot  %>%
#   ggplot(aes(x=ag_eco9_nm, y=emi_mg))+
#   theme_bw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.line=element_line(colour="black"),legend.title=element_blank(),
#         axis.text.x = element_text(size = 14, angle = 45, hjust=1),
#         axis.text.y = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         strip.text = element_text(size = 16),
#         legend.text = element_text(size = 14),
#         legend.position="top"
#         )+
#   geom_boxplot(aes(fill=pathway))+
#   geom_hline(yintercept = 0, linetype = 2) +
#   facet_wrap(~type, scales = "free") +
#   scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
#   ylab(expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")))+
#   xlab("") +
#   scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
#                                  expression(CH[4]~ebullition), 
#                                  expression(total~CO[2]~emissions)))
# ecoregion
# 
# depth<-lake.list.plot %>%
#   mutate(depth_cat=ifelse(depth_cat=="GT_6m","deep","shallow")) %>%
#   ggplot(aes(x=depth_cat, y=emi_mg))+
#   theme_bw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.line=element_line(colour="black"),legend.title=element_blank(),
#         axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         strip.text = element_text(size = 16),
#         legend.position="none"
#         )+
#   geom_boxplot(aes(fill=pathway))+
#   geom_hline(yintercept = 0, linetype = 2) +
#   facet_wrap(~type, scales = "free") +
#   scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
#   ylab(expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")))+
#   xlab("")+
#   scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
#                                  expression(CH[4]~ebullition), 
#                                  expression(total~CO[2]~emissions)))
# depth
# 
# productivity<-lake.list.plot %>%
#   mutate(chla_cat=ifelse(chla_cat=="GT_7","productive","unproductive")) %>%
#   ggplot(aes(x=chla_cat, y=emi_mg))+
#   theme_bw()+
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.line=element_line(colour="black"),legend.title=element_blank(),
#         axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         strip.text = element_text(size = 16),
#         legend.position="none"
#         )+
#   geom_boxplot(aes(fill=pathway))+
#   geom_hline(yintercept = 0, linetype = 2) +
#   facet_wrap(~type, scales = "free") +
#   scale_y_continuous(trans = "pseudo_log", breaks = c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000))+
#   ylab(expression(paste("Flux (mg m"^"-2"~"d"^"-1"*")")))+
#   xlab("")+
#   scale_fill_discrete(labels = c(expression(CH[4]~diffusion), 
#                                  expression(CH[4]~ebullition), 
#                                  expression(total~CO[2]~emissions)))
# productivity
# 
# top_row <- plot_grid(depth,productivity)
# 
# strata_fig<-plot_grid(top_row, ecoregion, ncol=1, rel_heights = c(1,1.75))
# strata_fig