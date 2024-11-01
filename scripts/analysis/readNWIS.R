#### Read in list of NWIS sites that John Harrison Compiled
#### Pull NWIS water level information for relevant sites/dates
#### Pull additional water level data found elsewhere

#Read non-NWIS water level data from csv files

canton <- readr::read_csv(paste0(userPath,
                                 "data/siteDescriptors/water_level/Canton.csv"))%>%
  janitor::clean_names() %>%
  filter(value>1600)%>%
  mutate(datetime=as.POSIXct(date,format="%m/%d/%Y %H:%M"))
plot(canton$value~canton$datetime,ylab="feet",xlab="Canton (3 ft)")
max(canton$value)-min(canton$value)
#Calculate the relative quantile of water level during SuRGE relateive to the year previous
cap<-ecdf(canton$value)(canton$value[2:3])

cantonm<-canton %>%
  mutate(lake_id="146",lake_name="Canton",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(cap)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,value,annual_watlev_prop)
colnames(cantonm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

keystone <- readr::read_csv(paste0(userPath,
                                     "data/siteDescriptors/water_level/Keystone.csv"))%>%
  janitor::clean_names() %>%
  mutate(datetime=as.POSIXct(date,format="%m/%d/%Y %H:%M"))
plot(keystone$value~keystone$datetime,ylab="feet",xlab="Keystone (10.9ft)")
max(keystone$value)-min(keystone$value)
kep<-ecdf(keystone$value)(keystone$value[2:3])

keystonem<-keystone %>%
  mutate(lake_id="148",lake_name="Keystone",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(kep)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,value,annual_watlev_prop)
colnames(keystonem)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

#keystone had two visits... this is second one
keystone2 <- readr::read_csv(paste0(userPath,
                                   "data/siteDescriptors/water_level/Keystone_2.csv"))%>%
  janitor::clean_names() %>%
  mutate(datetime=as.POSIXct(date,format="%m/%d/%Y %H:%M"))
plot(keystone2$value~keystone2$datetime,ylab="feet",xlab="Keystone (7ft)")
max(keystone2$value)-min(keystone2$value)
kep2<-ecdf(keystone2$value)(keystone2$value[2:3])

keystonem2<-keystone2 %>%
  mutate(lake_id="148",lake_name="Keystone",visit="2")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(kep2)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,value,annual_watlev_prop)
colnames(keystonem2)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

#Only  have 3 months of data for Oahe
oahe <- readr::read_csv(paste0(userPath,
                               "data/siteDescriptors/water_level/Oahe.csv"))%>%
  janitor::clean_names() %>%
  mutate(datetime=as.POSIXct(datetime,format="%m/%d/%Y %H:%M"))%>%
  mutate(elev_ft=elev_m*3.28084)
plot(oahe$elev_ft~oahe$datetime,ylab="feet",xlab="Oahe (14.9ft)")
max(oahe$elev_ft)-min(oahe$elev_ft)
oap <- "NA"

oahem<-oahe %>%
  mutate(lake_id="69",lake_name="Oahe",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(oap)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,elev_ft,annual_watlev_prop)
colnames(oahem)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

saguaro <- readr::read_csv(paste0(userPath, 
                                     "data/siteDescriptors/water_level/Saguaro_Lake_official-elevation.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(read_value_ft))%>%
  filter(approval>1199)%>%
  mutate(datetime=as.POSIXct(read_date_time,format="%m/%d/%Y %H:%M"))
plot(saguaro$read_value_ft~saguaro$datetime,ylab="feet",xlab="Saguaro Lake (6.5 ft)")
max(saguaro$read_value_ft)-min(saguaro$read_value_ft)
sap<-ecdf(saguaro$read_value_ft)(saguaro$read_value_ft[67032:67360])
mean(sap)
#Now calculate the rate of change in water level over the 3 days before measurements
a<-lm(saguaro$read_value_ft[66168:67032]~saguaro$datetime[66168:67032])
summary(a)  
coef(a)[2]*288

saguarom<-saguaro %>%
  mutate(lake_id="288",lake_name="Saguaro",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(sap)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,read_value_ft,annual_watlev_prop)
colnames(saguarom)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

madison <- readr::read_csv(paste0(userPath, 
                                   "data/siteDescriptors/water_level/madison.csv")) %>%
  janitor::clean_names() %>%
  mutate(datetime=as.POSIXct(read_date,format="%m/%d/%Y"))
plot(madison$elevation~madison$datetime,ylab="feet",xlab="Madison (1.8 ft)")
max(madison$elevation)-min(madison$elevation)

madisonm<-madison %>%
  mutate(lake_id="211",lake_name="Madison",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=NA)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,elevation,annual_watlev_prop)
colnames(madisonm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

sanleandro <-readr::read_csv(paste0(userPath, 
                                    "data/siteDescriptors/water_level/USL Res Level 2020 to 2024 daily averages.csv")) %>%
  janitor::clean_names() %>%
  filter(quality=="Good")%>%
  filter(average>0)%>%
  mutate(datetime=as.POSIXct(time,format="%m/%d/%Y %H:%M"))
plot(sanleandro$average~sanleandro$datetime, ylab="feet",xlab="Upper San Leandro (14.9 ft)")
max(sanleandro$average)-min(sanleandro$average)
slp<-ecdf(sanleandro$average)(sanleandro$average[345:346])
mean(slp)

sanleandrom<-sanleandro %>%
  mutate(lake_id="291",lake_name="Upper San Leandro",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(slp)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,average,annual_watlev_prop)
colnames(sanleandrom)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

emigrant <- readr::read_csv(paste0(userPath, 
                                  "data/siteDescriptors/water_level/emigrant.csv")) %>%
  janitor::clean_names() %>%
  mutate(datetime=as.POSIXct(date,format="%m/%d/%Y"))
plot(emigrant$emi_fb_ft~emigrant$datetime,ylab="feet",xlab="Emigrant Lake (43.7 ft)")
max(emigrant$emi_fb_ft)-min(emigrant$emi_fb_ft)
emp<-ecdf(emigrant$emi_fb_ft)(emigrant$emi_fb_ft[368:369])
mean(emp)

emigrantm<-emigrant %>%
  mutate(lake_id="249",lake_name="Emigrant",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(emp)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,emi_fb_ft,annual_watlev_prop)
colnames(emigrantm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

arkabutla <-readr::read_csv(paste0(userPath, 
                                    "data/siteDescriptors/water_level/Arkabutla.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(pool_level_ft))%>%
  mutate(datetime=as.POSIXct(date_time,format="%m/%d/%Y %H:%M"))
plot(arkabutla$pool_level_ft~arkabutla$datetime, ylab="feet",xlab="Arkabutla (22 ft)")
max(arkabutla$pool_level_ft)-min(arkabutla$pool_level_ft)
arp<-ecdf(arkabutla$pool_level_ft)(arkabutla$pool_level_ft[358:360])
mean(arp)

arkabutlam<-arkabutla %>%
  mutate(lake_id="2",lake_name="Arkabutla",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(arp)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,pool_level_ft,annual_watlev_prop)
colnames(arkabutlam)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

owyhee <-readr::read_csv(paste0(userPath, 
                                   "data/siteDescriptors/water_level/Owyhee.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(owy_fb))%>%
  mutate(datetime=as.POSIXct(date_time,format="%m/%d/%Y"))
plot(owyhee$owy_fb~owyhee$datetime, ylab="feet",xlab="Owyhee (21.1 ft)")
max(owyhee$owy_fb)-min(owyhee$owy_fb)
owp<-ecdf(owyhee$owy_fb)(owyhee$owy_fb[368:369])
mean(owp)

owyheem<-owyhee %>%
  mutate(lake_id="287",lake_name="Owyhee",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(owp)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,owy_fb,annual_watlev_prop)
colnames(owyheem)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

philips <-readr::read_csv(paste0(userPath, 
                                "data/siteDescriptors/water_level/philips.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(phl_fb_feet))%>%
  mutate(datetime=as.POSIXct(date_time,format="%m/%d/%Y"))
plot(philips$phl_fb_feet~philips$datetime, ylab="feet",xlab="Philips (23.6 ft)")
max(philips$phl_fb_feet)-min(philips$phl_fb_feet)
php<-ecdf(philips$phl_fb_feet)(philips$phl_fb_feet[367:368])
mean(php)

philipsm<-philips %>%
  mutate(lake_id="239",lake_name="Philips",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(php)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,phl_fb_feet,annual_watlev_prop)
colnames(philipsm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

boulder <-readr::read_csv(paste0(userPath, 
                                 "data/siteDescriptors/water_level/boulder.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(gage_ht_value))%>%
  mutate(datetime=as.POSIXct(date_time,format="%m/%d/%Y"))
plot(boulder$gage_ht_value~boulder$datetime, ylab="feet",xlab="Boulder (3.3 ft)")
max(boulder$gage_ht_value)-min(boulder$gage_ht_value)
bop<-ecdf(boulder$gage_ht_value)(boulder$gage_ht_value[367:368])
mean(bop)

boulderm<-boulder %>%
  mutate(lake_id="186",lake_name="Boulder",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(bop)*100)%>%
  mutate(dateTime=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,gage_ht_value,annual_watlev_prop)
colnames(boulderm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

newmelones <-readr::read_csv(paste0(userPath, 
                                 "data/siteDescriptors/water_level/new_melones.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(value))%>%
  mutate(datetime=as.POSIXct(date_time,format="%m/%d/%Y %H:%M"))
plot(newmelones$value~newmelones$datetime, ylab="feet",xlab="New Melones (81.3 ft)")
max(newmelones$value)-min(newmelones$value)
nmp<-ecdf(newmelones$value)(newmelones$value[8775:8844])
mean(nmp)

newmelonesm<-newmelones %>%
  mutate(lake_id="297",lake_name="New Melones",visit="1")%>%
  mutate(date=as.Date(datetime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(nmp)*100)%>%
  select(lake_id,lake_name,visit,date,datetime,value,annual_watlev_prop)
colnames(newmelonesm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")


## Now add water level data from NWIS
sylvan<-renameNWISColumns(readNWISuv("04100180","00065",startDate="2019-09-01",endDate="2020-09-03"))
plot(sylvan$GH_Inst~sylvan$dateTime, ylab="feet",xlab="Sylvan Lake (1.4 ft)")
max(sylvan$GH_Inst)-min(sylvan$GH_Inst)
#calculate the percentile of the observed water level compared to the year before
syp<-ecdf(sylvan$GH_Inst)(sylvan$GH_Inst[8820:8842])
mean(syp)

sylvanm<-sylvan %>%
  mutate(lake_id="235",lake_name="Sylvan",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(syp)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,GH_Inst,annual_watlev_prop)
colnames(sylvanm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

jacksonville<-renameNWISColumns(readNWISuv("08032200","62614",startDate="2021-09-11",endDate="2022-09-14"))
plot(jacksonville$X_62614_Inst~jacksonville$dateTime,ylab="feet",xlab="Jacksonville (2.6 ft)")
max(jacksonville$X_62614_Inst)-min(jacksonville$X_62614_Inst)
jap<-ecdf(jacksonville$X_62614_Inst)(jacksonville$X_62614_Inst[35112:35217])
mean(jap)

jacksonvillem<-jacksonville %>%
  mutate(lake_id="11",lake_name="Jacksonville",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(jap)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,X_62614_Inst,annual_watlev_prop)
colnames(jacksonvillem)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

hamilton<-renameNWISColumns(readNWISuv("07358500","00065",startDate="2022-06-04",endDate="2023-06-07"))
plot(hamilton$GH_Inst~hamilton$dateTime, ylab="feet",xlab="Hamilton Lake (6 ft)")
max(hamilton$GH_Inst)-min(hamilton$GH_Inst)
#calculate the percentile of the observed water level compared to the year before
hap<-ecdf(hamilton$GH_Inst)(hamilton$GH_Inst[35101:35215])
mean(hap)

hamiltonm<-hamilton %>%
  mutate(lake_id="99",lake_name="Hamilton",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(hap)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,GH_Inst,annual_watlev_prop)
colnames(hamiltonm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

baker<-renameNWISColumns(readNWISuv("12191600","62615",startDate="2017-09-11",endDate="2018-09-14"))
plot(baker$X_62615_Inst~baker$dateTime, ylab="feet",xlab="Baker (33.6 ft)")
max(baker$X_62615_Inst)-min(baker$X_62615_Inst)
bap<-ecdf(baker$X_62615_Inst)(baker$X_62615_Inst[8811:8841])
mean(bap)

bakerm<-baker %>%
  mutate(lake_id="999",lake_name="Baker",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(bap)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,X_62615_Inst,annual_watlev_prop)
colnames(bakerm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")


whorse<-renameNWISColumns(readNWISuv("13174000","00062",startDate="2020-07-07",endDate="2021-07-10"))
plot(whorse$X_00062_Inst~whorse$dateTime,ylab="feet",xlab="Wild Horse (10.8 ft)")
max(whorse$X_00062_Inst)-min(whorse$X_00062_Inst)
whp<-ecdf(whorse$X_00062_Inst)(whorse$X_00062_Inst[32452:32563])
mean(whp)

whorsem<-whorse %>%
  mutate(lake_id="298",lake_name="Wild Horse",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(whp)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,X_00062_Inst,annual_watlev_prop)
colnames(whorsem)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

overholser<-renameNWISColumns(readNWISuv("07240500","00065",startDate="2020-07-26",endDate="2021-07-29"))
plot(overholser$GH_Inst~overholser$dateTime, ylab="feet",xlab="Overholser (1.8 ft)")
max(overholser$GH_Inst)-min(overholser$GH_Inst)
#calculate the percentile of the observed water level compared to the year before
ovp<-ecdf(overholser$GH_Inst)(overholser$GH_Inst[8340:8366])
mean(ovp)

overholserm<-overholser %>%
  mutate(lake_id="167",lake_name="Overholser",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(ovp)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,GH_Inst,annual_watlev_prop)
colnames(overholserm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

pines<-renameNWISColumns(readNWISuv("07345900","62614",startDate="2021-09-13",endDate="2022-09-16"))
plot(pines$X_62614_Inst~pines$dateTime,ylab="feet",xlab="Lake O' the Pines (2.7 ft)")
max(pines$X_62614_Inst)-min(pines$X_62614_Inst)
pip<-ecdf(pines$X_62614_Inst)(pines$X_62614_Inst[34646:34771])
mean(pip)

pinesm<-pines %>%
  mutate(lake_id="3",lake_name="Lake O' the Pines",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=mean(pip)*100)%>%
  select(lake_id,lake_name,visit,date,dateTime,X_62614_Inst,annual_watlev_prop)
colnames(pinesm)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

# This data is from the river above the reservoir unfortunately
# tschida<-renameNWISColumns(readNWISuv("06345780","00065",startDate="2020-07-28",endDate="2021-07-31"))
# plot(tschida$GH_Inst~tschida$dateTime,ylab="feet",xlab="Tschida (9.3 ft)")
# max(tschida$GH_Inst)-min(tschida$GH_Inst)
# tsp<-ecdf(tschida$GH_Inst)(tschida$GH_Inst[29721:29831])
# mean(tsp)
# 
# tschidam<-tschida %>%
#   mutate(lake_id="68",lake_name="Tschida",visit="1")%>%
#   mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
#   mutate(annual_watlev_prop=mean(tsp)*100)%>%
#   select(lake_id,lake_name,visit,date,dateTime,GH_Inst,annual_watlev_prop)
# colnames(tschidam)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

francis<-renameNWISColumns(readNWISuv("06442996","00065",startDate="2020-06-23",endDate="2021-07-13"))
plot(francis$GH_Inst~francis$dateTime,ylab="feet",xlab="Francis Case lac & riv")
frp<-ecdf(francis$GH_Inst)(francis$GH_Inst[])
mean(frp)

francism<-francis %>%
  mutate(lake_id="70",lake_name="Francis",visit="1")%>%
  mutate(date=as.Date(dateTime,format="%Y-%m-%d"))%>%
  mutate(annual_watlev_prop=NA)%>%
  select(lake_id,lake_name,visit,date,dateTime,GH_Inst,annual_watlev_prop)
colnames(francism)<-c("lake_id","lake_name","visit","date","dateTime","feet","annual_watlev_prop")

## Bind rows together

walev<-rbind(pinesm,whorsem,bakerm,jacksonvillem,sylvanm,newmelonesm,
             boulderm,philipsm,owyheem,arkabutlam,sanleandrom,saguarom,
             overholserm,emigrantm,hamiltonm,francism,madisonm,cantonm,
             oahem,keystonem,keystonem2)

write.csv(walev,paste0(userPath, 
                       "data/siteDescriptors/water_level/water_levels_combined.csv"))

#Now create a file that has deployment and retrieval times for
#Chambers and Traps at each site with water level data


#need to link by lake and visit
walev_link<-walev %>%
  mutate(lake_id= as.numeric(lake_id),visit=as.numeric(visit))%>%
  group_by(paste(lake_id,visit))%>%
  summarise(lake_id=lake_id[1],visit=visit[1],annual_watlev_prop=annual_watlev_prop[1])%>%
  mutate(drawdown=ifelse(lake_id %in% c(239, 249, 287, 291, 297,298),"pronounced decrease",
                         ifelse(lake_id %in% c(999, 69, 3,11,146),"small decrease",
                                ifelse(lake_id %in% c(99, 148, 167, 186, 235),"steady",
                                       ifelse(lake_id == 70,"hydropeaking","increasing")))))
