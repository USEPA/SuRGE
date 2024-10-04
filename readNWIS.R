#### Read in list of NWIS sites that John Harrison Compiled
#### Pull NWIS water level information for relevant sites/dates
#### Pull additional water level data found elsewhere

#Station List

saguaro <- readr::read_csv(paste0(userPath, 
                                     "data/siteDescriptors/water_level/Saguaro_Lake_official-elevation.csv")) %>%
  janitor::clean_names() %>%
  filter(!is.na(read_value_ft))%>%
  filter(approval>1199)%>%
  mutate(datetime=as.POSIXct(read_date_time,format="%m/%d/%Y %H:%M"))
plot(saguaro$read_value_ft~saguaro$datetime,ylab="feet",xlab="Saguaro Lake (6.5 ft)")
max(saguaro$read_value_ft)-min(saguaro$read_value_ft)

sanleandro <-readr::read_csv(paste0(userPath, 
                                    "data/siteDescriptors/water_level/USL Res Level 2020 to 2024 daily averages.csv")) %>%
  janitor::clean_names() %>%
  filter(quality=="Good")%>%
  filter(average>0)%>%
  mutate(datetime=as.POSIXct(time,format="%m/%d/%Y %H:%M"))
plot(sanleandro$average~sanleandro$datetime, ylab="feet",xlab="Upper San Leandro (14.9 ft)")
max(sanleandro$average)-min(sanleandro$average)

sylvan<-renameNWISColumns(readNWISuv("04100180","00065",startDate="2019-09-01",endDate="2020-09-03"))
plot(sylvan$GH_Inst~sylvan$dateTime, ylab="feet",xlab="Sylvan Lake (1.4 ft)")
max(sylvan$GH_Inst)-min(sylvan$GH_Inst)

jacksonville<-renameNWISColumns(readNWISuv("08032200","62614",startDate="2021-09-11",endDate="2022-09-14"))
plot(jacksonville$X_62614_Inst~jacksonville$dateTime,ylab="feet",xlab="Jacksonville (2.6 ft)")
max(jacksonville$X_62614_Inst)-min(jacksonville$X_62614_Inst)

baker<-renameNWISColumns(readNWISuv("12191600","62615",startDate="2017-09-11",endDate="2018-09-14"))
plot(baker$X_62615_Inst~baker$dateTime, ylab="feet",xlab="Baker (33.6 ft)")
max(baker$X_62615_Inst)-min(baker$X_62615_Inst)

whorse<-renameNWISColumns(readNWISuv("13174000","00062",startDate="2020-07-07",endDate="2021-07-10"))
plot(whorse$X_00062_Inst~whorse$dateTime,ylab="feet",xlab="Wild Horse (10.8 ft)")
max(whorse$X_00062_Inst)-min(whorse$X_00062_Inst)

pines<-renameNWISColumns(readNWISuv("07345900","62614",startDate="2021-09-13",endDate="2022-09-16"))
plot(pines$X_62614_Inst~pines$dateTime,ylab="feet",xlab="Lake O' the Pines (2.7 ft)")
max(pines$X_62614_Inst)-min(pines$X_62614_Inst)

tschida<-renameNWISColumns(readNWISuv("06345780","00065",startDate="2020-07-28",endDate="2021-07-31"))
plot(tschida$GH_Inst~tschida$dateTime,ylab="feet",xlab="Tschida (9.3 ft)")
max(tschida$GH_Inst)-min(tschida$GH_Inst)

francis<-renameNWISColumns(readNWISuv("06442996","00065",startDate="2020-06-23",endDate="2021-06-30"))
plot(francis$GH_Inst~francis$dateTime,ylab="feet",xlab="Francis Case lac & riv")

francis_t<-renameNWISColumns(readNWISuv("06442996","00065",startDate="2020-07-10",endDate="2021-07-13"))
plot(francis_t$GH_Inst~francis_t$dateTime,ylab="feet",xlab="Francis Case tran (10.2 ft)")
max(francis_t$GH_Inst)-min(francis_t$GH_Inst)


#monroe water levels only began being collected in 2024
#monroe<-renameNWISColumns(readNWISuv("03372400","62614",startDate="2019-08-09",endDate="2020-08-13"))
