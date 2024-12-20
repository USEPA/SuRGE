# Analysis for AGU Poster
# December 4, 2024

#Overall emissions plots

test<-dat %>%
  group_by(lake_id)%>%
  summarise(ch4_tot=mean(ch4_total))

#create an object for overlaid density plots
di<-data.frame(dat$ch4_diffusion_best,"diffusion")
colnames(di)<-c("rate","type")
eb<-data.frame(dat$ch4_ebullition,"ebullition")
colnames(eb)<-c("rate","type")

dieb<-bind_rows(di,eb)

options(scipen = 999)
densplot<-dieb%>%
  mutate(rt=rate*24)%>%
  ggplot(aes(x=rt,color=type,fill=type))+
  geom_density(alpha=0.1)+
  scale_color_brewer(palette="Dark2",)+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position=c(0.8,0.7))+
  scale_x_log10()+
  xlab(expression(paste("Methane Flux (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  ylab("Density")
densplot
library(RColorBrewer)
myPal <- brewer.pal(2,"Dark2")

ggsave("densityplot.jpg",width=6,height=2.25,units="in",dpi=300,path="~/National_Reservoir_GHG_Survey/AGU_Poster")

#Make a density plot that shows the percent emission as diffusion

percentdiffusive<-dat %>%
  filter(!is.na(ch4_diffusion_best))%>%
  filter(!is.na(ch4_ebullition))%>%
  filter(!is.na(ch4_total))%>%
  filter(!is.na(site_depth))%>%
  mutate(percent_diffusive=ch4_diffusion_best/ch4_total)%>%
  mutate(percent_ebullition=ch4_ebullition/ch4_total)%>%
  mutate(depth_category=ifelse(site_depth<6,"shallow","deep"))

densplotper<-percentdiffusive%>%
  ggplot(aes(x=percent_ebullition,color=depth_category,fill=depth_category))+
  geom_density(alpha=0.1)+
  scale_color_brewer(palette="Set1",)+
  scale_fill_brewer(palette="Set1")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position=c(0.8,0.7))+
  xlab("Percent Ebullition")+
  ylab("Density")
densplotper

ggsave("percent_ebullition.jpg",width=6,height=2.25,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

#List of predictors examined:

#Trophic Status: shallow_chla_lab, shallow_chla_sonde, shallow_op, NitrogenCat,
 #                agkffactcat,chl_predicted_sample_month, chl_predicted_sample_season,
 #                doc_predicted_sample_month, doc_predicted_sample_season, nla17_chla

#Morphometry: site_depth, surface_area, shoreline_development, volume, circularity,
#             dynamic_ratio, littoral_fraction, max_depth, mean_depth, fetch

#Sediment Characteristics: sedimentation_m3y, C_sedimentation_m3y, kffactcat,
#                           omcat,scat, dam age

#Hydrology/Salinity:  RT, E_I, buoyf, deep_sp_cond, deep_s, deep_temp

#Sediment characteristics: 
#Salinity: sulfate concentrations, 
#Hydrologic: center buoyancy frequency, residence time, E:I, 

#In Each Category, Examine the Predictive Power of Different Variables

# TROPHIC STATUS

#filter to the subset of sites that has information on all the predictors

trop<-dat %>%
   filter( !is.na(ch4_diffusion_best),
     !is.na(shallow_chla_lab),
     !is.na(shallow_chla_sonde), 
        !is.na(shallow_op), 
         !is.na(NitrogenCat),
         !is.na(agkffactcat),
         !is.na(chl_predicted_sample_month),
         !is.na(chl_predicted_sample_season),
         !is.na(doc_predicted_sample_month),
         !is.na(doc_predicted_sample_season),
         !is.na(nla17_chla),
         )%>%
  mutate(shallow_chla_sonde=ifelse(shallow_chla_sonde<0.1,0.1,shallow_chla_sonde))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(ch4_diffusion=ch4_diffusion_best,
         shallow_chla_s=shallow_chla_sonde,
         ch4_diffusion_best=log(ch4_diffusion_best+1),
         shallow_chla_lab=log(shallow_chla_lab),
         shallow_chla_sonde=log(shallow_chla_sonde),
         chl_predicted_sample_month=log(chl_predicted_sample_month),
         nla17_chlalog=log(nla17_chla),
         chl_predicted_sample_season=log(chl_predicted_sample_season))%>%
  select(shallow_chla_lab, shallow_chla_sonde, shallow_op, NitrogenCat,
         agkffactcat,chl_predicted_sample_month, chl_predicted_sample_season,
         doc_predicted_sample_month, doc_predicted_sample_season,
         ch4_diffusion_best,lake_id,ch4_diffusion,shallow_chla_s,nla17_chla,
         nla17_chlalog)

#shapiro.test(log(trop$doc_predicted_sample_month))

m<-cor(trop)
corrplot(m,method="number")

a<-lmer(trop$ch4_diffusion_best~trop$shallow_chla_lab+(1|trop$lake_id),REML=FALSE)
summary(a)
#2115.8

b<-lmer(trop$ch4_diffusion_best~trop$shallow_chla_sonde+(1|trop$lake_id),REML=FALSE)
summary(b)
#2110.7

c<-lmer(trop$ch4_diffusion_best~trop$shallow_op+(1|trop$lake_id),REML=FALSE)
summary(c)
#2116.0

d<-lmer(trop$ch4_diffusion_best~trop$NitrogenCat+(1|trop$lake_id),REML=FALSE)
summary(d)
#2114.6

e<-lmer(trop$ch4_diffusion_best~trop$agkffactcat+(1|trop$lake_id),REML=FALSE)
summary(e)
#2115.9

f<-lmer(trop$ch4_diffusion_best~trop$chl_predicted_sample_month+(1|trop$lake_id),REML=FALSE)
summary(f)
#2115.9

g<-lmer(trop$ch4_diffusion_best~trop$chl_predicted_sample_season+(1|trop$lake_id),REML=FALSE)
summary(g)
#2115.2

h<-lmer(trop$ch4_diffusion_best~trop$doc_predicted_sample_month+(1|trop$lake_id),REML=FALSE)
summary(h)
#2115.9

i<-lmer(trop$ch4_diffusion_best~trop$doc_predicted_sample_season+(1|trop$lake_id),REML=FALSE)
summary(i)
#2112.9

j<-lmer(trop$ch4_diffusion_best~trop$nla17_chlalog+(1|trop$lake_id),REML=FALSE)
summary(j)
#2115.9

#Create a trophic status figure for methane diffusion

tropd_plot<-trop %>%
  mutate(trophic=ifelse(shallow_chla_s<12,"oligotrophic",ifelse(shallow_chla_s>24,"eutrophic","mesotrophic")))%>%
  ggplot(aes(x=shallow_chla_sonde,y=ch4_diffusion_best))+
    geom_point()
tropd_plot

trope<-dat %>%
  filter( !is.na(ch4_ebullition),
          !is.na(shallow_chla_lab),
          !is.na(shallow_chla_sonde), 
          !is.na(shallow_op), 
          !is.na(NitrogenCat),
          !is.na(agkffactcat),
          !is.na(nla17_chla),
          !is.na(chl_predicted_sample_month),
          !is.na(chl_predicted_sample_season),
          !is.na(doc_predicted_sample_month),
          !is.na(doc_predicted_sample_season))%>%
  mutate(shallow_chla_sonde=ifelse(shallow_chla_sonde<0.1,0.1,shallow_chla_sonde))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(shallow_chla_l=shallow_chla_lab,
         shallow_chla_lab=log(shallow_chla_lab),
         shallow_chla_sonde=log(shallow_chla_sonde),
         chl_predicted_sample_month=log(chl_predicted_sample_month),
         nla17_chlalog=log(nla17_chla),
         chl_predicted_sample_season=log(chl_predicted_sample_season))%>%
  select(shallow_chla_lab, shallow_chla_sonde, shallow_op, NitrogenCat,
         agkffactcat,chl_predicted_sample_month, chl_predicted_sample_season,
         doc_predicted_sample_month, doc_predicted_sample_season,
         ch4_ebullition,lake_id,shallow_chla_l,nla17_chla,nla17_chlalog)

shapiro.test(log(trope$nla17_chla))

n<-cor(trop)
corrplot(n,method="number")

aa<-lmer(trope$ch4_ebullition~trope$shallow_chla_lab+(1|trope$lake_id),REML=FALSE)
summary(aa)
#10297.8

bb<-lmer(trope$ch4_ebullition~trope$shallow_chla_sonde+(1|trope$lake_id),REML=FALSE)
summary(bb)
#10307.4

cc<-lmer(trope$ch4_ebullition~trope$shallow_op+(1|trope$lake_id),REML=FALSE)
summary(cc)
#10313.5

dd<-lmer(trope$ch4_ebullition~trope$NitrogenCat+(1|trope$lake_id),REML=FALSE)
summary(dd)
#10313.0

ee<-lmer(trope$ch4_ebullition~trope$agkffactcat+(1|trope$lake_id),REML=FALSE)
summary(ee)
#10313.5

ff<-lmer(trope$ch4_ebullition~trope$chl_predicted_sample_month+(1|trope$lake_id),REML=FALSE)
summary(ff)
#10312.0

gg<-lmer(trope$ch4_ebullition~trope$chl_predicted_sample_season+(1|trope$lake_id),REML=FALSE)
summary(gg)
#10304.9

hh<-lmer(trope$ch4_ebullition~trope$doc_predicted_sample_month+(1|trope$lake_id),REML=FALSE)
summary(hh)
#10312.4

ii<-lmer(trope$ch4_ebullition~trope$doc_predicted_sample_season+(1|trope$lake_id),REML=FALSE)
summary(ii)
#10303.5

jj<-lmer(trope$ch4_ebullition~trope$nla17_chlalog+(1|trope$lake_id),REML=FALSE)
summary(jj)
#10302.3

#Create a trophic status figure for methane ebullition
options(scipen = 999)

tropew<- dat %>%
  filter(!is.na(ch4_ebullition))%>%
  mutate(trophicpred=ifelse(!is.na(shallow_chla_lab),shallow_chla_lab,
                ifelse(!is.na(nla17_chla),nla17_chla,
                ifelse(!is.na(chl_predicted_sample_month),chl_predicted_sample_month,"NA"))))
tropew$trophicpred<-as.numeric(tropew$trophicpred)

tropew$trophic=ifelse(tropew$trophicpred<12,"Oligotrophic",ifelse(tropew$trophicpred>24,"Eutrophic","Mesotrophic"))
tropew$ebu=tropew$ch4_ebullition*24
tropew$ebup=ifelse(tropew$ebu>0,tropew$ebu,"0.0001")

tropd_plot<-tropew %>%
  ggplot(aes(x=trophic,y=ebu))+
  scale_color_manual(values=c("green4", "coral4", "#0077b6"))+
  geom_boxplot(aes(color=trophic))+
  annotate("text", label =expression(paste("185 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =1, y = 10000)+
  annotate("text", label =expression(paste("79 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =2, y = 10000)+
  annotate("text", label =expression(paste("46 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =3, y = 10000)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  ylab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  xlab("")
tropd_plot

ggsave("trophic_status.jpg",width=6,height=3.5,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

s<-filter(tropew,trophic=="Oligotrophic")
summary(s$ch4_ebullition)

tropt_plot<-dat %>%
  mutate(ch4tot=ch4_total*24)%>%
  mutate(trophicpred=as.numeric(ifelse(!is.na(shallow_chla_lab),shallow_chla_lab,
                            ifelse(!is.na(nla17_chla),nla17_chla,
                                   ifelse(!is.na(chl_predicted_sample_month),chl_predicted_sample_month,"NA")))))%>%
  mutate(trophic=ifelse(trophicpred<12,"Oligotrophic",ifelse(trophicpred>24,"Eutrophic","Mesotrophic")))%>%
  ggplot(aes(x=trophic,y=ch4tot))+
  scale_color_manual(values=c("green4", "coral4", "#0077b6"))+
  geom_boxplot(aes(color=trophic))+
  # annotate("text", label =expression(paste("185 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =1, y = 10000)+
  # annotate("text", label =expression(paste("79 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =2, y = 10000)+
  # annotate("text", label =expression(paste("46 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =3, y = 10000)+
  scale_y_log10()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  ylab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  xlab("")
tropt_plot

tplot<- trope %>%
  ggplot(aes(x=ebu,color=trophic,fill=trophic))+
  geom_density(alpha=0.1)+
  scale_x_log10()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        legend.position="top")+
  xlab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  ylab("Density")
tplot

oli<-filter(trope,trophic=="Oligotrophic")
mean(oli$ebu)

mes<-filter(trope,trophic=="Mesotrophic")
mean(mes$ebu)

eut<-filter(trope,trophic=="Eutrophic")
mean(eut$ebu)

##### Morphometry
#filter to the subset of sites that has information on all the predictors

morp<-dat %>%
  filter( !is.na(ch4_diffusion_best),
          !is.na(site_depth),
          !is.na(surface_area),
          !is.na(shoreline_development), 
          !is.na(volume), 
          !is.na(circularity),
          !is.na(dynamic_ratio),
          !is.na(littoral_fraction),
          !is.na(max_depth),
          !is.na(mean_depth),
          !is.na(fetch))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(ch4_diffusion=ch4_diffusion_best,
         ch4_diffusion_best=log(ch4_diffusion_best+1))%>%
  select(site_depth, surface_area, shoreline_development, volume, circularity,
        dynamic_ratio, littoral_fraction, max_depth, mean_depth, ch4_diffusion_best,
         ch4_diffusion,lake_id,fetch)%>%
  filter(!is.na(ch4_diffusion_best))

options(scipen = 0)
shapiro.test(log(dat$mean_depth+1))

m<-cor(morp)
corrplot(m,method="number")

a<-lmer(morp$ch4_diffusion_best~morp$site_depth+(1|morp$lake_id),REML=FALSE)
summary(a)
#2544.5

b<-lmer(morp$ch4_diffusion_best~morp$surface_area+(1|morp$lake_id),REML=FALSE)
summary(b)
#2559.2

c<-lmer(morp$ch4_diffusion_best~morp$shoreline_development+(1|morp$lake_id),REML=FALSE)
summary(c)
#2553.4

d<-lmer(morp$ch4_diffusion_best~morp$volume+(1|morp$lake_id),REML=FALSE)
summary(d)
#2564.8

e<-lmer(morp$ch4_diffusion_best~morp$circularity+(1|morp$lake_id),REML=FALSE)
summary(e)
#2569.9

f<-lmer(morp$ch4_diffusion_best~morp$dynamic_ratio+(1|morp$lake_id),REML=FALSE)
summary(f)
#2563.8

g<-lmer(morp$ch4_diffusion_best~morp$littoral_fraction+(1|morp$lake_id),REML=FALSE)
summary(g)
#2561.8

h<-lmer(morp$ch4_diffusion_best~morp$max_depth+(1|morp$lake_id),REML=FALSE)
summary(h)
#2564.2

i<-lmer(morp$ch4_diffusion_best~morp$mean_depth+(1|morp$lake_id),REML=FALSE)
summary(i)
#2565.1

j<-lmer(morp$ch4_diffusion_best~morp$fetch+(1|morp$lake_id),REML=FALSE)
summary(j)
#2554.1

#now morphometry for ebullition
morpe<-dat %>%
  filter( !is.na(ch4_ebullition),
          !is.na(site_depth),
          !is.na(surface_area),
          !is.na(shoreline_development), 
          !is.na(volume), 
          !is.na(circularity),
          !is.na(dynamic_ratio),
          !is.na(littoral_fraction),
          !is.na(max_depth),
          !is.na(mean_depth),
          !is.na(fetch))%>%
  select(site_depth, surface_area, shoreline_development, volume, circularity,fetch,
         dynamic_ratio, littoral_fraction, max_depth, mean_depth, ch4_ebullition,lake_id)

n<-cor(morpe)
corrplot(m,method="number")

aa<-lmer(morpe$ch4_ebullition~morpe$site_depth+(1|morpe$lake_id),REML=FALSE)
summary(aa)
#14097.7

bb<-lmer(morpe$ch4_ebullition~morpe$surface_area+(1|morpe$lake_id),REML=FALSE)
summary(bb)
#14119.3

cc<-lmer(morpe$ch4_ebullition~morpe$shoreline_development+(1|morpe$lake_id),REML=FALSE)
summary(cc)
#14115.2

dd<-lmer(morpe$ch4_ebullition~morpe$volume+(1|morpe$lake_id),REML=FALSE)
summary(dd)
#14118.9

ee<-lmer(morpe$ch4_ebullition~morpe$circularity+(1|morpe$lake_id),REML=FALSE)
summary(ee)
#14116.0

ff<-lmer(morpe$ch4_ebullition~morpe$dynamic_ratio+(1|morpe$lake_id),REML=FALSE)
summary(ff)
#14115.6

gg<-lmer(morpe$ch4_ebullition~morpe$littoral_fraction+(1|morpe$lake_id),REML=FALSE)
summary(gg)
#14118.7

hh<-lmer(morpe$ch4_ebullition~morpe$max_depth+(1|morpe$lake_id),REML=FALSE)
summary(hh)
#14118.6

ii<-lmer(morpe$ch4_ebullition~morpe$mean_depth+(1|morpe$lake_id),REML=FALSE)
summary(ii)
#14118.0

jj<-lmer(ch4_ebullition~fetch+(1|lake_id),REML=FALSE,data=morpe)
summary(jj)
#14118.7

#Build model with both site depth and shoreline development for plotting
kk<-lmer(ch4_ebullition~shoreline_development+site_depth+(1|lake_id),REML=FALSE,data=morpe)
summary(kk)
#1493

#Setup a datset with a couple shoreline development values spanning the range of site depths

site_depth<-c(0.3,1,1.8,2.5,3.3,5.002,6.2,15,75.3)
shoreline_development<-c(1.11,1.11,1.11,1.11,1.11,1.11,1.11,1.11,1.11)
#Picked lake with a mean ebullition near the mean of the whole dataset
lake_id=c(1001,1001,1001,1001,1001,1001,1001,1001,1001)
shoreline2<-c(3.674,3.674,3.674,3.674,3.674,3.674,3.674,3.674,3.674)
shoreline3<-c(12.09,12.09,12.09,12.09,12.09,12.09,12.09,12.09,12.09)
fakedata<-data.frame(site_depth,shoreline_development,lake_id)
fakedata2<-data.frame(site_depth,shoreline3,lake_id)


kkk<-predict(kk,newdata=fakedata, random.only=FALSE,re.form=NA)
lll<-predict(kk,newdata=fakedata2, random.only=FALSE,re.form=NA)
sd1<-data.frame(site_depth,shoreline_development,lake_id,kkk)
sd2<-data.frame(site_depth,shoreline_development,lake_id,lll)
kkkk<-lm(kkk~site_depth,data=sd1)
llll<-lm(lll~site_depth,data=sd2)
summary(kkkk)
summary(llll)
#Plot of depth versus both ebullition and diffusion

depdifplot<-dat %>%
  filter(!is.na(shoreline_development))%>%
  filter(!is.na(site_depth))%>%
  filter(!is.na(ch4_diffusion_best))%>%
  mutate(dif=ch4_diffusion_best*24)%>%
  ggplot(aes(x=site_depth,y=dif))+
  geom_point(aes(alpha=0.4,color="#1B9E77"))+
  scale_color_manual(values="#1B9E77")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10()+
  ylab(expression(paste("Diffusion (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  labs(color="Shoreline Complexity")+
  guides(alpha=FALSE)+
  xlab("Site Depth (m)")
depdifplot

ggsave("diffusive_depth.jpg",width=3,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")
#ggsave("sc_legend.jpg",width=6,height=3,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

dat$surface_area_bin= ifelse(dat$surface_area<5000000,"small",ifelse(dat$surface_area>5000000,"large","NA"))

surface_area_plot<-dat %>%
  mutate(dif=ch4_diffusion_best*24)%>%
  mutate(sa=surface_area/1000000)%>%
  mutate(size=ifelse(sa<5,"small (<5 km2)","large (>5 km2)"))%>%
  filter(!is.na(ch4_diffusion_best))%>%
  filter(!is.na(size))%>%
  ggplot(aes(x=size,y=dif))+
  geom_boxplot()+
  annotate("text", label =expression(paste("35 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =1, y = 10000)+
  annotate("text", label =expression(paste("105 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =2, y = 10000)+
  scale_color_gradient2()+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab(expression(paste("Diffusion (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="top")+
  labs(color="Shoreline Complexity")+
  guides(alpha=FALSE)+
  xlab("Surface Area")
surface_area_plot

ggsave("surface_area.jpg",width=6,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

sAebu<-dat %>%
  filter(!is.na(shoreline_development))%>%
  filter(!is.na(site_depth))%>%
  filter(!is.na(ch4_ebullition))%>%
  mutate(ebu=ch4_ebullition*24)%>%
  ggplot(aes(x=site_depth,y=ebu))+
  geom_point(aes(alpha=0.4,color="#D95F02"))+
  scale_color_manual(values="#D95F02")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10()+
  ylab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  xlab("Site Depth (m)")
sAebu

ggsave("ebullitive_depth.jpg",width=3,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

## Now examine sediment characteristics

#calculate per area sedimentation
dat$sedimentation_cubic_meters_ha_yr=dat$sedimentation_m3y/dat$surface_area
dat$C_sedimentation_cm_m2_yr=dat$C_sedimentation_m3y/dat$surface_area

sedi<-dat %>%
  filter( !is.na(ch4_diffusion_best),
          !is.na(sedimentation_cubic_meters_ha_yr),
          !is.na(C_sedimentation_cm_m2_yr), 
          !is.na(kffactcat), 
          !is.na(omcat),
          !is.na(scat),
          !is.na(year_completed)
  )%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(ch4_diffusion=ch4_diffusion_best,
         ch4_diffusion_best=log(ch4_diffusion_best+1)
         )%>%
  select(sedimentation_cubic_meters_ha_yr, C_sedimentation_cm_m2_yr, kffactcat,
         omcat,scat, ch4_diffusion_best,lake_id,ch4_diffusion, year_completed)%>%
  filter(!is.na(ch4_diffusion_best))

options(scipen = 0)
shapiro.test(log(dat$sedimentation_cubic_meters_ha_yr))

m<-cor(sedi)
corrplot(m,method="number")

a<-lmer(sedi$ch4_diffusion_best~sedi$sedimentation_cubic_meters_ha_yr+(1|sedi$lake_id),REML=FALSE)
summary(a)
#2618.9

b<-lmer(sedi$ch4_diffusion_best~sedi$C_sedimentation_cm_m2_yr+(1|sedi$lake_id),REML=FALSE)
summary(b)
#2611.3

c<-lmer(sedi$ch4_diffusion_best~sedi$omcat+(1|sedi$lake_id),REML=FALSE)
summary(c)
#2616.6

d<-lmer(sedi$ch4_diffusion_best~sedi$kffactcat+(1|sedi$lake_id),REML=FALSE)
summary(d)
#2621.0

e<-lmer(sedi$ch4_diffusion_best~sedi$scat+(1|sedi$lake_id),REML=FALSE)
summary(e)
#2620.7

f<-lmer(sedi$ch4_diffusion_best~sedi$year_completed+(1|sedi$lake_id),REML=FALSE)
summary(f)
#2619.9

## and now for ebullition
sedie<-dat %>%
  filter( !is.na(ch4_ebullition),
          !is.na(sedimentation_m3y),
          !is.na(C_sedimentation_cm_m2_yr), 
          !is.na(kffactcat), 
          !is.na(omcat),
          !is.na(scat),
          !is.na(year_completed)
  )%>%
  select(sedimentation_m3y, C_sedimentation_cm_m2_yr, kffactcat,year_completed,
         omcat,scat, ch4_diffusion_best,lake_id,ch4_ebullition)


n<-cor(sedi)
corrplot(n,method="number")

aa<-lmer(sedie$ch4_ebullition~sedie$sedimentation_m3y+(1|sedie$lake_id),REML=FALSE)
summary(aa)
#15056.4

bb<-lmer(sedie$ch4_ebullition~sedie$C_sedimentation_cm_m2_yr+(1|sedie$lake_id),REML=FALSE)
summary(bb)
#15055.8

cc<-lmer(sedie$ch4_ebullition~sedie$omcat+(1|sedie$lake_id),REML=FALSE)
summary(cc)
#15053.1

dd<-lmer(sedie$ch4_ebullition~sedie$kffactcat+(1|sedie$lake_id),REML=FALSE)
summary(dd)
#15052.2

ee<-lmer(sedie$ch4_ebullition~sedie$scat+(1|sedie$lake_id),REML=FALSE)
summary(ee)
#15056.8

ff<-lmer(sedie$ch4_ebullition~sedie$year_completed+(1|sedie$lake_id),REML=FALSE)
summary(ff)
#15056.9

#Plot of erodability vs. ebullition
options(scipen = 999)

kffebuplot<-sedie %>%
  mutate(ebu=ch4_ebullition*24)%>%
  ggplot(aes(x=kffactcat,y=ebu))+
  geom_point()+
  scale_y_log10()+
  #scale_x_log10()+
  ylab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        legend.position="none")+
  xlab("Catchment Soil Erodability")
kffebuplot

csediffplot<-sedi %>%
  mutate(diff=ch4_diffusion*24)%>%
  ggplot(aes(x=sedimentation_cubic_meters_ha_yr,y=diff))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()+
  ylab(expression(paste("Diffusion (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        legend.position="none")+
  xlab("C Sedimentation m3 per ha per y")
csediffplot

## Now examine hydrology and salinity
#fix buoyancy frequency so if thermocline depth is zero buoyancy frequency is 
#the minimum value in the dataset

#dat$buoyff<-ifelse(dat$thermdep2==0,0.45,dat$buoyf)

hydr<-dat %>%
  filter( !is.na(ch4_diffusion_best),
          !is.na(RT),
          !is.na(E_I), 
          !is.na(buoyf), 
          !is.na(shallow_sp_cond),
          !is.na(shallow_s),
          !is.na(shallow_temp),
          !is.na(deep_do_mg)
  )%>%
  mutate(shallow_chla_sonde=ifelse(shallow_chla_sonde<0.1,0.1,shallow_chla_sonde))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(ch4_diffusion=ch4_diffusion_best,
         ch4_diffusion_best=log(ch4_diffusion_best+1),
         shallow_s=log(shallow_s),
         shallow_sp_cond=log(shallow_sp_cond),
         buoyancy=buoyf,
         buoyf=log(buoyf),
         RT=log(RT)
)%>%
  select(RT, E_I, buoyf, shallow_sp_cond, shallow_s, shallow_temp,
         ch4_diffusion_best,lake_id,ch4_diffusion,deep_do_mg,buoyancy)

shapiro.test((hydr$shallow_sp_cond))

m<-cor(hydr)
corrplot(m,method="number")

a<-lmer(hydr$ch4_diffusion_best~hydr$RT+(1|hydr$lake_id),REML=FALSE)
summary(a)
#1966.7

b<-lmer(hydr$ch4_diffusion_best~hydr$E_I+(1|hydr$lake_id),REML=FALSE)
summary(b)
#1968.6

c<-lmer(hydr$ch4_diffusion_best~hydr$buoyf+(1|hydr$lake_id),REML=FALSE)
summary(c)
#1958.1

d<-lmer(hydr$ch4_diffusion_best~hydr$shallow_sp_cond+(1|hydr$lake_id),REML=FALSE)
summary(d)
#1965.4

e<-lmer(hydr$ch4_diffusion_best~hydr$shallow_s+(1|hydr$lake_id),REML=FALSE)
summary(e)
#1968.4

f<-lmer(hydr$ch4_diffusion_best~hydr$shallow_temp+(1|hydr$lake_id),REML=FALSE)
summary(f)
#1965.1

buodiffplot<-dat %>%
  filter(!is.na(buoyf))%>%
  filter(!is.na(ch4_diffusion_best))%>%
  mutate(diff=ch4_diffusion_best*24)%>%
  ggplot(aes(x=buoyf,y=diff))+
  geom_point(aes(alpha=0.4,color="#1B9E77"))+
  scale_color_manual(values="#1B9E77")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10()+
  ylab(expression(paste("Diffusion (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  xlab("Buoyancy Frequency")
buodiffplot

ggsave("buoyancy_frequency.jpg",width=3,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")


#now ebullition

hydre<-dat %>%
  filter( !is.na(ch4_ebullition),
          !is.na(RT),
          !is.na(E_I), 
          !is.na(buoyf), 
          !is.na(deep_sp_cond),
          !is.na(deep_s),
          !is.na(deep_temp),
          !is.na(deep_do_mg)
  )%>%
  mutate(shallow_chla_sonde=ifelse(shallow_chla_sonde<0.1,0.1,shallow_chla_sonde))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(deep_s=log(shallow_s),
         buoyancy=buoyf,
         buoyf=log(buoyf),
         RT=log(RT)
  )%>%
  select(RT, E_I, buoyf, deep_sp_cond, deep_s, deep_temp,
         lake_id,ch4_ebullition,deep_do_mg,buoyancy)

shapiro.test(log(hydre$deep_do_mg+1))

n<-cor(hydre)
corrplot(n,method="number")

aa<-lmer(hydre$ch4_ebullition~hydre$RT+(1|hydre$lake_id),REML=FALSE)
summary(aa)
#9742.8

bb<-lmer(hydre$ch4_ebullition~hydre$E_I+(1|hydre$lake_id),REML=FALSE)
summary(bb)
#9747.5

cc<-lmer(hydre$ch4_ebullition~hydre$buoyf+(1|hydre$lake_id),REML=FALSE)
summary(cc)
#9742.6

dd<-lmer(hydre$ch4_ebullition~hydre$deep_sp_cond+(1|hydre$lake_id),REML=FALSE)
summary(dd)
#9747.5

ee<-lmer(hydre$ch4_ebullition~hydre$deep_s+(1|hydre$lake_id),REML=FALSE)
summary(ee)


ff<-lmer(hydre$ch4_ebullition~hydre$deep_temp+(1|hydre$lake_id),REML=FALSE)
summary(ff)
#9733.0


gg<-lmer(hydre$ch4_ebullition~hydre$deep_do_mg+(1|hydre$lake_id),REML=FALSE)
summary(gg)
#9746.5

options(scipen = 999)
temebuplot<-dat %>%
  filter(!is.na(ch4_ebullition))%>%
  filter(!is.na(deep_temp))%>%
  filter(deep_temp>4.5)%>%
  mutate(ebu=ch4_ebullition*24)%>%
  ggplot(aes(x=deep_temp,y=ebu))+
  geom_point(aes(alpha=0.4,color="#D95F02"))+
  scale_color_manual(values="#D95F02")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10()+
  ylab(expression(paste("Ebullition (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  xlab("Bottom Temp (C)")
temebuplot

ggsave("ebullition_temp.jpg",width=3,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

## Let's check out total methane emission by sulfate per Jake's suggestion

#try for diffusion
sulfur<-dat%>%
  filter(!is.na(ch4_diffusion_best))%>%
  filter(!is.na(deep_s))%>%
  mutate(ch4dif=ch4_diffusion_best*24)%>%
  mutate(sulfur=ifelse(deep_s<70,"Low Sulfate","High Sulfate"))%>%
  ggplot(aes(x=sulfur,y=ch4dif))+
  geom_boxplot()+
  annotate("text", label =expression(paste("49 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =1, y = 10000)+
  annotate("text", label =expression(paste("106 mg CH"[4]*" m"^"-2"*"d"^"-1"*"")), size = 4, x =2, y = 10000)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  ylab(expression(paste("Diffusion (mg CH"[4]*" m"^"-2"*"d"^"-1"*")")))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position="none")+
  xlab("")
sulfur

ggsave("sulfate.jpg",width=5,height=3,dpi=300,units="in",path="~/National_Reservoir_GHG_Survey/AGU_Poster")

### Best Model For Diffusion

# Going to test site depth, surface area bin, shoreline development, sonde chlorophyll a,
# buoyancy frequency,C sedimentation

# Looking for an interaction between buoyancy frequency and sonde chlorophyll a

bestd<-dat %>%
  filter( !is.na(ch4_diffusion_best),
          !is.na(shallow_chla_sonde),
          !is.na(shoreline_development), 
          !is.na(buoyf),
          !is.na(site_depth),
          !is.na(C_sedimentation_cm_m2_yr),
  )%>%
  mutate(shallow_chla_sonde=ifelse(shallow_chla_sonde<0.1,0.1,shallow_chla_sonde))%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(ch4_diffusion=ch4_diffusion_best,
         ch4_diffusion_best=log(ch4_diffusion_best+1),
         shallow_chla_sonde_log=log(shallow_chla_sonde),
         buoyancy=buoyf,
         buoyf=log(buoyf)
  )%>%
  select(buoyf,shallow_chla_sonde,shallow_chla_sonde_log,shoreline_development,site_depth,
         #surface_area_bin, 
         ch4_diffusion_best,lake_id,ch4_diffusion,buoyancy,C_sedimentation_cm_m2_yr)

bd<-cor(bestd)
corrplot(bd,method="number")

a<-lmer(bestd$ch4_diffusion_best~bestd$shallow_chla_sonde+(1|bestd$lake_id),REML=FALSE)
summary(a)
#2540.8

b<-lmer(bestd$ch4_diffusion_best~bestd$buoyf+(1|bestd$lake_id),REML=FALSE)
summary(b)
#2537.4

c<-lmer(bestd$ch4_diffusion_best~bestd$C_sedimentation_cm_m2_yr+(1|bestd$lake_id),REML=FALSE)
summary(c)
#2547.5

d<-lmer(bestd$ch4_diffusion_best~bestd$shoreline_development+(1|bestd$lake_id),REML=FALSE)
summary(d)
#2540.6

e<-lmer(bestd$ch4_diffusion_best~bestd$site_depth+(1|bestd$lake_id),REML=FALSE)
summary(e)
#2521.8

#Test some plausible interactions
e<-lmer(bestd$ch4_diffusion_best~bestd$C_sedimentation_cm_m2_yr*bestd$buoyf+(1|bestd$lake_id),REML=FALSE)
summary(e)
#2538.4- Does not improve over model with buoyancy frequency only

f<-lmer(bestd$ch4_diffusion_best~bestd$shallow_chla_sonde*bestd$buoyf+(1|bestd$lake_id),REML=FALSE)
summary(f)
#2511.7 # better than the best single model

g<-lmer(bestd$ch4_diffusion_best~bestd$shallow_chla_sonde*bestd$site_depth+(1|bestd$lake_id),REML=FALSE)
summary(g)
#2518.8 #also better than the best single model, but not as good as buoyancy frequency interaction

#Test some additive models
h<-lmer(bestd$ch4_diffusion_best~bestd$shallow_chla_sonde*bestd$buoyf+bestd$site_depth+(1|bestd$lake_id),REML=FALSE)
summary(h)
#2496.8

##BEST MODEL
i<-lmer(ch4_diffusion_best~shoreline_development+shallow_chla_sonde*buoyf+site_depth+(1|lake_id),REML=FALSE,data=bestd)
summary(i)
#2492.8

j<-lmer(bestd$ch4_diffusion_best~bestd$shoreline_development+bestd$shallow_chla_sonde*bestd$site_depth+bestd$buoyf+(1|bestd$lake_id),REML=FALSE)
summary(j)
#2506.2

k<-lmer(bestd$ch4_diffusion_best~bestd$C_sedimentation_cm_m2_yr+
          bestd$shallow_chla_sonde*bestd$buoyf+bestd$site_depth+(1|bestd$lake_id),REML=FALSE)
summary(k)
#2494.7-- sedimentation isn't improving model more than 2 AIC over base

l<-lmer(bestd$ch4_diffusion_best~bestd$shallow_chla_sonde+
          bestd$shallow_chla_sonde*bestd$buoyf+bestd$site_depth+(1|bestd$lake_id),REML=FALSE)
summary(l)
#2496.8-- chlorophyll and buoyancy frequency on their own aren't improving the mode
#over AIC base either

#Make an interaction visualization

chlbuoplot<-dat %>%
  filter(!is.na(buoyf))%>%
  filter(!is.na(ch4_diffusion_best))%>%
  filter(!is.na(shallow_chla_sonde))%>%
  mutate(buoy_chl=buoyf*shallow_chla_sonde)%>%
  ggplot(aes(x=buoy_chl,y=ch4_diffusion_best,color=site_depth))+
  geom_point(aes(color=site_depth))+
  scale_x_log10()+
  scale_y_log10()
chlbuoplot


#Now do the same exercise for ebullition 

beste<-dat %>%
  filter( !is.na(ch4_ebullition),
          !is.na(shallow_chla_lab),
          !is.na(shoreline_development), 
          !is.na(deep_temp),
          !is.na(site_depth),
          !is.na(kffactcat),
  )%>%
  #make log transformations when shapiro test results are noticeably improved
  mutate(shallow_chla_lab_log=log(shallow_chla_lab))%>%
  select(shoreline_development,shallow_chla_lab,shallow_chla_lab_log,deep_temp,site_depth,
         ch4_ebullition,kffactcat,lake_id)

be<-cor(beste)
corrplot(be,method="number")

a<-lmer(beste$ch4_ebullition~beste$deep_temp+(1|beste$lake_id),REML=FALSE)
summary(a)
#11309

b<-lmer(beste$ch4_ebullition~beste$site_depth+(1|beste$lake_id),REML=FALSE)
summary(b)
#11318.7

c<-lmer(beste$ch4_ebullition~beste$kffactcat+(1|beste$lake_id),REML=FALSE)
summary(c)
#11330.6

d<-lmer(beste$ch4_ebullition~beste$shoreline_development+(1|beste$lake_id),REML=FALSE)
summary(d)
#11330.7

e<-lmer(beste$ch4_ebullition~beste$shallow_chla_lab+(1|beste$lake_id),REML=FALSE)
summary(e)
#11289.7

#Now test some plausible interactions

f<-lmer(beste$ch4_ebullition~beste$shallow_chla_lab*beste$site_depth+(1|beste$lake_id),REML=FALSE)
summary(f)
#11282.3

#### BEST MODEL
g<-lmer(ch4_ebullition~shallow_chla_lab*deep_temp+(1|lake_id),REML=FALSE,data=beste)
summary(g)
#11260.4

shallow_chla_lab<-c(0.146,2.666,5.892,20.209,19.387,236.47)
deep_temp<-c(18,18,18,18,18,18)
fakedata<-data.frame(shallow_chla_lab,deep_temp)

gg<-predict(g,newdata=fakedata,random.only=FALSE,re.form=NA)
fakedata$predch4<-gg
ggg<-lm(fakedata$predch4~fakedata$shallow_chla_lab)


deep_temp2<-c(24,24,24,24,24,24)
fakedata2<-data.frame(shallow_chla_lab,deep_temp2)

gg2<-predict(g,newdata=fakedata2,random.only=FALSE,re.form=NA)
fakedata2$predch4<-gg2
ggg2<-lm(fakedata2$predch4~fakedata2$shallow_chla_lab)
summary(ggg2)

k<-lmer(ch4_ebullition~shallow_chla_lab+deep_temp+(1|lake_id),REML=FALSE,data=beste)
summary(k)
#11273.1-- interactive is 13 AIC better than additive

interplot<-dat %>%
  filter(!is.na(shallow_chla_lab))%>%
  filter(!is.na(deep_temp))%>%
  filter(!is.na(ch4_ebullition))%>%
  ggplot(aes(x=shallow_chla_lab,y=ch4_ebullition))+
  geom_point(aes())+
  geom_abline(intercept=1.3469,slope=0.0185,color="blue")+
  scale_y_log10()+
  scale_x_log10()
interplot

predict(g,)

# #Plot interaction from best model
# library(effects)
# 
# #cut temperature into categorical variable
# beste$tempcut<-cut(beste$deep_temp, breaks=10)
# 
# effect(beste$shallow_chla_lab_log:beste$tempcut)

#Now build up best model

h<-lmer(beste$ch4_ebullition~beste$site_depth+beste$shallow_chla_lab*beste$deep_temp+(1|beste$lake_id),REML=FALSE)
summary(h)
#11260.6- site depth doesn't really add much here

i<-lmer(beste$ch4_ebullition~beste$shoreline_development+beste$shallow_chla_lab*beste$deep_temp+(1|beste$lake_id),REML=FALSE)
summary(i)
#11262.4- shoreline development doesn't really add much here

j<-lmer(beste$ch4_ebullition~beste$kffactcat+beste$shallow_chla_lab*beste$deep_temp+(1|beste$lake_id),REML=FALSE)
summary(j)
#11258.5- k factor doesn't quite improve this by 2 AIC

