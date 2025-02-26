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
breaks <-c(-10^(nbreaks:1),10^(nbreaks:1))

densplotco2<-diebc%>%
  mutate(rt=rate*24)%>%
  mutate(rtc=rt*(12.01/44.009))%>%
  ggplot(aes(x=rt,color=type,fill=type))+
  geom_density(alpha=0.1)+
  scale_color_manual(values = c("#56B4E9","#009E73","#D55E00"))+
  scale_fill_manual(values = c("#56B4E9","#009E73","#D55E00"))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14,angle = 90),
        axis.title = element_text(size = 16),
        legend.position="none")+
  scale_x_continuous(trans = pseudo_log_trans(sigma = 10^(-nbreaks), base = 10),breaks=breaks)+
  xlab(expression(paste("Carbon Dioxide (mg CO"[2]*" m"^"-2"*"d"^"-1"*")")))+
  ylab("Density")
densplotco2

densplotco2b<-totc%>%
  mutate(rt=rate*24)%>%
  mutate(rtc=rt*(12.01/44.009))%>%
  ggplot(aes(x=rt,color=type,fill=type))+
  geom_density(alpha=0.1)+
  scale_color_manual(values = "#D55E00")+
  scale_fill_manual(values = "#D55E00")+
  geom_vline(xintercept=0)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"),legend.title=element_blank(),
        axis.text = element_text(size = 14,angle = 90),
        axis.title = element_text(size = 16),
        legend.position="none")+
  xlab(expression(paste("Carbon Dioxide (mg CO"[2]*" m"^"-2"*"d"^"-1"*")")))+
  ylab("Density")
densplotco2b

dens<-plot_grid(densplot,densplotco2,ncol=1,align="v",labels=c("A","B"),rel_heights = c(1,1))
dens