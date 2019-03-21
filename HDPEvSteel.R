# HDPE vs Steel analysis
#mined PHMSA incident data for comparison
library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)
library(reshape2)


gdist10 <- gd2010toPresent <- read_excel("gd2010toPresent.xlsx", 
                                         sheet = "gd2010toPresent")
gdist0409 <- read_excel("gdmar2004to2009.xlsx",
                        sheet = "gdmar2004to2009")

#subset the most useful variables####
gdsub10 <- gdist10[,c(2:4,7,36,43,73,83,120,124,158:159,96)]

gsub0409 <- gdist0409[,c(2:4,16,34,38,44,61,67,71,89,90,27)]
nm <- names(gsub0409)

nm[4] <- "REPORT_RECEIVED_DATE"
nm[10] <- "MATERIAL_INVOLVED"
nm[9] <- "SYSTEM_PART_INVOLVED"
nm[8] <- "ACCIDENT_PSIG"
nm[13] <- "RELEASE_TYPE"
names(gsub0409) <- nm

gdist_all <- full_join(gdsub10, gsub0409)
gdist_all$year <- year(gdist_all$REPORT_RECEIVED_DATE)


#Clean up categories ####
gdist_all$MATERIAL_INVOLVED <- revalue(gdist_all$MATERIAL_INVOLVED,c("PLASTIC"="PE"))
gdist_all$MATERIAL_INVOLVED <- revalue(gdist_all$MATERIAL_INVOLVED,c("POLYETHELENE PLASTIC"="PE"))

#Material Count ####
gdist_all %>% filter(FF=="NO",SYSTEM_PART_INVOLVED=="MAIN") %>% group_by(MATERIAL_INVOLVED) %>% count()
pesteel <- gdist_all %>% filter(MATERIAL_INVOLVED=="PE"|MATERIAL_INVOLVED=="STEEL", SYSTEM_PART_INVOLVED=="MAIN",FF=="NO")
pesteel <- pesteel %>% mutate(CoF=FATAL*10E6+INJURE*10E6/3+TOTAL_COST_CURRENT)
pesteel$RELEASE_TYPE <- ifelse(pesteel$RELEASE_TYPE=="N/A",NA, pesteel$RELEASE_TYPE)
pesteel

byyear <- pesteel %>% filter(year>2003 & year<2019) %>% group_by(year,MATERIAL_INVOLVED) %>% summarise(fatal=sum(FATAL), injure=sum(INJURE), CoF=sum(CoF), n=length(SERIOUS))
count_cause <- pesteel %>% group_by(MATERIAL_INVOLVED, year, MAP_CAUSE) %>% count()

ggplot(pesteel, aes(CoF))+
  geom_histogram(aes(fill=MATERIAL_INVOLVED),col='black')+
  facet_grid(~MATERIAL_INVOLVED)+
  scale_x_log10()+
  theme_bw(16,"Serif")+
  theme(legend.position = c(0.10,0.85), legend.background = element_rect(color = 'black'), plot.margin = margin(0.6,0.6,0.6,0.6,"cm"))+
  guides(fill=guide_legend(title = "Material"))+
  labs(title = "Consequence of Failure for PE and Steel Incidents",subtitle = "PHMSA Gas Distribution incidents (2004 - 2019)", x="CoF in Dollars (Log Scale)",y="Count")

#by cause
ggplot(pesteel, aes(MAP_CAUSE))+
  geom_bar(aes(fill=MATERIAL_INVOLVED),position = "dodge", col='black')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = -45,hjust = 0))

#excluding 3rd party damage

ggplot(filter(pesteel,MAP_SUBCAUSE!="THIRD PARTY EXCAVATION DAMAGE"), aes(MAP_CAUSE))+
  geom_bar(aes(fill=MATERIAL_INVOLVED),col='black',position = "dodge")+
  theme_bw(16,"serif")+
  theme(axis.text.x = element_text(angle = -45,hjust = 0),legend.position = c(0.15,0.85),legend.background = element_rect(color = 'black'),plot.margin=margin(2,2,2,2,"cm"))+
  labs(title = "Incident Count by Cause and Material Type", subtitle = "PHMSA incident data 2004 - 2019 (Excluding Excavation Damage)",x= "Mapped Cause", y="Count")

#including Ex. damage

ggplot(pesteel, aes(MAP_CAUSE))+
  geom_bar(aes(fill=MATERIAL_INVOLVED),col='black',position = "dodge")+
  theme_bw(18,"serif")+
  theme(axis.text.x = element_text(angle = -45,hjust = 0),legend.position = c(0.15,0.85),legend.background = element_rect(color = 'black'),plot.margin=margin(2,2.2,2,2,"cm"))+
  labs(title = "Incident Count by Cause and Material Type", subtitle = "PHMSA incident data 2004 - 2019",x= "Mapped Cause", y="Count")

ggplot(filter(pesteel,MAP_SUBCAUSE!="THIRD PARTY EXCAVATION DAMAGE"), aes(MAP_SUBCAUSE))+
  geom_bar(aes(fill=MATERIAL_INVOLVED),col='black',position = "dodge")+
  theme_bw(16,"serif")+
  theme(axis.text.x = element_text(angle = -45,hjust = 0),legend.position = c(0.90,0.85),legend.background = element_rect(color = 'black'),plot.margin=margin(2,6,2,1,"cm"))+
  labs(title = "Incident Count by Sub-Cause and Material Type", subtitle = "PHMSA incident data 2004 - 2019 (Excluding Excavation Damage)",x= "Mapped Sub-Cause", y="Count") + guides(fill=guide_legend(title = "Material"))

#relaease type plot ####
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0]

ggplot(pesteel, aes(RELEASE_TYPE))+
  geom_bar(aes(fill=MATERIAL_INVOLVED),col='black',position = "dodge", alpha=0.95)+
  theme_bw(18,"serif")+
  theme(axis.text.x = element_text(angle = -30,hjust = 0, size = rel(0.75)),
        legend.position = c(0.90,0.05),legend.background = element_rect(color = 'black'),
        plot.margin=margin(1,1,1,1,"cm"), axis.title.x = element_text(size=rel(1.5)))+
  labs(title = "Incident Count by Cause and Material Type", 
       subtitle = "PHMSA Distribution Incident Data (2004 - 2019)",x= "Release Type", y="Count", caption = "Incidents on mains only, excluding \"fire first\" incidents\n Note varying y-scales ")+
  scale_fill_brewer(palette = "Set1", type = "div")+ 
  guides(fill=guide_legend(title = "Material"))+facet_wrap(~MAP_CAUSE, scales = "free_y")+
  scale_y_continuous(breaks= int_breaks )


#MCMC Analysis of PE and Steel CoF
library(BEST)
best_pest <- BESTmcmc(log10( pesteel_fil$CoF[which(x = pesteel_fil$MATERIAL_INVOLVED=="STEEL")]),
                      log10(pesteel_fil$CoF[which(x = pesteel_fil$MATERIAL_INVOLVED=="PE")]),
                      numSavedSteps = 5e4)

plotPost(best_pest$mu1 - best_pest$mu2, compVal = 0, xlab = bquote(mu[Steel] - mu[PE]), 
         cex.lab = 1.75, main = "Difference of Mean CoF", 
         col = "skyblue2")

#mileage annual report data ####
gd2017 <- read_excel("annual_gas_distribution_2017.xlsx", skip = 2)
gd2016 <- read_excel("annual_gas_distribution_2016.xlsx", skip = 2)
gd2015 <- read_excel("annual_gas_distribution_2015.xlsx", skip = 2)
gd2014 <- read_excel("annual_gas_distribution_2014.xlsx", skip = 2)
gd2013 <- read_excel("annual_gas_distribution_2013.xlsx", skip = 2)
gd2012 <- read_excel("annual_gas_distribution_2012.xlsx", skip = 2)
gd2011 <- read_excel("annual_gas_distribution_2011.xlsx", skip = 2)
gd2010 <- read_excel("annual_gas_distribution_2010.xlsx", skip = 2)
gd2009 <- read_excel("annual_gas_distribution_2009.xlsx")
gd2008 <- read_excel("annual_gas_distribution_2008.xlsx")
gd2007 <- read_excel("annual_gas_distribution_2007.xlsx")
gd2006 <- read_excel("annual_gas_distribution_2006.xlsx")
gd2005 <- read_excel("annual_gas_distribution_2005.xlsx")
gd2004 <- read_excel("annual_gas_distribution_2004.xlsx")

gd2004 <- gd2004[,1:224]
gd2005 <- gd2005[,1:224]
gd2006 <- gd2006[,1:224]
gd2007 <- gd2007[,1:224]
gd2008 <- gd2008[,1:224]
gd2009 <- gd2009[,1:224]

bind0409 <- bind_rows(gd2004,gd2005,gd2006,gd2007,gd2008,gd2009)
bind10 <- bind_rows(gd2010, gd2011, gd2012, gd2013, gd2014, gd2015, gd2016, gd2017)

yrmiles10 <- bind10 %>% group_by(REPORT_YEAR) %>% 
  summarise(pemiles=sum(MMILES_PE_TOTAL, na.rm = T),
            steelmiles=sum(MMILES_STEEL_TOTAL, na.rm=T)) %>% arrange(REPORT_YEAR)

yrmiles0409 <- bind0409 %>% group_by(YR) %>% 
  summarise(pemiles=sum(PEMT,na.rm = T), steelmiles=sum(STMT, na.rm=T)) %>% arrange(YR)

names(yrmiles10)[1] <- "year"
names(yrmiles0409)[1] <- "year"
yrbind <- bind_rows(yrmiles10, yrmiles0409)
yrmmelt <- melt(yrbind,id.vars = 1)
names(byyear)[2] <- "material"
yrmmelt$material <- toupper(yrmmelt$material)

#marry up the two data sets
milejoin <- byyear %>% left_join(yrmmelt,by=c("year","material") )
milejoin$value[30] <- milejoin$value[28]
milejoin$value[29] <- milejoin$value[27]
milejoin <- milejoin[,-7]
names(milejoin)[7] <- "miles"

milejoin <- milejoin %>% mutate(incident_mile=n/miles)

ggplot(milejoin, aes(year,incident_mile*1000))+geom_bar(stat = "identity", aes(fill=material), alpha=0.9, position = "dodge")+theme_bw(16,"serif")+scale_x_continuous(breaks = seq(2004,2018,by=2))+scale_fill_brewer(palette = "Set1")+labs(title = "Incidents per 1,000 Miles of Main by Material", x="Year", y="Incidents/1,000 Miles of Main", caption="PHMSA Distribution incident data (2004-2018) \n excluding \"Fire First\" incidents")+theme(plot.margin = margin(0.6,0.6,0.6,0.6,"cm"))
