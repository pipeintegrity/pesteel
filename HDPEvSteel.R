# HDPE vs Steel analysis
#mined PHMSA incident data for comparison
library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)



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
pesteel

byyear <- pesteel %>% group_by(year,MATERIAL_INVOLVED) %>% summarise(fatal=sum(FATAL), injure=sum(INJURE), CoF=sum(CoF))

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
  labs(title = "Incident Count by Sub-Cause and Material Type", subtitle = "PHMSA incident data 2004 - 2019 (Excluding Excavation Damage)",x= "Mapped Sub-Cause", y="Count")


#MCMC Analysis of PE and Steel CoF
library(BEST)
best_pest <- BESTmcmc(log10( pesteel_fil$CoF[which(x = pesteel_fil$MATERIAL_INVOLVED=="STEEL")]),
                      log10(pesteel_fil$CoF[which(x = pesteel_fil$MATERIAL_INVOLVED=="PE")]),
                      numSavedSteps = 5e4)

plotPost(best_pest$mu1 - best_pest$mu2, compVal = 0, xlab = bquote(mu[Steel] - mu[PE]), 
         cex.lab = 1.75, main = "Difference of Mean CoF", 
         col = "skyblue2")
