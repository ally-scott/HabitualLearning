
cat("\f") #clear console


library(readxl)
library(tidyverse)
library(emmeans)
library(lmerTest)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig5")

rm(list = ls())
GADVPFPauc8 <- read.csv("GADVPFP_dffstage8plottab.csv")
GADVPFPaucstage8 <- GADVPFPauc8 %>%
  mutate(rat = `subject`,
         rat = parse_number(`rat`),
         day= `stageday`,
         cue = `cue`,
         auc=`aucvalues`,
         pump=`pumpID`,
         sex = `sex`,
         peprob=`peRat10sec`) %>% 
  select(auc, rat, stage, day, cue,sex,pump,peprob)



#GADVPFPaucall<-rbind(GADVPFPauc,GADVPFPControlauc)
#GADVPFPaucstage1to4= filter (GADVPFPaucall,stage<5)
#GADVPFPaucstage1to4 <- GADVPFPaucstage1to4 %>%
# select (-cue)
#GADVPFPaucstage5= filter (GADVPFPaucall,stage==5)

# run linear mixed effect models
###
aucstage8_model<- lmer(auc ~sex*as.factor(pump)*cue+ (1 | rat), data = GADVPFPaucstage8)
aucstage8_model_nosex<- lmer(auc ~ as.factor(pump) + (1 | rat), data = subset(GADVPFPaucstage8,cue=='DS'))
#anova(aucstage8_model,aucstage8_model_nosex)
aucstage8_lme<-summary(aucstage8_model_nosex)
aucstage8_lmeanova<- anova(aucstage8_model_nosex)
capture.output(aucstage8_lme, file = "aucstage8dff_lme_results.doc")
capture.output(aucstage8_lmeanova, file = "aucstage8dff_lmeanova_results.doc")


## follow up comparisons 
em <- emmeans(aucstage8_model_nosex,specs= ~ as.factor(pump))

DS_suc <- c(1,0)
DS_probe <- c(0,1)


null1 <- c(0,0)




contrast(em, method = list("DS_suc - DS_probe" = DS_suc - DS_probe))


DSsucrose=subset(GADVPFPaucstage8,cue=='DS'& pump==1)
DSprobe=subset(GADVPFPaucstage8,cue=='DS'& pump==2)
res <- t.test(DSsucrose$auc, DSprobe$auc, paired = TRUE)


GADVPFPaucstage8mean <- subset(GADVPFPaucstage8,cue=='DS' & day<3) %>%
  group_by(pump,rat)%>%
  summarize(mean_auc= mean(auc))


stage8aucmeans <- subset(GADVPFPaucstage8,cue=='DS' & day<3) %>%
  group_by(pump)%>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),auc)


stage8peprobmeans <- subset(GADVPFPaucstage8,cue=='DS' & day<3) %>%
  group_by(day)%>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),peprob)

stage8peprobday1<- subset(GADVPFPaucstage8,cue=='DS' & day==1 &  rat!=12)
stage8peprobday2<- subset(GADVPFPaucstage8,cue=='DS' & day==2) 
stage8peprobttest <- t.test(stage8peprobday1$peprob, stage8peprobday2$peprob, paired = TRUE)  

library(BayesFactor)

bf = ttestBF(x = DSsucrose$auc,DSprobe$auc, paired=TRUE)
bf

library(ggplot2)
# auc with line and area SEM


ggplot(subset(GADVPFPaucstage8,cue=='DS' & day<3), aes(x=pump, y=auc,color=as.factor(pump))) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "bar")+
  geom_point(aes(size = 3))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  facet_grid(cols = vars(day)) + 
  labs(title="AUC Stage 8",x ="pump", y = "AUC (0-5 seconds z-scored data) ") +
  #scale_x_continuous(limits = c(1,2), breaks = seq(from = 1, to = 2, by = 1))+
  #scale_y_continuous(limits = c(0,10),breaks = seq(from = 0, to = 10, by = 2))+
  ggsave("AUC stages 8dff.pdf")



ggplot(subset(GADVPFPaucstage8mean ), aes(x=as.factor(pump), y=mean_auc )) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "bar")+
  #stat_summary(fun = "mean", geom = "bar")+
  geom_point(aes(size = 3))+
  scale_y_continuous(limits = c(0,12),breaks = seq(from = 0, to = 12, by = 2))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="AUC Stage 8",x ="Pump", y = "AUC (0-5 seconds z-scored data) ")  
ggsave("AUC stages 8_withpointsdff.pdf")

