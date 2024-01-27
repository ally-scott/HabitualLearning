
cat("\f") #clear console


library(readxl)
library(tidyverse)
library(emmeans)
library(lmerTest)
library(rstatix)

setwd("G:/Shared drives/Richard Lab/Papers and Abstracts/Papers in Progress/Alexandra Scott/GADVPFP/Figs/fig6")

rm(list = ls())
GADVPFPauc9 <- read.csv("GADVPFP_dffstage9plottab.csv")
GADVPFPaucstage9 <- GADVPFPauc9 %>%
  mutate(rat = `subject`,
         rat = parse_number(`rat`),
         day= `stageday`,
         cue = `cue`,
         auc=`aucvalues`,
         sex = `sex`,
         peprob=`peRat10sec`) %>% 
  select(auc, rat, stage, day, cue,sex,peprob)



#GADVPFPaucall<-rbind(GADVPFPauc,GADVPFPControlauc)
#GADVPFPaucstage1to4= filter (GADVPFPaucall,stage<5)
#GADVPFPaucstage1to4 <- GADVPFPaucstage1to4 %>%
# select (-cue)
#GADVPFPaucstage5= filter (GADVPFPaucall,stage==5)

# run linear mixed effect models
###
aucstage9_model<- lmer(auc ~sex*cue*as.factor(day)+ (1 | rat), data = GADVPFPaucstage9)
aucstage9_model_nosex<- lmer(auc ~ as.factor(day) + (1 | rat), data = subset(GADVPFPaucstage9,cue=='DS'))
anova(aucstage9_model,aucstage9_model_nosex)
aucstage9_lme<-summary(aucstage9_model_nosex)
aucstage9_lmeanova<- anova(aucstage9_model_nosex)
capture.output(aucstage9_lme, file = "aucstage9dff_lme_results.doc")
capture.output(aucstage9_lmeanova, file = "aucstage9dff_lmeanova_results.doc")

aucstage9_model_nosexNS<- lmer(auc ~ as.factor(day) + (1 | rat), data = subset(GADVPFPaucstage9,cue=='NS'))
aucstage9_lme_NS<-summary(aucstage9_model_nosexNS)
aucstage9_lmeanova_NS<- anova(aucstage9_model_nosexNS)
capture.output(aucstage9_lme_NS, file = "aucstage9dff_lme_results_NS.doc")
capture.output(aucstage9_lmeanova_NS, file = "aucstage9dff_lmeanova_results_NS.doc")



## follow up comparisons 
em <- emmeans(aucstage9_model_nosex,specs= ~ as.factor(day))

DS_extday1 <- c(1,0,0)
DS_extday2<- c(0,1,0)
DS_extday3<- c(0,0,1)

null1 <- c(0,0,0)




contrast(em, method = list("DS_1 - DS_2" = DS_extday1 - DS_extday2,
                           "DS_1 - DS_3" = DS_extday1 - DS_extday3,
                           "DS_2 - DS_3" = DS_extday2 - DS_extday3))





stage9aucmeans <- subset(GADVPFPaucstage9,cue=='DS' ) %>%
  group_by(day)%>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),auc)


stage9prob<- na.omit(GADVPFPaucstage9)
stage9peprobmeans <- subset(stage9prob,cue=='DS' ) %>%
  group_by(day)%>%
  summarise_each(funs(mean,se=sd(.)/sqrt(n())),peprob)

res.aov <- subset(GADVPFPaucstage9,cue=='DS' ) %>% anova_test(peprob ~ as.factor(day))
res.aov

res.aov <- subset(GADVPFPaucstage9,cue=='NS' ) %>% anova_test(peprob ~ as.factor(day))
res.aov

auc.aov <- subset(GADVPFPaucstage9,cue=='DS' ) %>% anova_test(auc ~ as.factor(day))
auc.aov

pwc <- subset(GADVPFPaucstage9,cue=='DS' ) %>% tukey_hsd(peprob ~ as.factor(day))
pwc

stage9peprobday1<- subset(GADVPFPaucstage8,cue=='DS' & day==1)
stage9peprobday2<- subset(GADVPFPaucstage8,cue=='DS' & day==2) 
stage9peprobday2<- subset(GADVPFPaucstage8,cue=='DS' & day==3) 
stage8peprobttest <- t.test(stage8peprobday1$peprob, stage8peprobday2$peprob, paired = TRUE)  

library(ggplot2)
# auc with line and area SEM


ggplot(GADVPFPaucstage9, aes(x=day, y=auc,color=cue)) + 
  stat_summary(fun.data = mean_se,geom = "ribbon")+      
  stat_summary(fun = "mean", geom = "line")+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="AUC Stage 9",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ") +
  scale_x_continuous(limits = c(1,3), breaks = seq(from = 1, to = 3, by = 1))+
  #scale_y_continuous(limits = c(0,10),breaks = seq(from = 0, to = 10, by = 2))+
  ggsave("AUC stages 9dff.pdf")



ggplot(GADVPFPaucstage9, aes(x=day, y=auc, color=cue )) + 
  stat_summary(fun.data = mean_se,geom = "errorbar")+      
  stat_summary(fun = "mean", geom = "bar")+
  facet_grid(vars(rows=cue))+
  #stat_summary(fun = "mean", geom = "bar")+
  geom_point(aes(group=rat,size = 3))+
  #(aes(group=rat))+
  #stat_summary(fun.data = mean_se,geom = "area")+
  #facet_grid(cols = vars(sex)) + 
  labs(title="AUC Stage 9",x ="Training Day", y = "AUC (0-5 seconds z-scored data) ")  
ggsave("AUC stages 9_withpointsdff.pdf")

