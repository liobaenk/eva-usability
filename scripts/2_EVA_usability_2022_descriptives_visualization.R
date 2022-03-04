
# EVA Usability Study - Descriptives  --------


# Date:                   February 16, 2022
# PI:                     Dr. Anne Weigand & Prof. Dr. Isabel Dziobek
# Author:                 Lioba Enk - contact: lioba.enk@maxplanckschools.de




# Input: preprocessed bigdata set
# Output: descriptive tables + two graphics (A. distributions of usability outcome measures, B. distribution of prepost FRT study 2)




rm(list=ls()) 

library(dplyr)
library(tidyr)
library(table1)
library(ggplot2)
library(ggpubr)
library(gridExtra)






# Settings ------------------------------------------------------------------




main_path= 'C:/Users/Lioba/Documents/Research/data/eva-usability/data/'
setwd(main_path)


ifelse(!dir.exists("output"), dir.create("output"), "Folder exists already")
out_path='output/'




cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #for graphics



# Read --------------------------------------------------------------------




bigdata = read.csv('C:/Users/Lioba/Documents/Research/data/eva-usability/data/EVA_usability_preprocessed.csv')


bigdata$group <- factor(bigdata$group, levels = c("NT", "ASC"))
colnames(bigdata)[1] <- "subject"




# Participant characteristics (study 1 AND T0 of study 2) ---------------------------------------------------------------------



# exclude t2 measurement results
data <- bigdata[!(bigdata$timepoint == 2),]

label(data$age) <- "Age, in yrs"
label(data$education_yrs) <- "Education, in years"
label(data$AQ_total) <- "AQ-k"
label(data$TAS_sum) <- "TAS-20"
label(data$SPF_E_sum) <- "SPF empathy"
label(data$SPF_C_sum) <- "SPF competence"
label(data$SPF_PT_sum) <- "SPF perspective taking"
label(data$CEEQ_msp_sum) <- "CEEQ msp"
label(data$FRT_rt_mean) <- "FRT - overall RT"
label(data$FRT_rt_hit_mean) <- "FRT - hit RT"
label(data$FRT_accuracy) <- "FRT - accuracy (%correct)"

table_demog<-table1(~age + education_yrs + AQ_total + TAS_sum + SPF_E_sum + SPF_C_sum + SPF_PT_sum + CEEQ_msp_sum + FRT_rt_mean +
         FRT_rt_hit_mean + FRT_accuracy | group, data=data)
write.csv(table_demog, file=paste(out_path,'descriptives_t0_demog_both_studies.csv'), row.names = FALSE)


rm(data)



# Usability (Supplementary 3): SUS & meCUE (study 1 # T1 of study 2) & NASA-TLX (study 1 only) ---------------------------------------------------------------------



data <- bigdata[!(bigdata$study == "long" & bigdata$timepoint == 1),]

label(data$meCUE_usefulness) <- "meCUE usefulness"
label(data$meCUE_usability) <- "meCUE usability"
label(data$meCUE_aesthetics) <- "meCUE aesthetics"
label(data$meCUE_relationship) <- "meCUE relationship"
label(data$meCUE_posemo) <- "meCUE positive emotions"
label(data$meCUE_negemo) <- "meCUE negative emotions"
label(data$meCUE_global) <- "meCUE total judgement"
label(data$SUS_total) <- "SUS total"
table_mecue_sus<-table1(~ meCUE_usefulness + meCUE_usability + meCUE_aesthetics + meCUE_relationship + meCUE_posemo +
                meCUE_negemo + meCUE_global + SUS_total | group, data=data)
write.csv(table_mecue_sus, file=paste(out_path,'descriptives_t1_meCUE_SUS_both_studies.csv'), row.names = FALSE)



data_nasa <- bigdata[bigdata$study == "lab",]

label(data_nasa$NASA_physical_demand) <- "Nasa physical demand"
label(data_nasa$NASA_mental_demand) <- "Nasa mental demand"
label(data_nasa$NASA_temporal_demand) <- "Nasa temporal demand"
label(data_nasa$NASA_performance) <- "Nasa performance"
label(data_nasa$NASA_effort) <- "Nasa effort"
label(data_nasa$NASA_frustration) <- "Nasa frustration"
label(data_nasa$NASA_total) <- "Nasa weighted mean (total)"
table_nasa<-table1(~ NASA_physical_demand + NASA_mental_demand + NASA_temporal_demand + NASA_performance +
         NASA_effort + NASA_frustration + NASA_total | group, data=data_nasa)
write.csv(table_nasa, file=paste(out_path,'descriptives_t1_NASA-TLX_study1.csv'), row.names = FALSE)



rm(data, data_nasa)




# Graphic A - Distributions of usability outcome measures per group ---------------------------



data <- bigdata[!(bigdata$study_1_lab_2_long == 2 & bigdata$timepoint == 1),]

data2<-data %>%
  gather(key = "mecue_subscales", value = "mecue_value", meCUE_usability, meCUE_usefulness, 
         meCUE_aesthetics, meCUE_relationship, meCUE_posemo, meCUE_negemo) #for analyses of mecue subscales with range 1-7
data2$mecue_subscales <- factor(data2$mecue_subscales, levels = c("meCUE_usability", "meCUE_usefulness", "meCUE_aesthetics",
                                                                        "meCUE_relationship", "meCUE_posemo", "meCUE_negemo"),
                                   labels = c("Usability", "Usefulness", "Aesthetics", "Relationship", "Positive emotions", "Negative emotions"))

data_nasa <- data %>%
  gather(key = "nasa_subscales", value = "nasa_value", NASA_mental_demand, NASA_physical_demand, NASA_temporal_demand,
         NASA_performance, NASA_effort, NASA_frustration, NASA_total)
data_nasa <- data_nasa[data_nasa$study_1_lab_2_long == 1,] #nasa-tlx was only part of study 1
data_nasa$nasa_subscales <- factor(data_nasa$nasa_subscales, levels = c("NASA_mental_demand", "NASA_physical_demand", "NASA_temporal_demand",
                                                                        "NASA_performance", "NASA_effort", "NASA_frustration", "NASA_total"),
                  labels = c("Mental demand", "Physical demand", "Temporal demand", "Performance", "Effort", "Frustration", "Total (weighted) mean"))




# calculate medians for plots per subscale and group (ASC, NT)

med_mecue_per_group<-data2 %>%
  group_by(group, mecue_subscales) %>%
  summarize(median=median(mecue_value))

med_nasa_per_group<-data_nasa %>%
  group_by(group, nasa_subscales) %>%
  summarize(median=median(nasa_value))

med_sus_per_group<-data %>%
  group_by(group) %>%
  summarize(median=median(SUS_total))

med_mecue_global_per_group<-data %>%
  group_by(group) %>%
  summarize(median=median(meCUE_global))



plot1<-ggplot(data2, aes(x=mecue_value, fill=group)) + 
  geom_density(alpha=0.3) +
  geom_vline(data=med_mecue_per_group, aes(xintercept=median,colour=group), linetype="dashed", size=0.8) +
  scale_x_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7)) +
  labs(title = "meCUE", y = "Density", fill = "Group", colour = "Median") +
  facet_wrap(. ~ mecue_subscales, ncol=3) +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size = 16))


plot2<-ggplot(data, aes(x=meCUE_global, fill=group)) + 
  geom_density(alpha=0.3) +
  geom_vline(data=med_mecue_global_per_group, aes(xintercept=median,colour=group), linetype="dashed", size=0.8) +
  labs(title = "Total judgement", y = "Density", fill = "Group", colour = "Median") +
  scale_x_continuous(limits = c(-5,5), breaks=c(-5, -2.5, 0, 2.5, 5)) +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size = 16))
  

plot3<-ggplot(data, aes(x=SUS_total, fill=group)) + 
  geom_density(alpha=0.3) +
  scale_x_continuous(limits = c(0,100), breaks=c(0, 25, 50, 75, 100)) +
  geom_vline(data=med_sus_per_group, aes(xintercept=median,colour=group), linetype="dashed", size=0.8) +
  labs(title = "SUS", y = "Density", fill = "Group", colour = "Median") +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size = 16))


plot4<-ggplot(data_nasa, aes(x=nasa_value, fill=group)) + 
  geom_density(alpha=0.3) +
  geom_vline(data=med_nasa_per_group, aes(xintercept=median,colour=group), linetype="dashed", size=0.8) +
  scale_x_continuous(limits = c(0,100), breaks=c(0, 25, 50, 75, 100)) +
  labs(title = "NASA-TLX", y = "Density", fill = "Group", colour = "Median") +
  facet_wrap(. ~ nasa_subscales, ncol=3) +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        title = element_text(size = 18),
        axis.text.x = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(size = 16))



svg(file=paste(out_path,'usability_distributions_per_group.svg'), width=10, height=14)

grid.arrange(plot1, plot3, plot4, ncol = 3, nrow=6,
             layout_matrix = rbind(c(1,1,1), 
                                   c(1,1,1),
                                   c(3,3,NA),
                                   c(4,4,4),
                                   c(4,4,4),
                                   c(4,4,4)))
dev.off()


svg(file=paste(out_path,'usability_distributions_per_group_with_mecue_global_2.svg'), width=10, height=14)

grid.arrange(plot1, plot2, plot3, plot4,  ncol = 5, nrow=7, widths=c(3,2.125,0.5,0.325,3),
             heights=c(1,1,1,0.4,1,1,1),
             layout_matrix = rbind(c(1,1,1,1,1), 
                                   c(1,1,1,1,1),
                                   c(3,3,NA,2,2),
                                   c(3,3,NA,NA,NA),
                                   c(4,4,4,4,4),
                                   c(4,4,4,4,4),
                                   c(4,4,4,4,4)))
dev.off()




rm(data, data2, data_nasa)




# Pre-post changes in FRT performance and SPF (study 2) ---------------------------------------------------------------------




data <- bigdata[bigdata$study == "long",]


# Pre-post on merged data (N=13)
table1(~ FRT_rt_mean + FRT_rt_hit_mean + FRT_accuracy + SPF_E_sum + SPF_C_sum + SPF_PT_sum | timepoint, data=data)

# Pte-post between both groups
table_FRT_prepost_NT<-table1(~ FRT_rt_mean + FRT_rt_hit_mean + FRT_accuracy + SPF_E_sum + SPF_C_sum + SPF_PT_sum | timepoint + group, data=data)
write.csv(table_FRT_prepost_NT, file=paste(out_path,'descriptives_prepost_FRT_between_groups.csv'), row.names = FALSE)


rm(data)


# Graphic B - Distributions of FRT outcome measures for NTs ---------------










# Supplementary 1: Differences between NT groups on baseline measures --------------------------





data <- bigdata[bigdata$group == "NT" & bigdata$timepoint == 1,]


label(data$age) <- "Age, in yrs"
label(data$education_yrs) <- "Education, in years"
label(data$AQ_total) <- "AQ-k"
label(data$TAS_sum) <- "TAS-20"
label(data$SPF_E_sum) <- "SPF empathy"
label(data$SPF_C_sum) <- "SPF competence"
label(data$SPF_PT_sum) <- "SPF perspective taking"
label(data$CEEQ_msp_sum) <- "CEEQ msp"
label(data$FRT_rt_mean) <- "FRT - overall RT"
label(data$FRT_rt_hit_mean) <- "FRT - hit RT"
label(data$FRT_accuracy) <- "FRT - accuracy (%correct)"

table_NT_between_studies<-table1(~age + education_yrs + AQ_total + TAS_sum + SPF_E_sum + SPF_C_sum + SPF_PT_sum + CEEQ_msp_sum + FRT_rt_mean +
                FRT_rt_hit_mean + FRT_accuracy | study, data=data)
write.csv(table_NT_between_studies, file=paste(out_path,'descriptives_t0_NT_demog_between_studies.csv'), row.names = FALSE)





data <- bigdata[bigdata$group == "NT",]
data <- data[!(data$study == "long" & data$timepoint == 1),]


label(data$meCUE_usefulness) <- "meCUE usefulness"
label(data$meCUE_usability) <- "meCUE usability"
label(data$meCUE_aesthetics) <- "meCUE aesthetics"
label(data$meCUE_relationship) <- "meCUE relationship"
label(data$meCUE_posemo) <- "meCUE positive emotions"
label(data$meCUE_negemo) <- "meCUE negative emotions"
label(data$meCUE_global) <- "meCUE total judgement"
label(data$SUS_total) <- "SUS total"
table_NT_mecue_sus_between_studies<-table1(~ meCUE_usefulness + meCUE_usability + meCUE_aesthetics + meCUE_relationship + meCUE_posemo +
                meCUE_negemo + meCUE_global + SUS_total | study, data=data)
write.csv(table_NT_mecue_sus_between_studies, file=paste(out_path,'descriptives_t1_NT_meCUE_SUS_between_studies.csv'), row.names = FALSE)



rm(data)




# Supplementary 2: Differences between ASC groups on baseline measures --------------------------



## Baseline measures

data <- bigdata[!(bigdata$timepoint == 2 | bigdata$group == "NT"),]
highlight_df <- data %>% filter(study == "long")


# age
bxp_age<-ggplot(data,aes(x=factor(0),y=age))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="Age") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=age), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(20,50), breaks=c(10,20,30,40,50))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())

# education
bxp_edu<-ggplot(data,aes(x=factor(0),y=education_yrs))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="Education in years") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=education_yrs), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(4,12), breaks=c(4,6,8,10,12))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())

# AQk
bxp_aqk<-ggplot(data,aes(x=factor(0),y=AQ_total))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="AQk") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=AQ_total), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(15,35), breaks=c(15,20,25,30,33))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())

# TAS-20
bxp_tas<-ggplot(data,aes(x=factor(0),y=TAS_sum))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="TAS-20") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=TAS_sum), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(40,80), breaks=c(40,50,60,70,80))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())

# CEEQ msp
bxp_ceeq<-ggplot(data,aes(x=factor(0),y=CEEQ_msp_sum))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="CEEQ msp") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=CEEQ_msp_sum), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(5,40), breaks=c(10,20,30,40))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())



# SPF
data_spf <- data %>% gather(key = "SPF", value = "value", SPF_E_sum, SPF_C_sum, SPF_PT_sum)
data_spf$SPF <- factor(data_spf$SPF, levels = c("SPF_E_sum", "SPF_C_sum", "SPF_PT_sum"),
                                labels = c("Empathy", "Competence", "Perspective taking"))
highlight_spf <- data_spf %>% filter(study == "long")

bxp_spf<-ggplot(data_spf,aes(x=SPF,y=value))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="SPF") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_spf, 
             aes(x=SPF,y=value), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(10,30), breaks=c(10,20,30))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_text(size=14))
rm(data_spf, highlight_spf)


# FRT RTs
data_frt <- data %>% gather(key = "FRT", value = "RT", FRT_rt_mean, FRT_rt_hit_mean)
data_frt$FRT <- factor(data_frt$FRT, levels = c("FRT_rt_mean", "FRT_rt_hit_mean"), labels = c("Overall RT", "Hit RT"))
data_frt$RT_sec<-data_frt$RT/1000 #msec in sec
highlight_frt <- data_frt %>% filter(study == "long")

bxp_frt_rt<-ggplot(data_frt,aes(x=FRT,y=RT_sec))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="FRT explicit", y = "Seconds") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_frt, 
             aes(x=FRT,y=RT_sec), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(5,40), breaks=c(10, 20, 30, 40))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size=16),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_text(size=14))
rm(data_frt, highlight_frt)

# FRT accuracy
bxp_frt_acc<-ggplot(data,aes(x=factor(0),y=FRT_accuracy))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="", y = "Percentage") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=FRT_accuracy), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(20,100), breaks=c(20,40,60,80,100))+
  scale_x_discrete(labels="Accuracy") +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size=16),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_text(size=14))


rm(data)



## Usability outcome measures


data <- bigdata[!(bigdata$timepoint == 1 & bigdata$study == "long"),]
data <- data[data$group == "ASC",]
highlight_df <- data %>% filter(study == "long")




# meCUE

data_mecue <- data %>% gather(key = "meCUE", value = "value", meCUE_usefulness, meCUE_usability, meCUE_aesthetics, meCUE_relationship, meCUE_posemo, meCUE_negemo)
data_mecue$meCUE <- factor(data_mecue$meCUE, levels = c("meCUE_usability", "meCUE_usefulness", "meCUE_aesthetics",
                                                                  "meCUE_relationship", "meCUE_posemo", "meCUE_negemo"),
                                labels = c("Usability", "Usefulness", "Aesthetics", "Relationship", "Pos. emotions", "Neg. emotions"))
highlight_mecue <- data_mecue %>% filter(study == "long")

bxp_mecue<-ggplot(data_mecue,aes(x=meCUE,y=value))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="meCUE") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_mecue, 
             aes(x=meCUE,y=value), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_text(size=14))

rm(data_mecue, highlight_mecue)


bxp_mecue_total<-ggplot(data,aes(x=factor(0),y=meCUE_global))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=meCUE_global), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(-5,5), breaks=c(-5,-2.5,0,2.5,5))+
  scale_x_discrete(labels = "Total judgement") +
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_text(size=14))


# SUS
bxp_sus<-ggplot(data,aes(x=factor(0),y=SUS_total))+
  geom_boxplot(outlier.shape=8, outlier.size=4)+ labs(title="SUS") +
  geom_point(alpha=2, colour="purple", size=2)+
  geom_point(data=highlight_df, 
             aes(x=factor(0),y=SUS_total), 
             color='red',
             size=2) +
  scale_y_continuous(limits = c(10,100), breaks=c(20,40,60,80,100))+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        title = element_text(size = 18),
        axis.text.x = element_blank())


rm(data, highlight_spf, highlight_df)





svg(file=paste(out_path,'bxp_ASC_baseline_and_usability_measures.svg'), width=13, height=14)


grid.arrange(bxp_age, bxp_edu, bxp_aqk, bxp_tas, bxp_spf, bxp_ceeq, bxp_frt_rt, bxp_frt_acc,
             bxp_mecue, bxp_mecue_total, bxp_sus,
             ncol = 6, nrow=3,
             widths=c(0.2125,0.1,2.9,3,3,3),
             layout_matrix = rbind(c(NA,1,1,2,3,4), 
                                   c(NA,5,5,5,5,6),
                                   c(7,7,7,7,8,NA),
                                   c(NA,NA,9,9,9,10),
                                   c(NA,11,11,NA,NA,NA)))
             #widths=c(3,2.125,0.5,0.325,3), heights=c(1,1,1,0.4,1,1,1),
dev.off()






# Correlation - Plot significant ones -------------------------------------

# create dfs



data_nt_lab <- bigdata[bigdata$study == "lab" & bigdata$group == "NT",] #N=21
data_all_lab <- bigdata[bigdata$study == "lab",] #N=31

data_all_long_t0 <- bigdata[bigdata$study == "long" & bigdata$timepoint == 1,]
data_all_long_t1 <- bigdata[bigdata$study == "long" & bigdata$timepoint == 2,]
data_merged_long<-cbind(data_all_long_t0[,1:25], data_all_long_t1[,26:33], data_all_long_t0[,34:46])
data_all_n44<-rbind(data_all_lab, data_merged_long)

data_all_NT <- data_all_n44[data_all_n44$group == "NT",] #N=31




rm(data_all_long_t0, data_all_long_t1, data_merged_long)




# plot lab_nt+asc



lab_nt_asc1<-ggscatter(data_all_lab, x = "SPF_C_sum", y = "NASA_frustration", fill="group", shape = 21, size = 2.5, 
                       palette = c("green", "blue"),
                       add = "reg.line", conf.int = TRUE,    
                       add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                       xlab = "SPF competence", ylab = "NASA frustration", title = "Lab NT+ASC") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 17, label.y = 75) +
  annotate("text", x = 21, y = 75.05, label = "p_fdr = 0.0098**")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt_asc2<-ggscatter(data_all_lab, x = "TAS_sum", y = "NASA_mental_demand", fill="group", shape = 21, size = 2.5, 
                       palette = c("green", "blue"),
                       add = "reg.line", conf.int = TRUE,    
                       add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                       xlab = "TAS-20", ylab = "NASA mental demand", title = "Lab NT+ASC") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 28, label.y = 40) +
  annotate("text", x = 42, y = 40.05, label = "p_fdr = 0.1268")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))


svg(file=paste(out_path,'spearman_corr_lab_nt_asc.svg'), width=18, height=7)
grid.arrange(lab_nt_asc1, lab_nt_asc2, ncol = 2, nrow=1)
dev.off()




# plot lab_nt (based on script 3 analysis)



lab_nt1<-ggscatter(data_nt_lab, x = "SPF_C_sum", y = "NASA_frustration",
          add = "reg.line", conf.int = TRUE,    
          add.params = list(color = "red", fill = "lightgrey"),
          ggtheme = theme_minimal(),
          xlab = "SPF competence", ylab = "NASA frustration", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 20, label.y = 75) +
  annotate("text", x = 23, y = 75.5, label = "p_fdr = 0.0356*")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt2<-ggscatter(data_nt_lab, x = "TAS_sum", y = "NASA_mental_demand",
                   add = "reg.line", conf.int = TRUE,    
                   add.params = list(color = "red", fill = "lightgrey"),
                   ggtheme = theme_minimal(),
                   xlab = "TAS-20", ylab = "NASA mental demand", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 30, label.y = 40) +
  annotate("text", x = 40, y = 40.3, label = "p_fdr = 0.0495*")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt3<-ggscatter(data_nt_lab, x = "AQ_total", y = "meCUE_usefulness",
                   add = "reg.line", conf.int = TRUE,    
                   add.params = list(color = "red", fill = "lightgrey"),
                   ggtheme = theme_minimal(),
                   xlab = "AQk", ylab = "meCUE usefulness", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 3, label.y = 2) +
  annotate("text", x = 8, y = 2.03, label = "p_fdr = 0.0136*")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt4<-ggscatter(data_nt_lab, x = "AQ_total", y = "SUS_total",
                   add = "reg.line", conf.int = TRUE,    
                   add.params = list(color = "red", fill = "lightgrey"),
                   ggtheme = theme_minimal(),
                   xlab = "AQk", ylab = "SUS total", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 5, label.y = 60) +
  annotate("text", x = 10, y = 60.2, label = "p_fdr = 0.1645")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt5<-ggscatter(data_nt_lab, x = "TAS_sum", y = "meCUE_usefulness",
          add = "reg.line", conf.int = TRUE,    
          add.params = list(color = "red", fill = "lightgrey"),
          ggtheme = theme_minimal(),
          xlab = "TAS-20", ylab = "meCUE usefulness", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 30, label.y = 2.5) +
  annotate("text", x = 40, y = 2.53, label = "p_fdr = 0.0673")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_nt6<-ggscatter(data_nt_lab, x = "SPF_PT_sum", y = "meCUE_usefulness",
          add = "reg.line", conf.int = TRUE,    
          add.params = list(color = "red", fill = "lightgrey"),
          ggtheme = theme_minimal(),
          xlab = "TAS-20", ylab = "meCUE usefulness", title = "Lab NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 25, label.y = 2.5) +
  annotate("text", x = 29, y = 2.53, label = "p_fdr = 0.0997")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))





svg(file=paste(out_path,'spearman_corr_lab_nt.svg'), width=25, height=12)
grid.arrange(lab_nt1, lab_nt2, lab_nt3, 
             lab_nt4, lab_nt5, lab_nt6, ncol = 3, nrow=2)
dev.off()







# plot nt lab+long





lab_long_nt1<-ggscatter(data_all_NT, x = "AQ_total", y = "meCUE_usefulness", fill="study", shape = 21, size = 2.5, 
                       palette = c("purple", "orange"),
                       add = "reg.line", conf.int = TRUE,    
                       add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                       xlab = "AQk", ylab = "meCUE usefulness", title = "Lab+Long NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 2, label.y = 2) +
  annotate("text", x = 8, y = 2.03, label = "p_fdr = 0.0033**")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_long_nt2<-ggscatter(data_all_NT, x = "SPF_PT_sum", y = "meCUE_usefulness", fill="study", shape = 21, size = 2.5, 
                        palette = c("purple", "orange"),
                        add = "reg.line", conf.int = TRUE,    
                        add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                        xlab = "SPT perspective taking", ylab = "meCUE usefulness", title = "Lab+Long NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 25, label.y = 2.5) +
  annotate("text", x = 30, y = 2.53, label = "p_fdr = 0.0460*")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_long_nt3<-ggscatter(data_all_NT, x = "AQ_total", y = "meCUE_negemo", fill="study", shape = 21, size = 2.5, 
                        palette = c("purple", "orange"),
                        add = "reg.line", conf.int = TRUE,    
                        add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                        xlab = "AQk", ylab = "meCUE negative emotions", title = "Lab+Long NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 12, label.y = 1) +
  annotate("text", x = 17, y = 1.03, label = "p_fdr = 0.0258*")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_long_nt4<-ggscatter(data_all_NT, x = "AQ_total", y = "SUS_total", fill="study", shape = 21, size = 2.5, 
                        palette = c("purple", "orange"),
                        add = "reg.line", conf.int = TRUE,    
                        add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                        xlab = "AQk", ylab = "SUS total", title = "Lab+Long NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 3, label.y = 60) +
  annotate("text", x = 8.5, y = 60.3, label = "p_fdr = 0.1025")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

lab_long_nt5<-ggscatter(data_all_NT, x = "TAS_sum", y = "meCUE_usefulness", fill="study", shape = 21, size = 2.5, 
                        palette = c("purple", "orange"),
                        add = "reg.line", conf.int = TRUE,    
                        add.params = list(color = "red", fill = "lightgrey"), ggtheme = theme_minimal(),
                        xlab = "TAS-20", ylab = "meCUE usefulness", title = "Lab+Long NT") +
  stat_cor(method = "spearman", alternative = "two.sided",r.accuracy = 0.01, p.accuracy = 0.01,
           cor.coef.name = "rho", label.x = 28, label.y = 2) +
  annotate("text", x = 40, y = 2.03, label = "p_fdr = 0.2357")+
  theme(
    plot.title = element_text(color="black", size=20, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))




svg(file=paste(out_path,'spearman_corr_lab_long_nt.svg'), width=25, height=14)
grid.arrange(lab_long_nt1, lab_long_nt2, lab_long_nt3, lab_long_nt4, lab_long_nt5, ncol = 3, nrow=2)
dev.off()








###########################################################################################



