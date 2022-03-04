
# EVA Usability Study - Analysis  --------


# Date:                   February 27, 2022
# Principal investigator: Prof. Dr. Isabel Dziobek
# Author:                 Lioba Enk - contact: lioba.enk@maxplanckschools.de





rm(list=ls()) 

library(dplyr)
library(tidyr)
library(tidyverse)
library(rstatix)
library(coin) #for effect size calculations





# Settings ------------------------------------------------------------------





ifelse(!dir.exists("output"), dir.create("output"), "Folder exists already")


out_path='output/'






# Read --------------------------------------------------------------------




bigdata = read.csv('data/EVA_usability_preprocessed.csv')
bigdata$group <- factor(bigdata$group, levels = c("NT", "ASC"))
colnames(bigdata)[1] <- "subject"






# Inference stats: participant characteristics (study 1 AND T0 of study 2) ---------------------------------------------------------------------


# exclude t2 measurement results
data <- bigdata[!(bigdata$timepoint == 2),]



# Chi-square test of homogeneity
Tab = table(bigdata$sex_0_m_1_f, bigdata$group)
chisq.test(Tab, correct=F) 




# Non-parametric independent between-group comparisons (Mann-Whitney-Wilcoxon tests)

# Note: to make sure that eff_sizes were matching MWW tests, we computed this function as well:
# data  %>% wilcox_test(FRT_rt_hit_mean~group) %>% add_significance()


# Argument exact=F is added to the function in case of discrete variables 
# (then function cannot compute exact p-values with ties, and so continuity correction is applied)

wilcox.test(age~group, data = data, exact = F)
wilcox.test(education_yrs~group, data = data, exact = F)

wilcox.test(AQ_total~group, data = data, exact = F) #p<0.001
wilcox_effsize(AQ_total~group, data=data)

wilcox.test(TAS_sum~group, data = data, exact = F) #p=0.0098
wilcox_effsize(TAS_sum~group, data=data)

wilcox.test(SPF_E_sum~group, data = data, exact = F) #p=0.00145
wilcox_effsize(SPF_E_sum~group, data=data)

wilcox.test(SPF_C_sum~group, data = data, exact = F)

wilcox.test(SPF_PT_sum~group, data = data, exact = F) #p=0.00252
wilcox_effsize(SPF_PT_sum~group, data=data)

wilcox.test(CEEQ_msp_sum~group, data = data, exact = F) #p<0.001
wilcox_effsize(CEEQ_msp_sum~group, data=data)


# FRT

wilcox.test(FRT_rt_mean~group, data = data) #=0.0028
wilcox_effsize(FRT_rt_mean~group, data=data)

wilcox.test(FRT_rt_hit_mean~group, data = data) #p=0.001275
wilcox_effsize(FRT_rt_hit_mean~group, data=data)

wilcox.test(FRT_accuracy~group, data = data, exact=F)

rm(data)





# Inference stats (Suppl. Material 2): usability outcomes (SUS + meCUE: study 1 and T1 of study 2 | NASA: study 1) ---------------------------------------------------------------------


data <- bigdata[!(bigdata$study == "long" & bigdata$timepoint == 1),]
data_nasa <- bigdata[bigdata$study == "lab",]


# non-parametric independent between-group comparisons (Mann-Whitney-Wilcoxon tests)

wilcox.test(SUS_total~group, data = data, exact = F)
wilcox.test(meCUE_usefulness~group, data = data, exact = F)
wilcox.test(meCUE_usability~group, data = data, exact = F)
wilcox.test(meCUE_aesthetics~group, data = data, exact = F)
wilcox.test(meCUE_relationship~group, data = data, exact = F)
wilcox.test(meCUE_posemo~group, data = data, exact = F)
wilcox.test(meCUE_negemo~group, data = data, exact = F)
wilcox.test(meCUE_global~group, data = data, exact = F)

wilcox.test(NASA_mental_demand~group, data = data_nasa, exact = F)
wilcox.test(NASA_physical_demand~group, data = data_nasa, exact = F)
wilcox.test(NASA_temporal_demand~group, data = data_nasa, exact = F)
wilcox.test(NASA_performance~group, data = data_nasa, exact = F)

wilcox.test(NASA_effort~group, data = data_nasa, exact = F)
#data_nasa  %>% wilcox_test(NASA_effort~group) %>% add_significance()
wilcox_effsize(NASA_effort~group, data=data_nasa)

wilcox.test(NASA_frustration~group, data = data_nasa, exact = F)
wilcox.test(NASA_total~group, data = data_nasa, exact = F)



rm(data, data_nasa)




# Pre-post changes in FRT performance and SPF (study 2) ---------------------------------------------------------------------



data <- bigdata[bigdata$study == "long",]
data_nt <- bigdata[bigdata$study == "long" & bigdata$group == "NT",]



# non-parametric dependent Wilcoxon signed-rank tests fpr FRT + SPF

# Note: Correctness of results was checked using the following function:
#data_nt  %>% wilcox_test(FRT_accuracy~timepoint, paired = TRUE) %>% add_significance()



# for NTs only (N=10)

wilcox.test(FRT_rt_mean~timepoint, data = data_nt, paired=T) #p=0.00586
wilcox_effsize(FRT_rt_mean ~ timepoint, data=data_nt, paired = TRUE)

wilcox.test(FRT_rt_hit_mean~timepoint, data = data_nt, paired=T) #p=0.001953
wilcox_effsize(FRT_rt_hit_mean ~ timepoint, data=data_nt, paired = TRUE)

wilcox.test(FRT_accuracy~timepoint, data = data_nt, paired=T, exact=F) #p=0.017
wilcox_effsize(FRT_accuracy ~ timepoint, data=data_nt, paired = TRUE)


# for NT and ASC merged (N=13)

wilcox.test(FRT_rt_mean~timepoint, data = data, paired=T) #p=0.000732
wilcox_effsize(FRT_rt_mean ~ timepoint, data = data, paired = TRUE)

wilcox.test(FRT_rt_hit_mean~timepoint, data = data, paired=T) #p=0.00024
wilcox_effsize(FRT_rt_hit_mean ~ timepoint, data = data, paired = TRUE)

wilcox.test(FRT_accuracy~timepoint, data = data, paired=T, exact=F) #p=0.0038
wilcox_effsize(FRT_accuracy ~ timepoint, data= data, paired = TRUE)



rm(data, data_nt)







# Supplementary 1: differences between NT groups --------------------------


data <- bigdata[bigdata$group == "NT" & bigdata$timepoint == 1,]


# Chi-square test of homogeneity
Tab = table(data$sex_0_m_1_f, data$study)
chisq.test(Tab, correct=F) 


# Non-parametric independent between-group comparisons (Mann-Whitney-Wilcoxon tests)

# Argument exact=F is added to the function in case of discrete variables 
# (then function cannot compute exact p-values with ties, and so continuity correction is applied)


wilcox.test(age~study, data = data, exact = F)
wilcox.test(education_yrs~study, data = data, exact = F)
wilcox.test(AQ_total~study, data = data, exact = F)
wilcox.test(TAS_sum~study, data = data, exact = F)
wilcox.test(SPF_E_sum~study, data = data, exact = F)
wilcox.test(SPF_C_sum~study, data = data, exact = F)
wilcox.test(SPF_PT_sum~study, data = data, exact = F)
wilcox.test(CEEQ_msp_sum~study, data = data, exact = F)

# FRT

wilcox.test(FRT_rt_mean~study, data = data)
wilcox.test(FRT_rt_hit_mean~study, data = data)
wilcox.test(FRT_accuracy~study, data = data, exact=F)





data <- bigdata[bigdata$group == "NT",]
data <- data[!(data$study == "long" & data$timepoint == 1),]


# non-parametric independent between-group comparisons (Mann-Whitney-Wilcoxon tests)

wilcox.test(SUS_total~study, data = data, exact = F)
wilcox.test(meCUE_usefulness~study, data = data, exact = F)
wilcox.test(meCUE_usability~study, data = data, exact = F)
wilcox.test(meCUE_aesthetics~study, data = data, exact = F)
wilcox.test(meCUE_relationship~study, data = data, exact = F)
wilcox.test(meCUE_posemo~study, data = data, exact = F)
wilcox.test(meCUE_negemo~study, data = data, exact = F)
wilcox.test(meCUE_global~study, data = data, exact = F)






# Non-parametric Spearman correlation between socio-cognitive ability + EVA evaluation (study 1 only / both studies separately) ---------------------------------------------------------------------




library(psych)
library(corrplot)

data_nt_lab <- bigdata[bigdata$study == "lab" & bigdata$group == "NT",] #N=21
data_all_lab <- bigdata[bigdata$study == "lab",] #N=31


data_all_lab<-data_all_lab[c(8,9,12,19:33,37,41:45)]

print(corr.test(data_all_lab, use = "pairwise", method = "spearman", adjust = "fdr", 
                alpha = .05, ci = TRUE, minlength = 4), digits = 4, short=T)



data_nt_lab<-data_nt_lab[c(8,9,12,19:33,37,41:45)]

print(corr.test(data_nt_lab, use = "pairwise", method = "spearman", adjust = "fdr", 
                alpha = .05, ci = TRUE, minlength = 4), digits = 4, short=T)







# prepare data set for correlational analyses on (A) all NTs of study 1+2 (B) all participants

data_all_long_t0 <- bigdata[bigdata$study == "long" & bigdata$timepoint == 1,]
data_all_long_t1 <- bigdata[bigdata$study == "long" & bigdata$timepoint == 2,]
data_merged_long<-cbind(data_all_long_t0[,1:25], data_all_long_t1[,26:33], data_all_long_t0[,34:46])

data_all_n44<-rbind(data_all_lab, data_merged_long)



data_all_NT <- data_all_n44[data_all_n44$group == "NT",] #N=31
data_all_NT<-data_all_NT[c(8,9,12,26:33,37,41:45)] #no NASA correlations because not part of study 2

print(corr.test(data_all_NT, use = "pairwise", method = "spearman", adjust = "fdr", 
                alpha = .05, ci = TRUE, minlength = 4), digits = 4, short=T)





data_all_n44<-data_all_n44[c(8,9,12,26:33,37,41:45)] #no NASA correlations because not part of study 2

print(corr.test(data_all_n44, use = "pairwise", method = "spearman", adjust = "fdr", 
                alpha = .05, ci = TRUE, minlength = 4), digits = 4, short=T)


###########################################################################################



