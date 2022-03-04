
# EVA Usability Study - Preprocessing of questionnaire data  --------


# Date:                   February 08, 2022
# Study investigators:    Dr. Anne Weigand & Prof. Dr. Isabel Dziobek
# Author:                 Lioba Enk - contact: lioba.enk@maxplanckschools.de




rm(list=ls()) 

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggpubr)


# Note on NASA-TLX: This questionnaire (assessing work load) was analysed automatically
# via this platform where conduction also took place:
# https://humansystems.arc.nasa.gov/groups/tlx/
# Subscales: - Mental Demand, Physical Demand, Temporal Demand, Performance, Effort, Frustration 




# Settings ------------------------------------------------------------------



main_path= 'C:/Users/Lioba/Dropbox/EVA_Franziska_Anne/1_EVA_Geordnet/2_analysis/Analysis_Lioba_2022/data/' # main preprocessing output folder

setwd(main_path)




# Read --------------------------------------------------------------------



#bigdata2 = read.csv2('C:/Users/Lioba/Dropbox/EVA_Franziska_Anne/1_EVA_Geordnet/2_analysis/Analysis_Lioba_2022/data/raw/EVA_usability_raw_master_060222.csv')
bigdata = read.csv2('data/raw/EVA_usability_raw_master_060222.csv')



# SUS ---------------------------------------------------------------------


# 5-point Likert scale from 1 (totally disagree) to 5 (totally agree)


bigdata <- bigdata %>%
  mutate(adapt_sus_reg_sum1 = (SUS_I1+SUS_I3+SUS_I5+SUS_I7+SUS_I9)-5,
         adapt_sus_reg_sum2 = 25-(SUS_I2+SUS_I4+SUS_I6+SUS_I8+SUS_I10),
         SUS_total = (adapt_sus_reg_sum1+adapt_sus_reg_sum2)*2.5
  )



# meCUE -------------------------------------------------------------------


# 7-point Likert scale with items ranging from 1 (totally disagree) to 4 (neutral) to 7 (totally agree)
# meCUE_global item rangs from -5 to 0 to +5, does not require preprocessing


bigdata <- bigdata %>%
  mutate(meCUE_usefulness = (meCUE_N1+meCUE_N2+meCUE_N3)/3, #Nuetzlichkeit
         meCUE_usability = (meCUE_U1+meCUE_U2+meCUE_U3)/3, #Benutzbarkeit
         meCUE_aesthetics = (meCUE_A1+meCUE_A2+meCUE_A3)/3,
         meCUE_relationship = (meCUE_B1+meCUE_B2+meCUE_B3)/3,
         meCUE_posemo = (meCUE_AP1+meCUE_AP2+meCUE_AP3+meCUE_DP1+meCUE_DP2+meCUE_DP3)/6,
         meCUE_negemo = (meCUE_AN1+meCUE_AN2+meCUE_AN3+meCUE_DN1+meCUE_DN2+meCUE_DN3)/6
  )


bigdata$meCUE_global<-as.numeric(sub(",", ".", sub(".", "", bigdata$meCUE_global, fixed=TRUE), fixed=TRUE)) #exchange , by . for decimals
bigdata$meCUE_global<-as.numeric(bigdata$meCUE_global)
which(bigdata$meCUE_global<(-5))
bigdata[32,161]
bigdata[32,161] = (-3.5) #hard coded
bigdata[42,161]
bigdata[42,161] = (-1.5)


# AQK - short version -----------------------------------------------------


# Items are coded 0 (disagree, slightly disagree) or 1 (slightly agree, agree)
# retrieved analysis instructions from: https://novopsych.com.au/assessments/diagnosis/autism-spectrum-quotient/


bigdata <- bigdata %>%
  mutate(AQK_SIS = AQK_1+AQK_7+AQK_8+AQK_10+AQK_11+AQK_13+AQK_14+AQK_20+AQK_24+AQK_28
         +AQK_31, #Social Interaction and Spontaneity
         AQK_FV = AQK_3+AQK_5+AQK_6+AQK_9+AQK_16+AQK_17+AQK_18+AQK_22+AQK_23+AQK_26
         +AQK_32+AQK_33, #Fantasie + Vorstellungsverm?gen
         AQK_KV = AQK_2+AQK_4+AQK_12+AQK_15+AQK_19+AQK_21+AQK_25+AQK_27+AQK_29+AQK_30, #Kommunication + Reziprozitaet
         AQ_total = AQK_SIS+AQK_FV+AQK_KV
  )




# TAS-20 ------------------------------------------------------------------


# 5-point Likert scale ranging from 1 (strongly disagree) to 3 (neutral) to 5 (strongly agree)


# reverse reversed items
bigdata$TAS_4_rev_done = 6-bigdata$TAS_4_rev
bigdata$TAS_5_rev_done = 6-bigdata$TAS_5_rev
bigdata$TAS_10_rev_done = 6-bigdata$TAS_10_rev
bigdata$TAS_18_rev_done = 6-bigdata$TAS_18_rev
bigdata$TAS_19_rev_done = 6-bigdata$TAS_19_rev


bigdata <- bigdata %>%
  mutate(TAS_identify_sum = TAS_1+TAS_3+TAS_6+TAS_7+TAS_9+TAS_13+TAS_14,
         TAS_describe_sum = TAS_2+TAS_4_rev_done+TAS_11+TAS_12+TAS_17,
         TAS_eot_sum = TAS_5_rev_done+TAS_8+TAS_10_rev_done+TAS_15+TAS_16+TAS_18_rev_done+TAS_19_rev_done+TAS_20,
         TAS_sum = TAS_identify_sum+TAS_describe_sum+TAS_eot_sum
  )





# SPF ---------------------------------------------------------------------


# 5-point Likert ranging from 1 to 5 (nie, selten, manchmal, oft, immer)


# reverse reversed items
bigdata$SPF_05_E_rev_done = 6-bigdata$SPF_05_E_rev
bigdata$SPF_17_E_rev_done = 6-bigdata$SPF_17_E_rev
bigdata$SPF_04_P_rev_done = 6-bigdata$SPF_04_P_rev



bigdata <- bigdata %>%
  mutate(SPF_E_sum = SPF_02_E+SPF_05_E_rev_done+SPF_11_E+SPF_17_E_rev_done+SPF_23_E+SPF_25_E+SPF_28_E,
         SPF_C_sum = SPF_03_K+SPF_08_K+SPF_13_K+SPF_18_K+SPF_21_K+SPF_26_K+SPF_35_K,
         SPF_PT_sum = SPF_04_P_rev_done+SPF_10_P+SPF_14_P+SPF_19_P+SPF_27_P+SPF_31_P+SPF_34_P
  )




# CEEQ --------------------------------------------------------------------


# 5-point Likert scale ranging from 1 (does not apply at all) to 5 (applies absolutely)


# reverse reversed items
bigdata$ceeq_msp1_rev_done = 6-bigdata$ceeq_msp1_rev
bigdata$ceeq_msp7_rev_done = 6-bigdata$ceeq_msp7_rev

bigdata <- bigdata %>%
  mutate(CEEQ_msp_sum = ceeq_msp1_rev_done+ceeq_msp2+ceeq_msp3+ceeq_msp_4+ceeq_msp5+ceeq_msp6+ceeq_msp7_rev_done+ceeq_msp8,
         CEEQ_msp_mean = (ceeq_msp1_rev_done+ceeq_msp2+ceeq_msp3+ceeq_msp_4+ceeq_msp5+ceeq_msp6+ceeq_msp7_rev_done+ceeq_msp8)/8
  )




# FRT ----------------------------------------------------------------------


# create accuracy (percentage of correct responses) variable

bigdata <- bigdata %>% mutate(FRT_accuracy = FRT_n_hits/25*100)




# Save preprocessed -------------------------------------------------------



bigdata2 = bigdata[,c(1:7, 194, 88:98, 101, 104, 107, 110, 113, 116, 164, 165:170, 161, 171:174, 180:183, 187:189, 192:193)] #hard coded!


write.csv(bigdata2, file=paste(main_path,'EVA_usability_preprocessed_new.csv'), row.names = FALSE)






###########################################################################################



