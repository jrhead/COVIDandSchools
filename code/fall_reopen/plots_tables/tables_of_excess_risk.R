### This file generates Figures 4 and 5 in the manuscript: Effect of various reopening strategies ###

library(lubridate)
library(ggplot2)
library(matrixStats)
library(forcats)
library(stringr)
library(ggpubr)
library(RColorBrewer)
library(ggstance)


## Load output data of interest and rename them to have unique names

#Interventions where hybrid by half class
load(".\\output\\main_reopening_scenarios\\ReopeningSims_HighCommunityTrans_KidsEquallySusceptible.RData")
outcomesHT.ES <- outcomes_full
load(".\\output\\main_reopening_scenarios\\ReopeningSims_ModerateCommunityTrans_KidsEquallySusceptible.RData")
outcomesMT.ES <- outcomes_full
load(".\\output\\main_reopening_scenarios\\ReopeningSims_HighCommunityTrans_KidsHalfSusceptible.RData")
outcomesHT.HS <- outcomes_full
load(".\\output\\main_reopening_scenarios\\ReopeningSims_ModerateCommunityTrans_KidsHalfSusceptible.RData")
outcomesMT.HS <- outcomes_full
rm(outcomes_full)

#Itnerventions where hybrid by grade shifts
load(".\\output\\main_reopening_scenarios\\ReopeningSims_HighCommunityTrans_KidsEquallySusceptible_HybridByGrade.RData")
load(".\\output\\main_reopening_scenarios\\ReopeningSims_ModerateCommunityTrans_KidsEquallySusceptible_HybridByGrade.RData")
load(".\\output\\main_reopening_scenarios\\ReopeningSims_HighCommunityTrans_KidsHalfSusceptible_HybridByGrade.RData")
load(".\\output\\main_reopening_scenarios\\ReopeningSims_ModerateCommunityTrans_KidsHalfSusceptible_HybridByGrade.RData")

#Load function for summarizing output
source(".\\code\\fall_reopen\\plots_tables\\functions_for_summarizing_ouput.R")

N <- nrow(data.frame(outcomesHT.ES[[1]][12])) #Number of agents

########################################################################################
###                EXCESS RISK FOR ANY INFECTION (incl. asymptomatic)                ###
########################################################################################

##HT = high community transmission; MT = moderate community transmission
##ES = kids equally susceptible; HS = kids half as susceptible

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies.R
excessrisk_seroprev.HT.ES <- calc_excess_risk_seroprev(outcomesHT.ES, lci = 0.025, uci = 0.975)
excessrisk_seroprev.MT.ES <- calc_excess_risk_seroprev(outcomesMT.ES, lci = 0.025, uci = 0.975)
excessrisk_seroprev.HT.HS <- calc_excess_risk_seroprev(outcomesHT.HS, lci = 0.025, uci = 0.975)
excessrisk_seroprev.MT.HS <- calc_excess_risk_seroprev(outcomesMT.HS, lci = 0.025, uci = 0.975)

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies_hybridbygrade.R
excessrisk.HT.ES2 <- calc_excess_risk_seroprev2(reopening2.HT.ES, lci = 0.025, uci = 0.975)
excessrisk.MT.ES2 <- calc_excess_risk_seroprev2(reopening2.MT.ES, lci = 0.025, uci = 0.975)
excessrisk.HT.HS2 <- calc_excess_risk_seroprev2(reopening2.HT.HS, lci = 0.025, uci = 0.975)
excessrisk.MT.HS2 <- calc_excess_risk_seroprev2(reopening2.MT.HS, lci = 0.025, uci = 0.975)

########################################################################################
###                     EXCESS RISK FOR ANY CLINICAL INFECTION                       ###
########################################################################################

##HT = high community transmission; MT = moderate community transmission
##ES = kids equally susceptible; HS = kids half as susceptible

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies.R
excessrisk.HT.ES <- calc_excess_risk(outcomesHT.ES, lci = 0.025, uci = 0.975)
excessrisk.MT.ES <- calc_excess_risk(outcomesMT.ES, lci = 0.025, uci = 0.975)
excessrisk.HT.HS <- calc_excess_risk(outcomesHT.HS, lci = 0.025, uci = 0.975)
excessrisk.MT.HS <- calc_excess_risk(outcomesMT.HS, lci = 0.025, uci = 0.975)

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies_hybridbygrade.R
excessrisk.HT.ES2 <- calc_excess_risk2(reopening2.HT.ES, lci = 0.025, uci = 0.975)
excessrisk.MT.ES2 <- calc_excess_risk2(reopening2.MT.ES, lci = 0.025, uci = 0.975)
excessrisk.HT.HS2 <- calc_excess_risk2(reopening2.HT.HS, lci = 0.025, uci = 0.975)
excessrisk.MT.HS2 <- calc_excess_risk2(reopening2.MT.HS, lci = 0.025, uci = 0.975)

########################################################################################
###                        EXCESS RISK FOR HOSPITALIZATION                           ###
########################################################################################

##HT = high community transmission; MT = moderate community transmission
##ES = kids equally susceptible; HS = kids half as susceptible

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies.R
excessrisk_hosp.HT.ES <- calc_excess_risk_hosp(outcomesHT.ES, lci = 0.025, uci = 0.975)
excessrisk_hosp.MT.ES <- calc_excess_risk_hosp(outcomesMT.ES, lci = 0.025, uci = 0.975)
excessrisk_hosp.HT.HS <- calc_excess_risk_hosp(outcomesHT.HS, lci = 0.025, uci = 0.975)
excessrisk_hosp.MT.HS <- calc_excess_risk_hosp(outcomesMT.HS, lci = 0.025, uci = 0.975)

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies_hybridbygrade.R
excessrisk_hosp.HT.ES2 <- calc_excess_risk_hosp2(reopening2.HT.ES, lci = 0.025, uci = 0.975)
excessrisk_hosp.MT.ES2 <- calc_excess_risk_hosp2(reopening2.MT.ES, lci = 0.025, uci = 0.975)
excessrisk_hosp.HT.HS2 <- calc_excess_risk_hosp2(reopening2.HT.HS, lci = 0.025, uci = 0.975)
excessrisk_hosp.MT.HS2 <- calc_excess_risk_hosp2(reopening2.MT.HS, lci = 0.025, uci = 0.975)

########################################################################################
###                              EXCESS RISK OF DEATH                                ###
########################################################################################

##HT = high community transmission; MT = moderate community transmission
##ES = kids equally susceptible; HS = kids half as susceptible

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies.R
excessrisk_mort.HT.ES <- calc_excess_risk_mort(outcomesHT.ES, lci = 0.025, uci = 0.975)
excessrisk_mort.MT.ES <- calc_excess_risk_mort(outcomesMT.ES, lci = 0.025, uci = 0.975)
excessrisk_mort.HT.HS <- calc_excess_risk_mort(outcomesHT.HS, lci = 0.025, uci = 0.975)
excessrisk_mort.MT.HS <- calc_excess_risk_mort(outcomesMT.HS, lci = 0.025, uci = 0.975)

#Call function to generate a dataframe of excess risk and 95% CI, by person group
#these are for the interventions included in main_reopening_strategies_hybridbygrade.R
excessrisk_mort.HT.ES2 <- calc_excess_risk_mort2(reopening2.HT.ES, lci = 0.025, uci = 0.975)
excessrisk_mort.MT.ES2 <- calc_excess_risk_mort2(reopening2.MT.ES, lci = 0.025, uci = 0.975)
excessrisk_mort.HT.HS2 <- calc_excess_risk_mort2(reopening2.HT.HS, lci = 0.025, uci = 0.975)
excessrisk_mort.MT.HS2 <- calc_excess_risk_mort2(reopening2.MT.HS, lci = 0.025, uci = 0.975)