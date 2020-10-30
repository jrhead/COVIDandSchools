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
paired.palette <- brewer.pal(12, "Paired") #save color scheme


########################################################
##   Figure 4. Excess risk for each subgroup          ##
########################################################

##HT = high community transmission; MT = moderate community transmission
##ES = kids equally susceptible; HS = kids half as susceptible

#Call function to generate a dataframe of excess risk, IQR on excess risk, by person group
#these are for the interventions included in main_reopening_strategies.R
excessrisk.HT.ES <- calc_excess_risk(outcomesHT.ES, lci = 0.25, uci = 0.75)
excessrisk.MT.ES <- calc_excess_risk(outcomesMT.ES, lci = 0.25, uci = 0.75)
excessrisk.HT.HS <- calc_excess_risk(outcomesHT.HS, lci = 0.25, uci = 0.75)
excessrisk.MT.HS <- calc_excess_risk(outcomesMT.HS, lci = 0.25, uci = 0.75)

#Call function to generate a dataframe of excess risk, IQR on excess risk, by person group
#these are for the interventions included in main_reopening_strategies_hybridbygrade.R
excessrisk.HT.ES2 <- calc_excess_risk2(reopening2.HT.ES, lci = 0.25, uci = 0.75)
excessrisk.MT.ES2 <- calc_excess_risk2(reopening2.MT.ES, lci = 0.25, uci = 0.75)
excessrisk.HT.HS2 <- calc_excess_risk2(reopening2.HT.HS, lci = 0.25, uci = 0.75)
excessrisk.MT.HS2 <- calc_excess_risk2(reopening2.MT.HS, lci = 0.25, uci = 0.75)

#Define susceptibility and community transmission levels
excessrisk.HT.ES$SusceptRatio <- excessrisk.MT.ES$SusceptRatio <- 
  excessrisk.HT.ES2$SusceptRatio <- excessrisk.MT.ES2$SusceptRatio <- "Children equally susceptible"

excessrisk.HT.HS$SusceptRatio <- excessrisk.MT.HS$SusceptRatio <- 
  excessrisk.HT.HS2$SusceptRatio <- excessrisk.MT.HS2$SusceptRatio <- "Children half as susceptible"

excessrisk.HT.ES$CommunityTrans <- excessrisk.HT.HS$CommunityTrans <- 
  excessrisk.HT.ES2$CommunityTrans <- excessrisk.HT.HS2$CommunityTrans <- "High"

excessrisk.MT.HS$CommunityTrans <- excessrisk.MT.ES$CommunityTrans <- 
  excessrisk.MT.HS2$CommunityTrans <- excessrisk.MT.ES2$CommunityTrans <- "Moderate"

#Define susceptibility and community transmission levels
excessrisk_full <- rbind(excessrisk.HT.ES, excessrisk.HT.ES2, excessrisk.MT.ES, excessrisk.MT.ES2,
                         excessrisk.HT.HS, excessrisk.HT.HS2, excessrisk.MT.HS, excessrisk.MT.HS2)

#Subet the dataframe to include just what we want to plot
subset.risk <- subset(excessrisk_full, Classification %in% c("Elementary teacher", "Middle school teacher", "High school teacher",
                                                             "Elementary student", "Middle school student", "High school student",
                                                             "HH member", "Community member") &
                        Intervention != "Weekly testing of teachers" & Intervention != "Monthly testing of teachers" &
                        Intervention != "Schools closed")

#Define 4 roles: Household member, community member, student, and teacher
subset.risk$Role <- "Household member"
subset.risk$Role[subset.risk$Classification == "Community member"] <- "Community member"
subset.risk$Role[subset.risk$Classification %in% c("Elementary student", "Middle school student", "High school student")] <- "Student"
subset.risk$Role[subset.risk$Classification %in% c("Elementary teacher", "Middle school teacher", "High school teacher")] <- "Teacher"

#Define school levels: Elementary, middle, high school
subset.risk$Level <- "N/A"
subset.risk$Level[subset.risk$Classification %in% c("Middle school teacher", "Middle school student")] <- "Middle school"
subset.risk$Level[subset.risk$Classification %in% c("High school teacher", "High school student")] <- "High school"
subset.risk$Level[subset.risk$Classification %in% c("Elementary teacher", "Elementary student")] <- "Elementary school"

#Re-level factor variables
subset.risk$Intervention <- factor(subset.risk$Intervention,
                                   levels = c( 
                                     "2-day half class shifts",
                                     "2-day staggered grades",
                                     "Stable cohorts (strong)", 
                                     "Masks", 
                                     "Stable cohorts (weak)", 
                                     "Weekly testing of teachers + students", 
                                     "Monthly testing of teachers + students", 
                                     "No additional precautions"))


subset.risk$Scenario <- factor(subset.risk$CommunityTrans,
                               levels = c("Moderate", "High"))

subset.risk$Level <- factor(subset.risk$Level,
                            levels = c("Elementary school", "Middle school", "High school", "N/A"))

subset.risk$SusceptRatio <- factor(subset.risk$SusceptRatio,
                                   levels = c("Children half as susceptible", "Children equally susceptible"))

#Create lines for easier viewing of the plot - to separate the interventions
line_positions = as.numeric(factor(subset.risk$Intervention, levels = unique(subset.risk$Intervention)))
line_positions = line_positions + .5
line_positions = unique(line_positions)
line_positions = line_positions[!is.na(line_positions)]


### Left Side of Plot: Students and Teachers only ###

#subset data to just teachers and students
subset.risk1 <- subset(subset.risk, Role == "Teacher" | Role == "Student")
subset.risk1$Role <- factor(subset.risk1$Role,
                            levels = c("Teacher", "Student"))

#create data frame that allows drawing a vertical line at 0 excess risk
vline.data <- data.frame(cbind("z" = c(0,0,0,0), 
                               "Role" = c("Teacher", "Teacher", "Student", "Student"),
                               "susceptRatio" = rep(c("Children half as susceptible", "Chilren equally susceptible"),2)
))

#create data frame that allows drawing rectangles for separation of the interventions
rect.data <- data.frame(cbind("line_positions" = rep(line_positions,4),
                              "Role" = c(rep("Teacher", 2*length(line_positions)), rep("Student", 2*length(line_positions))),
                              "susceptRatio" = rep(c(rep("Children half as susceptible",length(line_positions)),
                                                     rep("Children equally susceptible", length(line_positions))),2)))
rect.data$line_positions <- as.numeric(as.character(rect.data$line_positions))

#Create the left hand side of the plot -- teachers and students
excess.risk1 <- ggplot(data=subset.risk1) +
  geom_point(data = subset(subset.risk1, !is.na(Intervention)),
             mapping = aes(x = P.Infect*100, y = Intervention, color = Level, shape = Scenario),
             position = position_dodgev(height = .99),
             size = 2) +
  geom_errorbarh(data = subset(subset.risk1, !is.na(Intervention)),
                 mapping = aes(xmin = P.Infect.Low*100, xmax = P.Infect.High*100, 
                               y = Intervention, color = Level, shape = Scenario),
                 width = 0.0, position = position_dodgev(height = .99), size = 0.5) +
  geom_vline(aes(xintercept = 0), vline.data, linetype = "dashed") +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[1]-1, ymax = line_positions[1]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[3]-1, ymax = line_positions[3]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[5]-1, ymax = line_positions[5]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[7]-1, ymax = line_positions[7]), alpha = 0.02) +
  theme_bw() + 
  scale_color_manual("School:", breaks = c("High school", "Middle school", "Elementary school"),
                     values = c("navy", "medium blue", "lightskyblue")) + 
  scale_shape_manual("Community transmission:", breaks = c("High", "Moderate"),
                     values = c("circle", "cross")) +
  xlab("Additional percentage with symptomatic infection") + ylab("") +
  theme(text = element_text(size = 20), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.y = element_blank(), 
        legend.position = "top", plot.title = element_text(face = "bold")) +
  facet_grid(Role ~ SusceptRatio)

excess.risk1

### Left Side of Plot: Household and community members only ###

#subset data to just HH and community members
subset.risk2 <- subset(subset.risk, Role == "Community member" | Role == "Household member")
subset.risk2$Role <- factor(subset.risk2$Role,
                            levels = c("Community member", "Household member"))

#create data frame that allows drawing a vertical line at 0 excess risk
vline.data <- data.frame(cbind("z" = c(0,0,0,0), 
                               "Role" = c("Community member", "Community member", "Household member", "Household member"),
                               "susceptRatio" = rep(c("Children half as susceptible", "Chilren equally susceptible"),2)
))

#create data frame that allows drawing rectangles for separation of the interventions
rect.data <- data.frame(cbind("line_positions" = rep(line_positions,4),
                              "Role" = c(rep("Community member", 2*length(line_positions)), rep("Household member", 2*length(line_positions))),
                              "susceptRatio" = rep(c(rep("Children half as susceptible",length(line_positions)),
                                                     rep("Children equally susceptible", length(line_positions))),2)))
rect.data$line_positions <- as.numeric(as.character(rect.data$line_positions))

#Create the right hand side of the plot -- community members and hh members
excess.risk2 <- ggplot(data=subset.risk2) +
  geom_point(data = subset(subset.risk2, !is.na(Intervention)),
             mapping = aes(x = P.Infect*100, y = Intervention, shape = Scenario),
             position = position_dodgev(height = .85),
             size = 2, color = "red") +
  geom_errorbarh(data = subset(subset.risk2, !is.na(Intervention)),
                 mapping = aes(xmin = P.Infect.Low*100, xmax = P.Infect.High*100, 
                               y = Intervention, shape = Scenario),
                 width = 0.0, position = position_dodgev(height = .85), size = 0.5, color = "red") +
  geom_vline(aes(xintercept = 0), vline.data, linetype = "dashed") +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[1]-1, ymax = line_positions[1]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[3]-1, ymax = line_positions[3]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[5]-1, ymax = line_positions[5]), alpha = 0.02) +
  geom_rect(data = rect.data, aes(xmin = -Inf, xmax = Inf, ymin = line_positions[7]-1, ymax = line_positions[7]), alpha = 0.02) +
  theme_bw() + 
  scale_shape_manual("Community transmission:", breaks = c("High", "Moderate"),
                     values = c("circle", "cross")) +
  xlab("Additional percentage with symptomatic infection") + ylab("") +
  theme(text = element_text(size = 20), 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.grid.major.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.position = "top", plot.title = element_text(face = "bold")) +
  facet_grid(Role ~ SusceptRatio)

excess.risk2

### Combine the two plots into one plot ###

excess.risk <- ggarrange(excess.risk1, excess.risk2, widths = c(1.5,1), common.legend = TRUE)

#Save as a .jpg or .pdf
ggsave(excess.risk, file = "Fig4.ExcessRisksofReopening.jpg", dpi = 600, height = 10, width = 20)
pdf(file = "Fig4.ExcessRisksofReopening.pdf", height = 15, width = 20)
excess.risk
dev.off()

###################################################################
##   Figure 5. Hospitalizations and incidence by group           ##
###################################################################

######################################################################
##   Panel A: excess cumulative infections by subgroup & scenario   ##
######################################################################

# Use the functions from functions_for_plotting.R to extract the
# attrbiutional effect of each school reopening strategy on cumulative incidence
# Do so for each of the 4 scenarios: 
## HT = high transmission; MT = moderate transmission;
## ES = children and adults assumed equally susceptible;
## HS = children assumed half as susceptible as adults

#Functions draw on opening strategies in the main_reopening_strategies.R file
#output dataframe contains information on excess, intervention, group type
allocation.clin.HT.ES <- calc_excess_incidence(outcomesHT.ES)
allocation.clin.MT.ES <- calc_excess_incidence(outcomesMT.ES)
allocation.clin.HT.HS <- calc_excess_incidence(outcomesHT.HS)
allocation.clin.MT.HS <- calc_excess_incidence(outcomesMT.HS)

#Functions draw on opening strategies in the main_reopening_strategies_hybridbygrade.R file
#output dataframe contains information on excess, intervention, group type
allocation.clin.HT.ES2 <- calc_excess_incidence2(reopening2.HT.ES)
allocation.clin.MT.ES2 <- calc_excess_incidence2(reopening2.MT.ES)
allocation.clin.HT.HS2 <- calc_excess_incidence2(reopening2.HT.HS)
allocation.clin.MT.HS2 <- calc_excess_incidence2(reopening2.MT.HS)

#Define susceptibility and community transmission levels
allocation.clin.HT.ES$SusceptRatio <- allocation.clin.MT.ES$SusceptRatio <- 
  allocation.clin.HT.ES2$SusceptRatio <- allocation.clin.MT.ES2$SusceptRatio <- "Children equally susceptible"

allocation.clin.HT.HS$SusceptRatio <- allocation.clin.MT.HS$SusceptRatio <- 
  allocation.clin.HT.HS2$SusceptRatio <- allocation.clin.MT.HS2$SusceptRatio <- "Children half as susceptible"

allocation.clin.HT.ES$CommunityTrans <- allocation.clin.HT.HS$CommunityTrans <- 
  allocation.clin.HT.ES2$CommunityTrans <- allocation.clin.HT.HS2$CommunityTrans <- "High"

allocation.clin.MT.HS$CommunityTrans <- allocation.clin.MT.ES$CommunityTrans <- 
  allocation.clin.MT.HS2$CommunityTrans <- allocation.clin.MT.ES2$CommunityTrans <- "Moderate"

#Bind the rows together into one dataframe
allocation.clin_full <- rbind(allocation.clin.HT.ES, allocation.clin.HT.ES2, allocation.clin.MT.ES, allocation.clin.MT.ES2,
                              allocation.clin.HT.HS, allocation.clin.HT.HS2, allocation.clin.MT.HS, allocation.clin.MT.HS2)


### Plot ###
#re-level factor variables
allocation.clin_full$Intervention <- factor(allocation.clin_full$Intervention,
                                            levels = c( 
                                              "2-day half class shifts",
                                              "2-day staggered grades",
                                              "Stable cohorts (strong)", 
                                              "Masks", 
                                              "Stable cohorts (weak)", 
                                              "Weekly testing of teachers + students", 
                                              "Monthly testing of teachers + students", 
                                              "No additional precautions"))


allocation.clin_full$CommunityTrans <- factor(allocation.clin_full$CommunityTrans,
                                              levels = c("Moderate", "High"))

allocation.clin_full$SusceptRatio <- factor(allocation.clin_full$SusceptRatio,
                                            levels = c("Children half as susceptible",
                                                       "Children equally susceptible"))
#plot 

clin.plot <- ggplot(subset(allocation.clin_full, !is.na(Intervention))) + 
  geom_bar(aes(x = CommunityTrans, y = Attribution/N*10000, 
               fill = factor(Classification, 
                             levels = c( "Community member","HH member",
                                         "Teacher", "Student"))),
           position="stack", stat="identity") +
  theme_bw() + 
  theme(text = element_text(size = 12)) + 
  theme(legend.position = "top") +
  ylab("Excess cumulative incidence per 10,000") +
  scale_fill_manual("Group:", breaks = c("Student", "Teacher", "HH member", "Community member"),
                    values = c(paired.palette[6], paired.palette[7], paired.palette[2],
                               paired.palette[4]))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
  geom_hline(aes(yintercept = 0)) + xlab("Community transmission levels") + 
  facet_grid(SusceptRatio ~ Intervention, labeller = label_wrap_gen(20)) 

######################################################################
##  Panel B: excess mean daily hospiatlizations by scenario & group ##
######################################################################

# Use the functions from functions_for_plotting.R to extract the
# attrbiutional effect of each school reopening strategy on excess mean daily hospitalizations
# Do so for each of the 4 scenarios: 
## HT = high transmission; MT = moderate transmission;
## ES = children and adults assumed equally susceptible;
## HS = children assumed half as susceptible as adults

#Functions draw on opening strategies in the main_reopening_strategies.R file
#output dataframe contains information on excess, intervention, group type
allocation.hosp.HT.ES <- calc_excess_hosp(outcomesHT.ES) #use default confidence intervals
allocation.hosp.MT.ES <- calc_excess_hosp(outcomesMT.ES)
allocation.hosp.HT.HS <- calc_excess_hosp(outcomesHT.HS)
allocation.hosp.MT.HS <- calc_excess_hosp(outcomesMT.HS)

#Functions draw on opening strategies in the main_reopening_strategies_hybridbygrade.R file
#output dataframe contains information on excess, intervention, group type
allocation.hosp.HT.ES2 <- calc_excess_hosp2(reopening2.HT.ES)
allocation.hosp.MT.ES2 <- calc_excess_hosp2(reopening2.MT.ES)
allocation.hosp.HT.HS2 <- calc_excess_hosp2(reopening2.HT.HS)
allocation.hosp.MT.HS2 <- calc_excess_hosp2(reopening2.MT.HS)

#Define susceptibility and community transmission levels
allocation.hosp.HT.ES$SusceptRatio <- allocation.hosp.MT.ES$SusceptRatio <- 
  allocation.hosp.HT.ES2$SusceptRatio <- allocation.hosp.MT.ES2$SusceptRatio <- "Children equally susceptible"

allocation.hosp.HT.HS$SusceptRatio <- allocation.hosp.MT.HS$SusceptRatio <- 
  allocation.hosp.HT.HS2$SusceptRatio <- allocation.hosp.MT.HS2$SusceptRatio <- "Children half as susceptible"

allocation.hosp.HT.ES$CommunityTrans <- allocation.hosp.HT.HS$CommunityTrans <- 
  allocation.hosp.HT.ES2$CommunityTrans <- allocation.hosp.HT.HS2$CommunityTrans <- "High"

allocation.hosp.MT.HS$CommunityTrans <- allocation.hosp.MT.ES$CommunityTrans <- 
  allocation.hosp.MT.HS2$CommunityTrans <- allocation.hosp.MT.ES2$CommunityTrans <- "Moderate"

#Bind the rows together into one dataframe
allocation.hosp_full <- rbind(allocation.hosp.HT.ES, allocation.hosp.HT.ES2, allocation.hosp.MT.ES, allocation.hosp.MT.ES2,
                         allocation.hosp.HT.HS, allocation.hosp.HT.HS2, allocation.hosp.MT.HS, allocation.hosp.MT.HS2)

### Generate plot B ###

#re-level factor variables
allocation.hosp_full$Intervention <- factor(allocation.hosp_full$Intervention,
                                            levels = c( 
                                              "2-day half class shifts",
                                              "2-day staggered grades",
                                              "Stable cohorts (strong)", 
                                              "Masks", 
                                              "Stable cohorts (weak)", 
                                              "Weekly testing of teachers + students", 
                                              "Monthly testing of teachers + students", 
                                              "No additional precautions"))


allocation.hosp_full$CommunityTrans <- factor(allocation.hosp_full$CommunityTrans,
                                              levels = c("Moderate", "High"))

allocation.hosp_full$SusceptRatio <- factor(allocation.hosp_full$SusceptRatio,
                                            levels = c("Children half as susceptible",
                                                       "Children equally susceptible"))

#plot

#outcome is in cumulative hospitalizations -- convert to mean daily by dividing by # of days during semester (125) and
# multiplying by average duration of stay in hospital (14.4 days)
hosp.plot <- ggplot(subset(allocation.hosp_full, !is.na(Intervention))) + 
  geom_bar(aes(x = CommunityTrans, y = Attribution/N*10000/125*14.4, #convert from cumulative people hospitalized to mean daily hospitalizations
               fill = factor(Classification,
                             levels = c( "Community member","HH member",
                                         "Teacher", "Student"))),
           position="stack", stat="identity") +
  theme_bw() + 
  theme(text = element_text(size = 12)) + 
  theme(legend.position = "top") +
  ylab("Excess mean daily hospitalization rate (per 10,000)") +
  scale_fill_manual("Group:", breaks = c("Student", "Teacher", "HH member", "Community member"),
                    values = c(paired.palette[6], paired.palette[7], paired.palette[2],
                               paired.palette[4]))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) + 
  geom_hline(aes(yintercept = 0)) + xlab("Community transmission levels") + 
  facet_grid(SusceptRatio ~ Intervention, labeller = label_wrap_gen(20)) 

hosp.plot


#### Combine plots and save ####
clinhosp <-  ggarrange(clin.plot, hosp.plot, nrow = 2, labels = c("A. Excess incidence", "B. Excess daily hospitalizations"),
                       hjust = -.1)

# save as .jpg or pdf
ggsave(clinhosp2, file = "Fig5Reopening_excess_incidence_hospitalizations.jpg", dpi = 600, height = 10, width = 12)

pdf(file = "Fig5Reopening_excess_incidence_hospitalizations.pdf", height = 10, width=12)
clinhosp
dev.off()
getwd()

