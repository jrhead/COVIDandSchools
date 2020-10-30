### This file generates Figure 2 in the manuscript: Effect of spring semester interventions ###

library(lubridate)
library(ggplot2)
library(matrixStats)
library(forcats)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(ggpubr)


## Load output data of interest
#Here, loading the situation where children < 10 years are assumed to be 
#half as susceptible to SARS-CoV-2 as adults
load(".\\output\\main_counterfactual_interventions\\SpringSemester_Less10HalfSusceptible_CF1_6.RData")

reps <- length(outcomes) #number of simulations
N <- nrow(data.frame(outcomes[[1]][8])) #agents
ndays <- length(outcomes[[1]]$OBS$CumTotal_I) # days simulation run for
  
###################################################################
##           Panel A: cumulative incidence by scenario           ##
###################################################################

#Initialize storage vector for each of the counterfactual scenarios examined
vectOBS <- vectCF1 <- vectCF2 <- vectCF3 <- vectCF4 <- 
  vectCF5 <- vectCF6 <- matrix(NA, ncol = ndays, nrow = reps)

#loop through each simulation and capture cumulative symptomatic incindence over time 
for(i in 1:reps){
  vectOBS[i,] <- outcomes[[i]]$OBS$CumTotal_IC #IC = clinical incidence
  vectCF1[i,] <- outcomes[[i]]$CF1$CumTotal_IC
  vectCF2[i,] <- outcomes[[i]]$CF2$CumTotal_IC
  vectCF3[i,] <- outcomes[[i]]$CF3$CumTotal_IC
  vectCF4[i,] <- outcomes[[i]]$CF4$CumTotal_IC
  vectCF5[i,] <- outcomes[[i]]$CF5$CumTotal_IC
  vectCF6[i,] <- outcomes[[i]]$CF6$CumTotal_IC
}

#Calculate the daily mean and CI's across the realizations of the model
dataframe <- as.data.frame(cbind("day" = 1:ndays,
                                 "OBS" = colMeans(vectOBS, na.rm = T),
                                 "OBS.low" = colQuantiles(vectOBS, probs = 0.025),
                                 "OBS.high" = colQuantiles(vectOBS, probs = 0.975),
                                 "CF1" = colMeans(vectCF1, na.rm = T),
                                 "CF1.low" = colQuantiles(vectCF1, probs = 0.025),
                                 "CF1.high" = colQuantiles(vectCF1, probs = 0.975),
                                 "CF2" = colMeans(vectCF2, na.rm = T),
                                 "CF2.low" = colQuantiles(vectCF2, probs = 0.025),
                                 "CF2.high" = colQuantiles(vectCF2, probs = 0.975),
                                 "CF3" = colMeans(vectCF3, na.rm = T),
                                 "CF3.low" = colQuantiles(vectCF3, probs = 0.025),
                                 "CF3.high" = colQuantiles(vectCF3, probs = 0.975),
                                 "CF4" = colMeans(vectCF4, na.rm = T),
                                 "CF4.low" = colQuantiles(vectCF4, probs = 0.025),
                                 "CF4.high" = colQuantiles(vectCF4, probs = 0.975),
                                 "CF5" = colMeans(vectCF5, na.rm = T),
                                 "CF5.low" = colQuantiles(vectCF5, probs = 0.025),
                                 "CF5.high" = colQuantiles(vectCF5, probs = 0.975),
                                 "CF6" = colMeans(vectCF6, na.rm = T),
                                 "CF6.low" = colQuantiles(vectCF6, probs = 0.025),
                                 "CF6.high" = colQuantiles(vectCF6, probs = 0.975)))

#Generate date, starting from date of simulation start (Jan 17, roughly 2 weeks before 1st known case)
dataframe$date <- as.Date(dataframe$day, origin = "2020-01-17")

#Generate plot A
paired.palette <- brewer.pal(7, "Paired")

a <- ggplot(subset(dataframe, date >= "2020-02-01"), aes(x=date)) + 
  geom_line(aes(y = OBS/N*10000, color = "Observed interventions"), size = 1) +
  geom_line(aes(y = CF1/N*10000, color = "Schools open"), size = 1) +
  geom_line(aes(y = CF2/N*10000, color = "Workplaces open"), size = 1) +
  geom_line(aes(y = CF3/N*10000, color = "Social gatherings permitted"), size = 1) +
  geom_line(aes(y = CF4/N*10000, color = "Schools + workplaces open"), size = 1) +
  geom_line(aes(y = CF5/N*10000, color = "Schools open + social gatherings permitted"), size = 1) +
  geom_line(aes(y = CF6/N*10000, color = "No interventions"), size = 1) +
  theme_bw() + ylab("Cumulative case incidence per 10,000") +
  xlab("Date") + theme(text = element_text(size = 12)) + 
  theme(axis.text = element_text(size = 15)) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-18")))) +
  labs(color = "Counterfactual Scenario:") + xlab("") +
  scale_color_manual(breaks = c("No interventions", "Schools + workplaces open", "Workplaces open",
                                  "Schools open + social gatherings permitted", "Social gatherings permitted", "Schools open",
                                  "Observed interventions"),
                       values = paired.palette)
a
#####################################################################################
##   Panel C: % increase in cumulative incidence by all & school-associated HH's   ##
#####################################################################################

### ALL FAMILIES ###

#Calculate the percent increase in cumulative incidence under each CF scenario, compared to observed
CF1av <- (vectCF1[,ndays] - vectOBS[,ndays])/vectOBS[,ndays] #calculate % increase on final day of simulation
CF2av <- (vectCF2[,ndays] - vectOBS[,ndays])/vectOBS[,ndays]
CF3av <- (vectCF3[,ndays] - vectOBS[,ndays])/vectOBS[,ndays]
CF4av <- (vectCF4[,ndays] - vectOBS[,ndays])/vectOBS[,ndays]
CF5av <- (vectCF5[,ndays] - vectOBS[,ndays])/vectOBS[,ndays]
CF6av <- (vectCF6[,ndays] - vectOBS[,ndays])/vectOBS[,ndays]

#Create dataframe with % increase and intervention name
Averted <- c(CF1av, CF2av, CF3av, CF6av) #combine for plot (look at 4 CFs)

Int <-c(rep("Schools open", reps),
        rep("Workplaces open", reps),
        rep("Social gatherings permitted", reps),
        rep("No interventions", reps))

boxplot <- data.frame(cbind(Averted, Int, "Population" = "All households"))
boxplot$Averted <- as.numeric(as.character(boxplot$Averted))

### FAMILIES WITH SCHOOL CHILDREN/TEACHERS ###

#Initialize storage vector
vectOBS.sh <- vectCF1.sh <- vectCF2.sh <- vectCF3.sh <- 
  vectCF4.sh <- vectCF5.sh <- vectCF6.sh <- matrix(NA, ncol = ndays, nrow = reps)

#loop through each simulation and capture cumulative symptomatic incindence over time 
#To do this for school households only, need to update N to NschoolHH
#Use incidence in school-associated households only

for(i in 1:reps){
  #extract stored synthetic population from the list of outcomes
  df <- data.frame(outcomes[[i]][8])
  #Identify who are students & teachers & admin
  schoolKid <- df[which(!is.na(df$df.School) &  df$df.Age < 18),]$df.ID
  #Indentify which households students/teachers belong in
  schoolHHs <- df[which(df$df.ID %in% schoolKid),]$df.HH
  #Indentify ID's of individuals belowing to those HH's
  schoolHHID <- df[which(df$df.HH %in% schoolHHs),]$df.ID
  #Determine length (N) of individuals in school-associated households
  NschoolHH <- length(schoolHHID)
  
  #Extract cumulative count of symptomatic cases; convert to cases per 10,000 pop
  vectOBS.sh[i,] <- outcomes[[i]]$OBS$CumSchoolHH_IC/NschoolHH*10000
  vectCF1.sh[i,] <- outcomes[[i]]$CF1$CumSchoolHH_IC/NschoolHH*10000
  vectCF2.sh[i,] <- outcomes[[i]]$CF2$CumSchoolHH_IC/NschoolHH*10000
  vectCF3.sh[i,] <- outcomes[[i]]$CF3$CumSchoolHH_IC/NschoolHH*10000
  vectCF4.sh[i,] <- outcomes[[i]]$CF4$CumSchoolHH_IC/NschoolHH*10000
  vectCF5.sh[i,] <- outcomes[[i]]$CF5$CumSchoolHH_IC/NschoolHH*10000
  vectCF6.sh[i,] <- outcomes[[i]]$CF6$CumSchoolHH_IC/NschoolHH*10000
}

#Calculate the percent increase in cumulative incidence from observed
CF1av.sh <- (vectCF1.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays] #calculate % increase on final day of simulation
CF2av.sh <- (vectCF2.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays]
CF3av.sh <- (vectCF3.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays]
CF4av.sh <- (vectCF4.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays]
CF5av.sh <- (vectCF5.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays]
CF6av.sh <- (vectCF6.sh[,ndays] - vectOBS.sh[,ndays])/vectOBS.sh[,ndays]

#Create dataframe with % increase and intervention name; bind with dataframe of all households
Averted.sh <- c(CF1av.sh, CF2av.sh, CF3av.sh, CF6av.sh)

boxplot.sh <- data.frame(cbind("Averted" = Averted.sh, Int, "Population" = "Households with students"))
boxplot.sh$Averted <- as.numeric(as.character(boxplot.sh$Averted))

boxplot <- rbind(boxplot, boxplot.sh)

#Generate plot C, releveling factor variables

boxplot <- boxplot %>%
  mutate(Int = fct_reorder(Int, Averted, .fun='median'))

boxplot <- boxplot %>%
  mutate(Int = fct_relevel(Int, "Schools open", after = 1))

c <- ggplot(boxplot, aes(x = as.factor(Int), y = Averted*100, fill = Population)) + geom_boxplot(outlier.shape = NA) +
  theme_bw() + ylab("Percent increase in cumulative incidence from observed") + xlab("Counterfactual") +
  scale_fill_brewer(palette = "Paired") + xlab("") +
  theme(text = element_text(size = 10)) + 
  theme(axis.text = element_text(size = 12)) +
  theme(legend.position = c(0.2,0.9),legend.background = element_rect(fill = "transparent")) + 
  geom_hline(aes(yintercept = 0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13))
c

###################################################################
##           Panel B: daily incidence by scenario                ##
###################################################################

#Initialize storage vector for each of the counterfactual scenarios examined
vectOBS <- vectCF1 <- vectCF2 <- vectCF3 <- vectCF4 <- 
  vectCF5 <- vectCF6 <- matrix(NA, ncol = ndays, nrow = reps)

#loop through each simulation and capture daily symptomatic incindence over time 
for(i in 1:reps){
  vectOBS[i,] <- outcomes[[i]]$OBS$TimeSeries_IC
  vectCF1[i,] <- outcomes[[i]]$CF1$TimeSeries_IC
  vectCF2[i,] <- outcomes[[i]]$CF2$TimeSeries_IC
  vectCF3[i,] <- outcomes[[i]]$CF3$TimeSeries_IC
  vectCF4[i,] <- outcomes[[i]]$CF4$TimeSeries_IC
  vectCF5[i,] <- outcomes[[i]]$CF5$TimeSeries_IC
  vectCF6[i,] <- outcomes[[i]]$CF6$TimeSeries_IC
}

#Calculate the daily mean and CI's across the realizations of the model
dataframe <- as.data.frame(cbind("day" = 1:ndays,
                                 "OBS" = colMeans(vectOBS, na.rm = T),
                                 "OBS.low" = colQuantiles(vectOBS, probs = 0.025),
                                 "OBS.high" = colQuantiles(vectOBS, probs = 0.975),
                                 "CF1" = colMeans(vectCF1, na.rm = T),
                                 "CF1.low" = colQuantiles(vectCF1, probs = 0.025),
                                 "CF1.high" = colQuantiles(vectCF1, probs = 0.975),
                                 "CF2" = colMeans(vectCF2, na.rm = T),
                                 "CF2.low" = colQuantiles(vectCF2, probs = 0.025),
                                 "CF2.high" = colQuantiles(vectCF2, probs = 0.975),
                                 "CF3" = colMeans(vectCF3, na.rm = T),
                                 "CF3.low" = colQuantiles(vectCF3, probs = 0.025),
                                 "CF3.high" = colQuantiles(vectCF3, probs = 0.975),
                                 "CF4" = colMeans(vectCF4, na.rm = T),
                                 "CF4.low" = colQuantiles(vectCF4, probs = 0.025),
                                 "CF4.high" = colQuantiles(vectCF4, probs = 0.975),
                                 "CF5" = colMeans(vectCF5, na.rm = T),
                                 "CF5.low" = colQuantiles(vectCF5, probs = 0.025),
                                 "CF5.high" = colQuantiles(vectCF5, probs = 0.975),
                                 "CF6" = colMeans(vectCF6, na.rm = T),
                                 "CF6.low" = colQuantiles(vectCF6, probs = 0.025),
                                 "CF6.high" = colQuantiles(vectCF6, probs = 0.975)))

#Generate date, starting from date of simulation start (Jan 17, roughly 2 weeks before 1st known case)
dataframe$date <- as.Date(dataframe$day, origin = "2020-01-17")

#Generate plot B
b <- ggplot(subset(dataframe, date >= "2020-02-01"), aes(x=date)) + 
  geom_line(aes(y = OBS/N*10000, color = "Observed interventions"), size = 1) +
  geom_line(aes(y = CF1/N*10000, color = "Schools open"), size = 1) +
  geom_line(aes(y = CF2/N*10000, color = "Workplaces open"), size = 1) +
  geom_line(aes(y = CF3/N*10000, color = "Socializing permitted"), size = 1) +
  geom_line(aes(y = CF4/N*10000, color = "Schools + workplaces open"), size = 1) +
  geom_line(aes(y = CF5/N*10000, color = "Schools open + socializing permitted"), size = 1) +
  geom_line(aes(y = CF6/N*10000, color = "No interventions"), size = 1) +
  theme_bw() + ylab("Daily cases per 10,000 population") +
  xlab("Date") + theme(text = element_text(size = 12)) + geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-18")))) +
  theme(axis.text = element_text(size = 15)) +
  labs(color = "Intervention") + scale_color_brewer(palette = "Paired") + xlab("") +
  theme(legend.position = "none") 

b

############################################################################################
##     Panel D: absolute difference in % seropositive by all & school-associated HH's     ##
############################################################################################

### ALL FAMILIES ###

#Initialize storage vectors
vectCF1 <- vectCF2 <- vectCF3 <-vectCF6 <- matrix(NA, ncol = 1, nrow = reps)

#loop through each simulation and calculate difference in seroprevalence on the final day of the simulation
for(i in 1:reps){
  vectCF1[i,] <- (outcomes[[i]]$CF1$CumTotal_I[ndays] - outcomes[[i]]$OBS$CumTotal_I[ndays])/N*100
  vectCF2[i,] <- (outcomes[[i]]$CF2$CumTotal_I[ndays] - outcomes[[i]]$OBS$CumTotal_I[ndays])/N*100
  vectCF3[i,] <- (outcomes[[i]]$CF3$CumTotal_I[ndays] - outcomes[[i]]$OBS$CumTotal_I[ndays])/N*100
  vectCF6[i,] <- (outcomes[[i]]$CF6$CumTotal_I[ndays] - outcomes[[i]]$OBS$CumTotal_I[ndays])/N*100
}

#Create dataframe with % increase and intervention name
Averted <- c(vectCF1, vectCF2, vectCF3, vectCF6)
Int <-c(rep("Schools open", reps),
        rep("Workplaces open", reps),
        rep("Social gatherings permitted", reps),
        rep("No interventions", reps))
boxplot <- data.frame(cbind(Averted, Int, "Population" = "All households"))
boxplot$Averted <- as.numeric(as.character(boxplot$Averted))


### FAMILIES WITH SCHOOL CHILDREN/TEACHERS ###

#Initialize storage vector
vectCF1.sh <- vectCF2.sh <- vectCF3.sh <-vectCF6.sh <- matrix(NA, ncol = 1, nrow = reps)

#loop through each simulation and capture cumulative symptomatic incindence over time 
#To do this for school households only, need to update N to NschoolHH
#Use incidence in school-associated households only

for(i in 1:reps){
  #extract stored synthetic population from the list of outcomes
  df <- data.frame(outcomes[[i]][8])
  #Identify who are students & teachers & admin
  schoolKid <- df[which(!is.na(df$df.School) &  df$df.Age < 18),]$df.ID
  #Indentify which households students/teachers belong in
  schoolHHs <- df[which(df$df.ID %in% schoolKid),]$df.HH
  #Indentify ID's of individuals belowing to those HH's
  schoolHHID <- df[which(df$df.HH %in% schoolHHs),]$df.ID
  #Determine length (N) of individuals in school-associated households
  NschoolHH <- length(schoolHHID)
  
  #Extract absolute difference in seroprevalence on the final day of the simulation
  vectCF1.sh[i,] <- (outcomes[[i]]$CF1$CumSchoolHH_I[139] - outcomes[[i]]$OBS$CumSchoolHH_I[139])/NschoolHH*100
  vectCF2.sh[i,] <- (outcomes[[i]]$CF2$CumSchoolHH_I[139] - outcomes[[i]]$OBS$CumSchoolHH_I[139])/NschoolHH*100
  vectCF3.sh[i,] <- (outcomes[[i]]$CF3$CumSchoolHH_I[139] - outcomes[[i]]$OBS$CumSchoolHH_I[139])/NschoolHH*100
  vectCF6.sh[i,] <- (outcomes[[i]]$CF6$CumSchoolHH_I[139] - outcomes[[i]]$OBS$CumSchoolHH_I[139])/NschoolHH*100
  
}

#Create dataframe with outcome, and bind with above
Averted <- c(vectCF1.sh, vectCF2.sh, vectCF3.sh, vectCF6.sh)
Int <-c(rep("Schools open", reps),
        rep("Workplaces open", reps),
        rep("Social gatherings permitted", reps),
        rep("No interventions", reps))
boxplot.sh <- data.frame(cbind(Averted, Int, "Population" = "Households with students"))
boxplot.sh$Averted <- as.numeric(as.character(boxplot.sh$Averted))

boxplot <- rbind(boxplot, boxplot.sh)

# Generate plot D, releveling factor variables
boxplot <- boxplot %>%
  mutate(Int = fct_reorder(Int, Averted, .fun='median'))

boxplot <- boxplot %>%
  mutate(Int = fct_relevel(Int, "Schools open", after = 1))

d <- ggplot(boxplot, aes(x = as.factor(Int), y = Averted, fill = Population)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() + ylab("Excess seroprevalence (absolute percentage)") + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("") +
  theme(text = element_text(size =10)) + 
  theme(legend.position = c(0.2,0.9),legend.background = element_rect(fill = "transparent")) +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) 

d

####################################################################################
##     Panel E: absolute difference in deaths by all & school-associated HH's     ##
####################################################################################

### ALL FAMILIES ###

#Intialize storage vectors
vectCF1 <- vectCF2 <- vectCF3 <-vectCF6 <- matrix(NA, ncol = 1, nrow = reps)

#Loop through simulations and calculate the difference in total deaths on the final day of the
# simulation for CF1,2,3&6 vs. observed. Convert to per 10,000 pop
for(i in 1:reps){
  vectCF1[i,] <- (outcomes[[i]]$CF1$CumTotal_M[ndays] - outcomes[[i]]$OBS$CumTotal_M[ndays])/N*10000
  vectCF2[i,] <- (outcomes[[i]]$CF2$CumTotal_M[ndays] - outcomes[[i]]$OBS$CumTotal_M[ndays])/N*10000
  vectCF3[i,] <- (outcomes[[i]]$CF3$CumTotal_M[ndays] - outcomes[[i]]$OBS$CumTotal_M[ndays])/N*10000
  vectCF6[i,] <- (outcomes[[i]]$CF6$CumTotal_M[ndays] - outcomes[[i]]$OBS$CumTotal_M[ndays])/N*10000
}

#Create dataframe with outcome
Averted <- c(vectCF1, vectCF2, vectCF3, vectCF6)
Int <-c(rep("Schools open", reps),
        rep("Workplaces open", reps),
        rep("Social gatherings permitted", reps),
        rep("No interventions", reps))
boxplot <- data.frame(cbind(Averted, Int, "Population" = "All households"))
boxplot$Averted <- as.numeric(as.character(boxplot$Averted))


### FAMILIES WITH SCHOOL CHILDREN/TEACHERS ###

#Initialize storage vector
vectCF1.sh <- vectCF2.sh <- vectCF3.sh <-vectCF6.sh <- matrix(NA, ncol = 1, nrow = reps)

#loop through each simulation and capture cumulative deaths per 10,000 pop over time 
#To do this for school households only, need to update N to NschoolHH
#Use deaths in school-associated households only

for(i in 1:reps){
  #extract stored synthetic population from the list of outcomes
  df <- data.frame(outcomes[[i]][8])
  #Identify who are students & teachers & admin
  schoolKid <- df[which(!is.na(df$df.School) &  df$df.Age < 18),]$df.ID
  #Indentify which households students/teachers belong in
  schoolHHs <- df[which(df$df.ID %in% schoolKid),]$df.HH
  #Indentify ID's of individuals belowing to those HH's
  schoolHHID <- df[which(df$df.HH %in% schoolHHs),]$df.ID
  #Determine length (N) of individuals in school-associated households
  NschoolHH <- length(schoolHHID)
  
  #Extract absolute difference in deaths per 10,000 pop on the final day of the simulation
  vectCF1.sh[i,] <- (outcomes[[i]]$CF1$CumSchoolHH_M[139] - outcomes[[i]]$OBS$CumSchoolHH_M[139])/NschoolHH*10000
  vectCF2.sh[i,] <- (outcomes[[i]]$CF2$CumSchoolHH_M[139] - outcomes[[i]]$OBS$CumSchoolHH_M[139])/NschoolHH*10000
  vectCF3.sh[i,] <- (outcomes[[i]]$CF3$CumSchoolHH_M[139] - outcomes[[i]]$OBS$CumSchoolHH_M[139])/NschoolHH*10000
  vectCF6.sh[i,] <- (outcomes[[i]]$CF6$CumSchoolHH_M[139] - outcomes[[i]]$OBS$CumSchoolHH_M[139])/NschoolHH*10000
  
}

#Combine into dataframe and merge with above
Averted <- c(vectCF1.sh, vectCF2.sh, vectCF3.sh, vectCF6.sh)
Int <-c(rep("Schools open", reps),
        rep("Workplaces open", reps),
        rep("Social gatherings permitted", reps),
        rep("No interventions", reps))
boxplot.sh <- data.frame(cbind(Averted, Int, "Population" = "Households with students"))
boxplot.sh$Averted <- as.numeric(as.character(boxplot.sh$Averted))

boxplot <- rbind(boxplot, boxplot.sh)

#Create plot E, relveling factor variables
boxplot <- boxplot %>%
  mutate(Int = fct_reorder(Int, Averted, .fun='median'))

e <- ggplot(boxplot, aes(x = as.factor(Int), y = Averted, fill = Population)) + geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, position=position_dodge(width = 0.75)) +
  theme_bw() + ylab("Excess deaths per 10,000 (absolute)") + xlab("Counterfactual") +
  theme(text = element_text(size =10)) + scale_fill_brewer(palette = "Paired") + xlab("") +
  theme(legend.position = c(0.2,0.9),legend.background = element_rect(fill = "transparent")) + 
  geom_hline(aes(yintercept = 0)) +
  theme(axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 13)) + ylim(c(-3,5))
e


#### Combine plots and save ####

p1 <- ggarrange(a,b, widths = c(1.5,1), labels = c("A", "B"))
p2 <- ggarrange(c,d,e, ncol = 3, widths = c(1,1,1), labels = c("C", "D", "E"))

fin <- ggarrange(p1,p2, nrow = 2)
fin

#save as .jpg or pdf
ggsave(fin, file = "Figure2.ImpactofSpring_SR10.jpg", dpi = 600, width = 18, height = 9.5)

pdf(file = "Figure2.ImpactofSpring_SR10.pdf",   
    width = 18, # The width of the plot in inches
    height = 9.6) # The height of the plot in inches
fin
dev.off()

