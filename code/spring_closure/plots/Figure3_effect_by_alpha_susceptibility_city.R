### This file generates Figure 3 in the manuscript:  
#     Influence of key epidemiological parameters on the effectiveness of school closures###
#         Parameters examined: proportion of households with children < 18 years
#                              ratio of force of infection of asymptomatic cases to symptomatic
#                              susceptibility of children < 20 yrs to SARS-CoV-2 compare to adults >= 20
### 

library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(cowplot)

############################################################################
##   Panel A: effect of school closure by alpha and susceptibility ratio  ##
############################################################################

#Load files that contain output under various assumed alpha and susceptibility ratios
files <- list.files(path = ".\\output\\vary_key_params\\alpha_susceptibilityratio")

for (i in 1:length(files)){
  load(paste0(".\\output\\vary_key_params\\alpha_susceptibilityratio\\", files[i]))
}
load(".\\output\\main_counterfactual_interventions\\SpringSemester_EqualSusceptibility_CF1_6.RData")
A50_SR100 <- outcomes
rm(outcomes)

reps <- length(A25_SR100) #number of simulations
N <- nrow(data.frame(A50_SR100[[1]][8])) #agents
ndays <- length(A25_SR100[[1]]$OBS$CumTotal_I) # days simulation run for

### For each combination of alpha & relative susceptibility, the following code:
# 1) initializes storage vector
# 2) loops through all independent simulations, and calculates for each:
#       a. percent increase in cumulative symptomatic infection per 10,000 pop compared to observed if schools had been open 
#       b. cumulative incidence per 10,000 under "observed" conditions on final day of simulation (June 1)
#       c. cumulative incidence per 10,000 under "schools open" scenario (CF1) on final day of simulation (June 1)

### 1. alpha = 0.25, relative susceptibility = 0.25 ###
A25SR25 <- A25SR25.OBS <-A25SR25.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A25SR25[rep] <- (A25_SR25[[rep]]$CF1$CumTotal_IC[ndays] - A25_SR25[[rep]]$OBS$CumTotal_IC[ndays])/A25_SR25[[rep]]$OBS$CumTotal_IC[ndays]
  A25SR25.OBS[rep] <- A25_SR25[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A25SR25.CF1[rep] <- A25_SR25[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 2. alpha = 0.25, relative susceptibility = 0.50 ###
A25SR50 <- A25SR50.OBS <-A25SR50.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A25SR50[rep] <- (A25_SR50[[rep]]$CF1$CumTotal_IC[ndays] - A25_SR50[[rep]]$OBS$CumTotal_IC[ndays])/A25_SR50[[rep]]$OBS$CumTotal_IC[ndays]
  A25SR50.OBS[rep] <- A25_SR50[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A25SR50.CF1[rep] <- A25_SR50[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 3. alpha = 0.25, relative susceptibility = 1 ###
A25SR100 <- A25SR100.OBS <-A25SR100.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A25SR100[rep] <- (A25_SR100[[rep]]$CF1$CumTotal_IC[ndays] - A25_SR100[[rep]]$OBS$CumTotal_IC[ndays])/A25_SR100[[rep]]$OBS$CumTotal_IC[ndays]
  A25SR100.OBS[rep] <- A25_SR100[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A25SR100.CF1[rep] <- A25_SR100[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 4. alpha = 0.50, relative susceptibility = 0.25 ###
A50SR25 <- A50SR25.OBS <-A50SR25.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A50SR25[rep] <- (A50_SR25[[rep]]$CF1$CumTotal_IC[ndays] - A50_SR25[[rep]]$OBS$CumTotal_IC[ndays])/A50_SR25[[rep]]$OBS$CumTotal_IC[ndays]
  A50SR25.OBS[rep] <- A50_SR25[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A50SR25.CF1[rep] <- A50_SR25[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 5. alpha = 0.50, relative susceptibility = 0.50 ###
A50SR50 <- A50SR50.OBS <-A50SR50.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A50SR50[rep] <- (A50_SR50[[rep]]$CF1$CumTotal_IC[ndays] - A50_SR50[[rep]]$OBS$CumTotal_IC[ndays])/A50_SR50[[rep]]$OBS$CumTotal_IC[ndays]
  A50SR50.OBS[rep] <- A50_SR50[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A50SR50.CF1[rep] <- A50_SR50[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 6. alpha = 0.50, relative susceptibility = 1 ###
A50SR100 <- A50SR100.OBS <-A50SR100.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A50SR100[rep] <- (A50_SR100[[rep]]$CF1$CumTotal_IC[ndays] - A50_SR100[[rep]]$OBS$CumTotal_IC[ndays])/A50_SR100[[rep]]$OBS$CumTotal_IC[ndays]
  A50SR100.OBS[rep] <- A50_SR100[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A50SR100.CF1[rep] <- A50_SR100[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 7. alpha = 0.75, relative susceptibility = 0.25 ###
A75SR25 <- A75SR25.OBS <-A75SR25.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A75SR25[rep] <- (A75_SR25[[rep]]$CF1$CumTotal_IC[ndays] - A75_SR25[[rep]]$OBS$CumTotal_IC[ndays])/A75_SR25[[rep]]$OBS$CumTotal_IC[ndays]
  A75SR25.OBS[rep] <- A75_SR25[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A75SR25.CF1[rep] <- A75_SR25[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 8. alpha = 0.75, relative susceptibility = 0.50 ###
A75SR50 <- A75SR50.OBS <-A75SR50.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A75SR50[rep] <- (A75_SR50[[rep]]$CF1$CumTotal_IC[ndays] - A75_SR50[[rep]]$OBS$CumTotal_IC[ndays])/A75_SR50[[rep]]$OBS$CumTotal_IC[ndays]
  A75SR50.OBS[rep] <- A75_SR50[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A75SR50.CF1[rep] <- A75_SR50[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

### 9. alpha = 0.75, relative susceptibility = 1 ###
A75SR100 <- A75SR100.OBS <-A75SR100.CF1 <-rep(NA, reps)
for (rep in 1:reps){
  A75SR100[rep] <- (A75_SR100[[rep]]$CF1$CumTotal_IC[ndays] - A75_SR100[[rep]]$OBS$CumTotal_IC[ndays])/A75_SR100[[rep]]$OBS$CumTotal_IC[ndays]
  A75SR100.OBS[rep] <- A75_SR100[[rep]]$OBS$CumTotal_IC[ndays]/N*10000
  A75SR100.CF1[rep] <- A75_SR100[[rep]]$CF1$CumTotal_IC[ndays]/N*10000
}

## Create a dataframe of percent increase in incidence from observed (schools closed) if schools had been open
difs <- c(A25SR25, A25SR50, A25SR100, A50SR25, A50SR50, A50SR100, A75SR25, A75SR50, A75SR100)
alpha <- c(rep(.25, 3*reps),rep(.50,3*reps), rep(.75,3*reps))
susceptRatio <- rep(c(rep(0.25, reps), rep(0.50, reps), rep(1, reps)),3)
dataframeA <- data.frame(cbind(difs, alpha, susceptRatio))

## Create a dataframe of total incidence when schools open vs. observed (closed),
# including means and 95% CI's over the simulations
points <- c(mean(A25SR25.OBS), mean(A25SR25.CF1), 
            mean(A25SR50.OBS), mean(A25SR50.CF1), 
            mean(A25SR100.OBS), mean(A25SR100.CF1), 
            mean(A50SR25.OBS), mean(A50SR25.CF1), 
            mean(A50SR50.OBS), mean(A50SR50.CF1), 
            mean(A50SR100.OBS), mean(A50SR100.CF1),  
            mean(A75SR25.OBS), mean(A75SR25.CF1), 
            mean(A75SR50.OBS), mean(A75SR50.CF1), 
            mean(A75SR100.OBS), mean(A75SR100.CF1))  

lower <- c(unname(quantile(A25SR25.OBS, prob = 0.25)), unname(quantile(A25SR25.CF1, prob = 0.25)), 
           unname(quantile(A25SR50.OBS, prob = 0.25)), unname(quantile(A25SR50.CF1, prob = 0.25)),
           unname(quantile(A25SR100.OBS, prob = 0.25)), unname(quantile(A25SR100.CF1, prob = 0.25)),
           unname(quantile(A50SR25.OBS, prob = 0.25)), unname(quantile(A50SR25.CF1, prob = 0.25)), 
           unname(quantile(A50SR50.OBS, prob = 0.25)), unname(quantile(A50SR50.CF1, prob = 0.25)),
           unname(quantile(A50SR100.OBS, prob = 0.25)), unname(quantile(A50SR100.CF1, prob = 0.25)),
           unname(quantile(A75SR25.OBS, prob = 0.25)), unname(quantile(A75SR25.CF1, prob = 0.25)), 
           unname(quantile(A75SR50.OBS, prob = 0.25)), unname(quantile(A75SR50.CF1, prob = 0.25)),
           unname(quantile(A75SR100.OBS, prob = 0.25)), unname(quantile(A75SR100.CF1, prob = 0.25)))

upper <- c(unname(quantile(A25SR25.OBS, prob = 0.75)), unname(quantile(A25SR25.CF1, prob = 0.75)), 
           unname(quantile(A25SR50.OBS, prob = 0.75)), unname(quantile(A25SR50.CF1, prob = 0.75)),
           unname(quantile(A25SR100.OBS, prob = 0.75)), unname(quantile(A25SR100.CF1, prob = 0.75)),
           unname(quantile(A50SR25.OBS, prob = 0.75)), unname(quantile(A50SR25.CF1, prob = 0.75)), 
           unname(quantile(A50SR50.OBS, prob = 0.75)), unname(quantile(A50SR50.CF1, prob = 0.75)),
           unname(quantile(A50SR100.OBS, prob = 0.75)), unname(quantile(A50SR100.CF1, prob = 0.75)),
           unname(quantile(A75SR25.OBS, prob = 0.75)), unname(quantile(A75SR25.CF1, prob = 0.75)), 
           unname(quantile(A75SR50.OBS, prob = 0.75)), unname(quantile(A75SR50.CF1, prob = 0.75)),
           unname(quantile(A75SR100.OBS, prob = 0.75)), unname(quantile(A75SR100.CF1, prob = 0.75)))


dataframeB <- data.frame(cbind(points, lower, upper))
dataframeB$alpha <- alpha <- c(rep(.25, 6),rep(.50,6), rep(.75,6))
dataframeB$susceptRatio <- rep(c(0.25, 0.25, 0.50, 0.50, 1, 1),3)
dataframeB$School <- rep(c("Closed", "Open"),9)


# Generate Panel A, showing differences in cumulative incidence when 
# schools open vs. closed by varying alpha and relative susceptibility
medSC <- median(dataframeA[which(dataframeA$alpha == 0.50 & dataframeA$susceptRatio == 0.50),]$difs)
quantile(dataframeA[which(dataframeA$alpha == 0.25 & dataframeA$susceptRatio == 0.25),]$difs, probs = c(0.025, 0.975))
mean(dataframeA[which(dataframeA$alpha == 0.25 & dataframeA$susceptRatio == 0.25),]$difs)

paired.palette <- brewer.pal(8, "Paired")
a <- ggplot(dataframeA) +
  geom_hline(aes(yintercept = 119.1), linetype = "dotted") +
  geom_hline(aes(yintercept = 49.6), linetype = "dotted") +
  geom_boxplot(aes(x = as.factor(susceptRatio), y = difs*100, fill = as.factor(alpha)), outlier.shape = NA) +
  geom_hline(aes(yintercept = 0)) +
  theme_bw() + theme(text = element_text(size = 12), axis.text = element_text(size = 13)) + 
  labs(fill = "Pr(asymptomatic transmission)") +
  xlab("Relative susceptibility of children <20 years vs. adults 20+") + 
  ylab("Percent increase in cumulative incidence from observed") +
  geom_text(x = 0.9, y=119+10, label="Workplaces open", size = 3.5) +
  geom_text(x = 1.1, y=50+10, label="Social gatherings permitted", size = 3.5) +
  guides(fill = guide_legend(title = expression(alpha))) +
  scale_fill_manual(values = c(paired.palette[2], paired.palette[4], paired.palette[8]))

a

###################################################################
##          Panel B: Effect of proportion HH with children       ##
###################################################################

#load saved simulations
load(".\\output\\vary_key_params\\proportion_children\\SpringSemester_Berkeley_CF1.RData")
load(".\\output\\vary_key_params\\proportion_children\\SpringSemester_Hayward_CF1.RData")
load(".\\output\\main_counterfactual_interventions\\SpringSemester_Less10HalfSusceptible_CF1_6.RData")
Oakland <- outcomes
rm(outcomes)

reps <- length(Oakland) #number of simulations
N <- nrow(data.frame(Oakland[[1]][8])) #agents
ndays <- length(Oakland[[1]]$OBS$CumTotal_I) # days simulation run for

### For each "city", the following code:
# 1) initializes storage vector
# 2) loops through all independent simulations, and calculates
#     percent increase in cumulative symptomatic infection per 10,000 pop compared to observed if schools had been open 
###


## 1. Hayward (Households with children = 34.4%) ##
HayWDif <- rep(NA, reps)
for (rep in 1:reps){
  HayWDif[rep] <- (Hayward[[rep]]$CF1$CumTotal_IC[ndays] - Hayward[[rep]]$OBS$CumTotal_IC[ndays])/Hayward[[rep]]$OBS$CumTotal_IC[ndays]
}
 
## 2. Berkeley (Households with children = 16.9%) ##
BerkDif <- rep(NA, reps)
for (rep in 1:reps){
  BerkDif[rep] <- (Berkeley[[rep]]$CF1$CumTotal_IC[ndays] - Berkeley[[rep]]$OBS$CumTotal_IC[ndays])/Berkeley[[rep]]$OBS$CumTotal_IC[ndays]
}

## 3. Oakland (Households with children = 25.1%) ##
OakDif <- rep(NA, reps)
for (rep in 1:reps){
  OakDif[rep] <- (Oakland[[rep]]$CF1$CumTotal_IC[ndays] - Oakland[[rep]]$OBS$CumTotal_IC[ndays])/Oakland[[rep]]$OBS$CumTotal_IC[ndays]
}

#Combine results into single dataframe
dataframeB <- data.frame(cbind("Dif" = c(BerkDif, OakDif, HayWDif), 
                               "propKids" <- c(rep(16.9,reps), rep(25.1,reps), rep(34.4,reps))))
dataframeB$City <- c(rep("Berkeley",reps), rep("Oakland",reps), rep("Hayward",reps))

##plot

b <- ggplot(dataframeB, aes(x = propKids, y = Dif*100, fill = City)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_hline(aes(yintercept = 0))+
  theme_bw() + theme(text = element_text(size = 12), axis.text = element_text(size = 13)) + 
  labs(fill = "City") +
  xlab("Percent of households with children <18 years") + 
  ylab("Percent increase in cumulative incidence from observed") +
  scale_fill_manual(breaks = c("Hayward", "Oakland", "Berkeley"),
                    values = c(paired.palette[5], paired.palette[6], paired.palette[7]))
b

#### Combine plots and save ####

fin <- ggdraw() +
  draw_plot(a, x=0, y = 0, width = 0.5, height = 0.95) +
  draw_plot(b, x = 0.5, y = 0, width = 0.5, height = 0.95) +
  draw_plot_label(c("A", "B"), c(0,0.5), c(1,1), size = 15)
  
fin

#save as .jpg or pdf
ggsave(fin, file = "Figure3_Sensitivity_City_Alpha_SR.jpg", dpi = 600, width = 12, height = 5)

pdf(file = "Figure3_Sensitivity_City_Alpha_SR.pdf", width = 12, height = 5)
fin
dev.off()

