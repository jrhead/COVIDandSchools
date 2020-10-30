## Generates Figure S3 in the manuscript ##

source(".\\code\\synth_pop.R")
source(".\\code\\contact_matrix.R") #make contact matrices
source(".\\code\\model_functions.R")

library(dplyr)
library(ggpubr)
library(gridExtra)

###################################################################
#                                                                 #
#                   PLOTTING FUNCTIONS                            #
#                                                                 #
###################################################################

#function to generate a plot of a age-stratified contact matrix
plotCM <- function(contact_list, synth_pop_df, AgeCut, legend = TRUE){
  
  matrix <- matrix(NA, nrow = nage, ncol = nage)
  
  for(i in 1:nage){
    id_i <- as.numeric(as.character(synth_pop_df[synth_pop_df$AgeCut == i,"ID"]))
    for (j in 1:nage){
      id_j <- as.numeric(as.character(synth_pop_df[synth_pop_df$AgeCut == j,"ID"]))
      subm <- contact_list[id_i,id_j]
      summ <- rowSums(subm)
      matrix[i,j] <- mean(summ)
    }
  }
  
  matrix_p <- apply(matrix, 1:2, function(x)  min(x,5))
  
  df <- melt(matrix_p, varnames = c("age1", "age2"), value.name = "contacts")
  
  
  my_breaks = c(0, 1, 2, 3, 4)
  
  plot <- ggplot(df, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
    geom_tile() + scale_fill_gradient2(low = "white", high = "midnightblue", name = "Count", breaks = my_breaks,
                                       labels = c("0", "1" ,"2", "3", "4+"), limits = c(0,5)) + theme_bw() +
    scale_x_continuous("Age of individual", breaks = c(1,2,3,4,5,6,7),
                       labels = c("0-4", "5-10", "11-13", "14-17", "18-39","40-65", "65+")) +
    scale_y_continuous("Age of contact", breaks = c(1,2,3,4,5,6,7),
                       labels = c("0-4", "5-10", "11-13", "14-17", "18-39","40-65", "65+")) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  if(legend == FALSE){
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}

#function to extract legend from a plot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
###################################################################

###################################################################
#                                                                 #
#                   LOAD SURVEY DATA                              #
#                                                                 #
###################################################################
#Load community matrices from survey data
load(".\\data\\survey_contact_data\\community_matrix_060820.RData")
comm_survey <- mean_comm_contacts
rm(mean_comm_contacts)
load(".\\data\\survey_contact_data\\work_060820.RData")
work_survey <- mean_loc2_noiptw
rm(mean_loc2_noiptw)
load(".\\data\\survey_contact_data\\childcare_060820.RData")
daycare_survey <- mean_loc4_noiptw
rm(mean_loc4_noiptw)
load(".\\data\\survey_contact_data\\transit_060820.RData")
transit_survey <- mean_loc7_noiptw
rm(mean_loc7_noiptw)

#Load the polymod data for the counterfactual situations
load(".\\data\\POLYMOD_contact_data\\polymod_dat.CF6.RData")
load(".\\data\\POLYMOD_contact_data\\polymod_dat.CF2_4.RData")
load(".\\data\\POLYMOD_contact_data\\polymod_dat.CF3_5.RData")

###CF1: FullCommunity_survey - LocDaycare_survey
CF1.comm <- comm_survey - daycare_survey
###CF2: FullCommunity_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
CF2.comm <- comm_survey - work_survey - transit_survey + polymod_dat.CF2_4
###CF3: [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey
CF3.comm <- polymod_dat.CF3_5 + work_survey + transit_survey
###CF4: FullCommunity_survey - LocDaycare_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
CF4.comm <- comm_survey - daycare_survey - work_survey - transit_survey + polymod_dat.CF2_4
###CF5: [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey - LocDaycare_survey
CF5.comm <- polymod_dat.CF3_5 + work_survey + transit_survey + daycare_survey
###CF6 is polymod_dat.CF6 -- which is the default from the original contact matrix generation!
#CF6.comm <- polymod_dat.CF6
###Obs is comm_survey
Obs.comm <- comm_survey - work_survey

###################################################################

###################################################################
#                                                                 #
#                   CREATE SYNTHETIC POPULATION                   #
#                                                                 #
###################################################################
#Population parameters
N =16000  
numCommunity <- 1 #numCommunity must be > any one school...
Nschools <- list(Elementary = 3, Middle = 1, High = 1)
avgHHsize <- 2.5
avgFamsize <- 3.3
MedAge <- 33
propFamHH <- 0.544
propKidHH <- 0.251
propSingleParent <- 0.40
propGrandparent <- 0.03
propMultigen <- 0.04
prop65p <- 0.111
propUnemployed <- 0.22 #unemployed (6%) + work from home -- 3.9 million worker base + college
NWork <- ((1-propUnemployed)*N/2)/20
ClassSize = 20

polymod_dat <- readRDS(".\\data\\POLYMOD_contact_data\\polymod_dat.RData")
matrix <- as.matrix(polymod_dat$matrix)

#Generate synthetic population
synth_pop_df <- synth_pop(N=N, avgHHsize, avgFamsize, numCommunity, MedAge, propKidHH, propFamHH, propUnemployed, 
                          propSingleParent, propGrandparent, propMultigen, prop65p, NWork, Nschools, ClassSize)

#extract # per age category
Age_Vect <- c(sum(synth_pop_df$AgeCat == "Under5"),
              sum(synth_pop_df$AgeCat == "5-10"),
              sum(synth_pop_df$AgeCat == "11-13"),
              sum(synth_pop_df$AgeCat == "14-17"),
              sum(synth_pop_df$AgeCat == "18-64"),
              sum(synth_pop_df$AgeCat == "65+"))

# For constant contact rate between age groups
AgeContRates <- matrix(c(
  matrix[1,]/Age_Vect[1],
  matrix[2,]/Age_Vect[2],
  matrix[3,]/Age_Vect[3],
  matrix[4,]/Age_Vect[4],
  matrix[5,]/Age_Vect[5],
  matrix[6,]/Age_Vect[6]), ncol = 6)

rownames(AgeContRates) <- c("Under5", "5-10", "11-13", "14-17", "18-64", "65+")

#define new age cats
synth_pop_df$Age <- as.numeric(as.character(synth_pop_df$Age))
synth_pop_df <- synth_pop_df %>% mutate(AgeCat2 = ifelse(Age < 5, 1,
                                                         ifelse(Age>=5 & Age < 13, 2,
                                                                ifelse(Age >=13 & Age < 18, 3,
                                                                       ifelse(Age >= 18 & Age <40, 4,
                                                                              ifelse(Age >=40 & Age < 65,5,6))))))
Age_Vect2 <- c(sum(synth_pop_df$AgeCat2 == 1),
               sum(synth_pop_df$AgeCat2 == 2),
               sum(synth_pop_df$AgeCat2 == 3),
               sum(synth_pop_df$AgeCat2 == 4),
               sum(synth_pop_df$AgeCat2 == 5),
               sum(synth_pop_df$AgeCat2 == 6))

synth_pop_df$Age <- as.numeric(as.character(synth_pop_df$Age))
synth_pop_df$AgeCatN <- as.character(synth_pop_df$AgeCat)
synth_pop_df$AgeCatN <- ifelse(synth_pop_df$Age >= 18 & synth_pop_df$Age <= 39, "18-39",as.character(synth_pop_df$AgeCat))
synth_pop_df$AgeCatN <- ifelse(synth_pop_df$Age >= 40 & synth_pop_df$Age <= 64, "40-64",as.character(synth_pop_df$AgeCat))


synth_pop_df$AgeCut <- cut(as.numeric(synth_pop_df$Age), 
                           breaks = c(0, 5, 11, 14, 19, 40, 65, 200),
                           labels = 1:7, right = T)

nage <- length(unique(synth_pop_df$AgeCut))

###################################################################

###################################################################
#                                                                 #
#                   DEFINE CONTACT MATRICES                       #
#                                                                 #
###################################################################

#define contact matrices by location under original (pre-SIP) conditions
contacts_orig_list <- Rcontact_matrix(synth_pop_df, 
                                      HHcontact = 1, 
                                      SCHcontact = .2/7,
                                      WORKcontact = 5/7 ,
                                      GRADEcontact = 1/7-.2/7, 
                                      CLASScontact = 5/7- 1/7-.2/7, 
                                      AgeContRates,
                                      propEssential=0.28,
                                      NWork)

##define matrix for when only essential work occurs
EWrk.cont <- contacts_orig_list[[7]]
EssentialWorkMat <- matrix(0, ncol = N, nrow = N)
for (i in 1:dim(EWrk.cont)[1]){
  EssentialWorkMat[EWrk.cont[i,1],EWrk.cont[i,2]] <- 5/7
  EssentialWorkMat[EWrk.cont[i,2],EWrk.cont[i,1]] <- 5/7
}
EssentialWorkMatsp <- as(EssentialWorkMat, "sparseMatrix")
rm(EssentialWorkMat)

###################################################################

###################################################################
#                                                                 #
#                    MAKE THE PLOTS                               #
#                                                                 #
###################################################################

#functions to remove x & Y axis

clean_axes <- theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank()
        )

clean_x.axis <- theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank()
)

clean_y.axis <- theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank()
)

lay <- rbind(c(rep(1,13),
               rep(2,10),
               rep(3,10),
               rep(4,10),
               rep(5,10),
               rep(6,2)))
### 1. Observed

###1: OBSERVED: Schools + Socializing  + workplaces CLOSED
OBS_contact_list <- contacts_orig_list
OBS_contact_list[[2]] <- EssentialWorkMatsp
OBS_contact_list[[3]] <- 0*OBS_contact_list[[3]] #School
OBS_contact_list[[4]] <- 0*OBS_contact_list[[4]] #Grade
OBS_contact_list[[5]] <- 0*OBS_contact_list[[5]] #Class
OBS_contact_list[[6]] <- gen_COMmat(Obs.comm) #Community

OBS_contact_list[[1]] <- as.matrix(OBS_contact_list[[1]])
OBS_contact_list[[2]] <- as.matrix(OBS_contact_list[[2]])
OBS_contact_list[[3]] <- as.matrix(OBS_contact_list[[3]])
OBS_contact_list[[4]] <- as.matrix(OBS_contact_list[[4]])
OBS_contact_list[[5]] <- as.matrix(OBS_contact_list[[5]])
OBS_contact_list[[6]] <- as.matrix(OBS_contact_list[[6]])

sum <- OBS_contact_list[[1]] + OBS_contact_list[[2]] + OBS_contact_list[[3]] +
  OBS_contact_list[[4]] + OBS_contact_list[[5]] + OBS_contact_list[[6]]


t <- plotCM(sum, synth_pop_df, AgeCut, legend = FALSE)
rm(sum)

HH <- plotCM(OBS_contact_list[[1]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_y.axis

SCH <- plotCM(OBS_contact_list[[3]] + OBS_contact_list[[4]] + OBS_contact_list[[5]], 
              synth_pop_df, AgeCut, legend = FALSE) + 
  clean_y.axis

WORK <-  plotCM(OBS_contact_list[[2]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_y.axis

COM <- plotCM(OBS_contact_list[[6]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_y.axis

text = "Observed     "

Obs.plot <- grid.arrange(t,HH,SCH,WORK,COM,text_grob(text, rot = -90), layout_matrix = lay)

#save space
rm(OBS_contact_list) 
rm(t)
rm(HH)
rm(SCH)
rm(WORK)
rm(COM)

### 2. CF1: Schools open

CF1_contact_list <- contacts_orig_list
CF1_contact_list[[2]] <- EssentialWorkMatsp #Work
CF1_contact_list[[6]] <- gen_COMmat(CF1.comm) #Community

CF1_contact_list[[1]] <- as.matrix(CF1_contact_list[[1]])
CF1_contact_list[[2]] <- as.matrix(CF1_contact_list[[2]])
CF1_contact_list[[3]] <- as.matrix(CF1_contact_list[[3]])
CF1_contact_list[[4]] <- as.matrix(CF1_contact_list[[4]])
CF1_contact_list[[5]] <- as.matrix(CF1_contact_list[[5]])
CF1_contact_list[[6]] <- as.matrix(CF1_contact_list[[6]])

sum <- CF1_contact_list[[1]] + CF1_contact_list[[2]] + CF1_contact_list[[3]] +
  CF1_contact_list[[4]] + CF1_contact_list[[5]] + CF1_contact_list[[6]]


t <- plotCM(sum, synth_pop_df, AgeCut, legend = FALSE) + 
  clean_x.axis
rm(sum)

HH <- plotCM(CF1_contact_list[[1]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

SCH <- plotCM(CF1_contact_list[[3]] + CF1_contact_list[[4]] + CF1_contact_list[[5]], 
              synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

WORK <-  plotCM(CF1_contact_list[[2]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

COM <- plotCM(CF1_contact_list[[6]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

text = "Schools open"

CF1.plot <- grid.arrange(t,HH,SCH,WORK,COM,text_grob(text, rot = -90), layout_matrix = lay)

rm(CF1_contact_list) #save space
rm(t)
rm(HH)
rm(SCH)
rm(WORK)
rm(COM)

### 2. CF2: Work open
CF2_contact_list <- contacts_orig_list
CF2_contact_list[[3]] <- 0*CF2_contact_list[[3]] #School
CF2_contact_list[[4]] <- 0*CF2_contact_list[[4]] #Grade
CF2_contact_list[[5]] <- 0*CF2_contact_list[[5]] #Class
CF2_contact_list[[6]] <- gen_COMmat(CF2.comm) #Community -- do NOT include work, DO add public transit (polymod)

CF2_contact_list[[1]] <- as.matrix(CF2_contact_list[[1]])
CF2_contact_list[[2]] <- as.matrix(CF2_contact_list[[2]])
CF2_contact_list[[3]] <- as.matrix(CF2_contact_list[[3]])
CF2_contact_list[[4]] <- as.matrix(CF2_contact_list[[4]])
CF2_contact_list[[5]] <- as.matrix(CF2_contact_list[[5]])
CF2_contact_list[[6]] <- as.matrix(CF2_contact_list[[6]])

sum <- CF2_contact_list[[1]] + CF2_contact_list[[2]] + CF2_contact_list[[3]] +
  CF2_contact_list[[4]] + CF2_contact_list[[5]] + CF2_contact_list[[6]]


t <- plotCM(sum, synth_pop_df, AgeCut, legend = FALSE) + 
  clean_x.axis
rm(sum)

HH <- plotCM(CF2_contact_list[[1]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

SCH <- plotCM(CF2_contact_list[[3]] + CF2_contact_list[[4]] + CF2_contact_list[[5]], 
              synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

WORK <-  plotCM(CF2_contact_list[[2]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

COM <- plotCM(CF2_contact_list[[6]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

text = "Work open"

CF2.plot <- grid.arrange(t,HH,SCH,WORK,COM,text_grob(text, rot = -90), layout_matrix = lay)

rm(CF2_contact_list) #save space
rm(t)
rm(HH)
rm(SCH)
rm(WORK)
rm(COM)

### 3. CF3: No social distancing
CF3_contact_list <- contacts_orig_list
CF3_contact_list[[2]] <- EssentialWorkMatsp 
CF3_contact_list[[3]] <- 0*CF3_contact_list[[3]] #School 
CF3_contact_list[[4]] <- 0*CF3_contact_list[[4]] #Grade
CF3_contact_list[[5]] <- 0*CF3_contact_list[[5]]
  CF3_contact_list[[6]] <- gen_COMmat(CF3.comm) + 
  2/20*contacts_orig_list[[2]] +
  2/20*contacts_orig_list[[5]] #still see some work and school contacts outside class and work


CF3_contact_list[[1]] <- as.matrix(CF3_contact_list[[1]])
CF3_contact_list[[2]] <- as.matrix(CF3_contact_list[[2]])
CF3_contact_list[[3]] <- as.matrix(CF3_contact_list[[3]])
CF3_contact_list[[4]] <- as.matrix(CF3_contact_list[[4]])
CF3_contact_list[[5]] <- as.matrix(CF3_contact_list[[5]])
CF3_contact_list[[6]] <- as.matrix(CF3_contact_list[[6]])

sum <- CF3_contact_list[[1]] + CF3_contact_list[[2]] + CF3_contact_list[[3]] +
  CF3_contact_list[[4]] + CF3_contact_list[[5]] + CF3_contact_list[[6]]


t <- plotCM(sum, synth_pop_df, AgeCut, legend = FALSE) + 
  clean_x.axis
rm(sum)

HH <- plotCM(CF3_contact_list[[1]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

SCH <- plotCM(CF3_contact_list[[3]] + CF3_contact_list[[4]] + CF3_contact_list[[5]], 
              synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

WORK <-  plotCM(CF3_contact_list[[2]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

COM <- plotCM(CF3_contact_list[[6]], synth_pop_df, AgeCut, legend = FALSE) + 
  clean_axes

text = "No social distancing"

CF3.plot <- grid.arrange(t,HH,SCH,WORK,COM,text_grob(text, rot = -90), layout_matrix = lay)

rm(CF3_contact_list) #save space
rm(t)
rm(HH)
rm(SCH)
rm(WORK)
rm(COM)


### 6. No interventions
contacts_orig_list[[1]] <- as.matrix(contacts_orig_list[[1]])
contacts_orig_list[[2]] <- as.matrix(contacts_orig_list[[2]])
contacts_orig_list[[3]] <- as.matrix(contacts_orig_list[[3]])
contacts_orig_list[[4]] <- as.matrix(contacts_orig_list[[4]])
contacts_orig_list[[5]] <- as.matrix(contacts_orig_list[[5]])
contacts_orig_list[[6]] <- as.matrix(contacts_orig_list[[6]])

sum <- contacts_orig_list[[1]] + contacts_orig_list[[2]] + contacts_orig_list[[3]] +
  contacts_orig_list[[4]] + contacts_orig_list[[5]] + contacts_orig_list[[6]]

#Get legend
t <- plotCM(sum, synth_pop_df, AgeCut)
legend <- get_legend(t)

t <- plotCM(sum, synth_pop_df, AgeCut, legend = FALSE) + 
  ggtitle("Total") + clean_x.axis
rm(sum)

HH <- plotCM(contacts_orig_list[[1]], synth_pop_df, AgeCut, legend = FALSE) + 
  ggtitle("Household") + clean_axes

SCH <- plotCM(contacts_orig_list[[3]] + contacts_orig_list[[4]] + contacts_orig_list[[5]], 
              synth_pop_df, AgeCut, legend = FALSE) + 
  ggtitle("School ") + clean_axes

WORK <-  plotCM(contacts_orig_list[[2]], synth_pop_df, AgeCut, legend = FALSE) + 
  ggtitle("Work") + clean_axes

COM <- plotCM(contacts_orig_list[[6]], synth_pop_df, AgeCut, legend = FALSE) + 
  ggtitle("Community") + clean_axes


text = "No interventions"


NoInt <- grid.arrange(t,HH,SCH,WORK,COM,text_grob(text, rot = -90), layout_matrix = lay)

rm(contacts_orig_list) #save space
rm(t)
rm(HH)
rm(SCH)
rm(WORK)
rm(COM)

#############################################
###           COMBINE PLOTS               ###
#############################################

lay2 <- cbind(c(rep(1,6),
               rep(2,5),
               rep(3,5),
               rep(4,5),
               rep(5,7)))

Final.plot <- grid.arrange(NoInt, CF1.plot, CF2.plot, CF3.plot, Obs.plot, ncol = 1, layout_matrix = lay2)

lay3 <- rbind(c(rep(1,10), rep(2,1)))
Final.plot <- grid.arrange(Final.plot, legend, layout_matrix = lay3)

ggsave(Final.plot, file = "ContactMatricesInterventions.jpg", dpi = 600, width = 10, height = 8)
