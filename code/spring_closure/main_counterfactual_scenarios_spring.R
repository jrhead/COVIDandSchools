# Main Code File to:
# 1. Create the synthetic population
# 2. Create contact matrices for certain counterfactial (CF) situations
# 3. Run the SEIR model under certain counterfactual situations
# 4. Summarize and save the output

##############  Community Contact Matrices for Each CF Situation #############
### CF1: Schools stay OPEN
###### Matrix = FullCommunity_survey - LocDaycare_survey
###
### CF2: Work OPEN 
###### Matrix = FullCommunity_survey - LocWork_survey - LocPubTrans_survey + PubTrans_polymod
###### Matrix = FullCommunity_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
###
### CF3: Socializing OPEN 
###### Matrix = Community_polymod - (School_polymod + Work_polymod + PubTrans_polymod) + LocWork_survey + LocPubTrans_survey
###### Matrix = [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey
###
### CF4: Schools + Work OPEN
###### Matrix = FullCommunity_survey - LocDaycare_survey - LocWork_survey - LocPubTrans_survey + PubTrans_polymod
###### Matrix = FullCommunity_survey - LocDaycare_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
###
### CF5: Schools + Socializing OPEN
###### Matrix = Community_polymod - (School_polymod + Work_polymod + PubTrans_polymod) + 
#                                             LocWork_survey + LocPubTrans_survey - LocDaycare_survey
###### Matrix = [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey - LocDaycare_survey
###
### CF6: Schools + Work + Socializing OPEN
###### Matrix = Community_polymod - (School_polymod + Work_polymod)
###### Matrix = [polymod_dat.CF6]
###
### OBS: Schools + Work + Socializing CLOSED
###### Matrix = FullCommunity_survey
######################################################################################

# MODEL PARAMETERS VARIED IN ANALYSES (see Fig. 3)
propKidHH <- 0.251 # proportion of HH with children <18; varied between 0.344 (Hayward) and 0.169 (Berkeley) for analysis by city
alpha = 0.5 #ratio of asymptomatic to symptomatic transmission; varied between 0.25 to 1
susceptRatio = 0.5 #ratio of the susceptibility of children < (susceptAgeSplit) to SARS-CoV-2 vs. adults; varied between 0.25 and 1
susceptAgeSplit = 10 #age at which suscpetibility changes according to the ratio above; agents < 10 assumed half as susceptible (varied between 10 yr and 20 yr)

#Source needed functions
source(".\\code\\spring_closure\\synth_pop.R") #make synthetic pop
source(".\\code\\spring_closure\\contact_matrix.R") #make contact matrices
source(".\\code\\spring_closure\\model_functions.R") #functions needed for SEIR


#load libraries needed to run simulatons run in parallel
library(doParallel)
library(foreach)

##############  1. Community Contact Matrices for Each CF Situation #############
### CF1: Schools stay OPEN
###### Matrix = FullCommunity_survey - LocDaycare_survey
###
### CF2: Work OPEN 
###### Matrix = FullCommunity_survey - LocWork_survey - LocPubTrans_survey + PubTrans_polymod
###### Matrix = FullCommunity_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
###
### CF3: Socializing OPEN 
###### Matrix = Community_polymod - (School_polymod + Work_polymod + PubTrans_polymod) + LocWork_survey + LocPubTrans_survey
###### Matrix = [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey
###
### CF4: Schools + Work OPEN
###### Matrix = FullCommunity_survey - LocDaycare_survey - LocWork_survey - LocPubTrans_survey + PubTrans_polymod
###### Matrix = FullCommunity_survey - LocDaycare_survey - LocWork_survey - LocPubTrans_survey + (polymod_dat.CF2_4)
###
### CF5: Schools + Socializing OPEN
###### Matrix = Community_polymod - (School_polymod + Work_polymod + PubTrans_polymod) + 
#                                             LocWork_survey + LocPubTrans_survey - LocDaycare_survey
###### Matrix = [polymod_dat.CF3_5] + LocWork_survey + LocPubTrans_survey - LocDaycare_survey
###
### CF6: Schools + Work + Socializing OPEN
###### Matrix = Community_polymod - (School_polymod + Work_polymod)
###### Matrix = [polymod_dat.CF6]
###
### OBS: Schools + Work + Socializing CLOSED
###### Matrix = FullCommunity_survey
######################################################################################

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
polymod_dat <- readRDS(".\\data\\POLYMOD_contact_data\\polymod_dat.RData")
matrix <- as.matrix(polymod_dat$matrix)

#Generate the community matrices for each CF situation
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

######################################################################
######   2. Create the synthetic population ##########################
######################################################################
##Population parameters (see supplement for sources)
N =16000  
numCommunity <- 1 
Nschools <- list(Elementary = 3, Middle = 1, High = 1)
avgHHsize <- 2.5
avgFamsize <- 3.3
MedAge <- 33
propFamHH <- 0.544
propSingleParent <- 0.40
propGrandparent <- 0.03
propMultigen <- 0.04
prop65p <- 0.111
propUnemployed <- 0.22 # unemployed (6%) + work from home -- 3.9 million worker base + college
NWork <- ((1-propUnemployed)*N/2)/20
ClassSize = 20

## Model parameters (see manuscript table 1)
R0_init = 2.5 #basic reproduction number

##Waiting times
#E --> I; I --> R or D; I --> H; H --> R or D
shapeEI = 4; scaleEI = 6
shapeIR = 7; scaleIR = 14
shapeIH = 7; scaleIH = 11
shapeHRD = 13; scaleHRD = 15

##set up parallel processing
nCores <- 5
#nCores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
registerDoParallel(nCores)
set.seed(123)

######################################################################
######   3. Run simulations for each CF scenario #####################
######################################################################

#Run all 6 situations 1000 times, in parallel

outcomes <- foreach(reps = 1:1000, .packages = "dplyr") %dopar% {
  
  ################## 0. PREP BY CREATING SYNTHETIC POPULATION AND THEIR FATES ################################
  #Generate synetic population
  synth_pop_df <- synth_pop(N=N, avgHHsize, avgFamsize, numCommunity, MedAge, propKidHH, propFamHH, propUnemployed, 
                            propSingleParent, propGrandparent, propMultigen, prop65p, NWork, Nschools, ClassSize)
  

  #Define number in each age group
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
  
  #define new age categories
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
  
  
  #Define original contact matrix under a no intervention situation
  contacts_orig_list <- Rcontact_matrix(synth_pop_df, 
                                        HHcontact = 1, 
                                        SCHcontact = .2/7,
                                        WORKcontact = 5/7 ,
                                        GRADEcontact = 1/7-.2/7, 
                                        CLASScontact = 5/7- 1/7-.2/7, 
                                        AgeContRates,
                                        propEssential=0.28,
                                        NWork)
  
  
  #calculate eigenvalue of contact matrix (used in calculation of beta below)
  #expressed as the weighted mean of total contacts (mean # of contacts from someone who has just been contacted)
  numCont <- rowSums(contacts_orig_list[[1]]) + 
    rowSums(contacts_orig_list[[2]]) + 
    rowSums(contacts_orig_list[[3]]) + 
    rowSums(contacts_orig_list[[4]]) + 
    rowSums(contacts_orig_list[[5]]) + 
    rowSums(contacts_orig_list[[6]])
  MAXEIG <- weighted.mean(numCont, w = numCont)
  
  #Define conditional probabilities for clinical outcomes 
  #rho: prob infection is clinical
  rho = rep(0.69, N) #prob case is clinical for under 18 is 21%, 69% for those older...
  rho[synth_pop_df$AgeCat == "Under5" | synth_pop_df$AgeCat == "5-10" |
        synth_pop_df$AgeCat == "11-13"| synth_pop_df$AgeCat == "14-17"] <- 0.21
  
  synth_pop_df$Age <- as.numeric(as.character(synth_pop_df$Age))
  
  #Define conditional probability of hospitalization
  h_rate <- rep(NA, N) #hospitalization rates
  #SOURCE: https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext
  h_rate[synth_pop_df$Age <10] <- 0.00001
  h_rate[synth_pop_df$Age >= 10 & synth_pop_df$Age < 20] <- 0.000408
  h_rate[synth_pop_df$Age >= 20 & synth_pop_df$Age < 30] <- 0.0104
  h_rate[synth_pop_df$Age >= 30 & synth_pop_df$Age < 40] <- 0.0343
  h_rate[synth_pop_df$Age >= 40 & synth_pop_df$Age < 50] <- 0.0425
  h_rate[synth_pop_df$Age >= 50 & synth_pop_df$Age < 60] <- 0.0816
  h_rate[synth_pop_df$Age >= 60 & synth_pop_df$Age < 70] <- 0.118
  h_rate[synth_pop_df$Age >= 70 & synth_pop_df$Age < 80] <- 0.166
  h_rate[synth_pop_df$Age >= 80] <- 0.184
  
  #define conditional probability of death
  mu = rep(NA, N) #death rates, among those hospitalized
  #SOURCE: https://www.bmj.com/content/369/bmj.m1923
  mu[synth_pop_df$Age <10] <- 0.017#0.5*(0.024 + 0.017)
  mu[synth_pop_df$Age >= 10 & synth_pop_df$Age < 20] <- 0.017#0.5*(0.024 + 0.017)
  mu[synth_pop_df$Age >= 20 & synth_pop_df$Age < 30] <- 0.026#0.5*(0.036 + 0.026)
  mu[synth_pop_df$Age >= 30 & synth_pop_df$Age < 40] <- 0.036#0.5*(0.059 + 0.036)
  mu[synth_pop_df$Age >= 40 & synth_pop_df$Age < 50] <- 0.062#0.5*(0.095 + 0.062)
  mu[synth_pop_df$Age >= 50 & synth_pop_df$Age < 60] <- 0.099#0.5*(0.143 + 0.099)
  mu[synth_pop_df$Age >= 60 & synth_pop_df$Age < 70] <- 0.151#0.5*(0.221 + 0.151)
  mu[synth_pop_df$Age >= 70 & synth_pop_df$Age < 80] <- 0.238#0.5*(0.363 + 0.238)
  mu[synth_pop_df$Age >= 80] <- 0.365#0.5*(0.365+0.538)
  
  ##determine fates for each agent
  fates <- fate_func(N, synth_pop_df,
                     R0_init, MAXEIG, 
                     alpha, susceptRatio, susceptAgeSplit,
                     shapeEI, scaleEI, 
                     shapeIR, scaleIR, 
                     shapeIH, scaleIH, 
                     shapeHRD, scaleHRD, 
                     rho, 
                     h_rate, 
                     mu, 
                     numE = round(runif(1, min = N*0.0005, max = N*0.001)))


  #define length of first set of conditions (Jan - March 16)
  days_before_intervention <- 60  
  
  ################## Stage 1: OBSERVED TRAJECTORY OF EPIDEMIC JAN 17 - MARCH 16 #################
  StatesPhase1 <- SEIRQ_func(synth_pop_df, 
                             start_day = 1, 
                             end_day = days_before_intervention, 
                             contacts_orig_list = contacts_orig_list,
                             alpha,
                             beta = fates[["beta"]],
                             time_state1 = fates[["time_state1"]], time_next_state1 = fates[["time_next_state1"]],
                             time_state2 = fates[["time_state2"]], time_next_state2 = fates[["time_next_state2"]],
                             time_state3 = fates[["time_state3"]], time_next_state3 = fates[["time_next_state3"]], 
                             fate = fates[["fate"]],
                             state_full = fates[["state"]],
                             state = fates[["state"]],
                             propComplyCase = 0.8*0.6, 
                             propComplyHH = 0, 
                             redContacts = 0.5)
  

  ################## Stage 2a: OBS: Schools + Socializing  + workplaces CLOSED #################
  #define observed contact matrix
  OBS_contact_list <- contacts_orig_list
  
  #Use essential workers
  EWrk.cont <- contacts_orig_list[[7]]
  EssentialWorkMat <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(EWrk.cont)[1]){
    EssentialWorkMat[EWrk.cont[i,1],EWrk.cont[i,2]] <- 5/7
    EssentialWorkMat[EWrk.cont[i,2],EWrk.cont[i,1]] <- 5/7
  }
  EssentialWorkMatsp <- as(EssentialWorkMat, "sparseMatrix")
  rm(EssentialWorkMat)
  
  
  OBS_contact_list[[2]] <- EssentialWorkMatsp
  OBS_contact_list[[3]] <- 0*OBS_contact_list[[3]] #School
  OBS_contact_list[[4]] <- 0*OBS_contact_list[[4]] #Grade
  OBS_contact_list[[5]] <- 0*OBS_contact_list[[5]] #Class
  OBS_contact_list[[6]] <- gen_COMmat(Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #call SEIR function
  StatesPhase2_OBS <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #corresponds to june 1
                                 contacts_orig_list = OBS_contact_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.80*0.6, 
                                 propComplyHH = 0.5,
                                 redContacts = 0.75
  )
  
  state_full.OBS <- StatesPhase2_OBS[["state_full"]]
  
  #extract summary indicators
  outcomeOBS <- modelled_outcomes(state_full.OBS, synth_pop_df)
  
  rm(StatesPhase2_OBS);
  rm(OBS_contact_list)
  gc()
  ###################################################
  
  ################## Stage 2b: CF1: Schools stay open, all else as observed ######################
  
  #define contact matrices for CF1
  CF1_contact_list <- contacts_orig_list
  CF1_contact_list[[2]] <- EssentialWorkMatsp #Work
  CF1_contact_list[[6]] <- gen_COMmat(CF1.comm) #Community -- include work in the COMmat here, subtract daycare
  
  #Call SEIR function
  StatesPhase2_CF1 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = CF1_contact_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.80*0.6, 
                                 propComplyHH = 0.5,
                                 redContacts = 0.75
  )
  
  state_full.CF1 <- StatesPhase2_CF1[["state_full"]]
  
  #extract summary indicators
  outcomeCF1 <- modelled_outcomes(state_full.CF1, synth_pop_df)
  gc()
  
  rm(StatesPhase2_CF1);
  rm(CF1_contact_list);
  
  ############Stage 2: CF2: Work stay open, all else as observed ######################
  
  #define contact matrices
  CF2_contact_list <- contacts_orig_list
  CF2_contact_list[[3]] <- 0*CF2_contact_list[[3]] #School
  CF2_contact_list[[4]] <- 0*CF2_contact_list[[4]] #Grade
  CF2_contact_list[[5]] <- 0*CF2_contact_list[[5]] #Class
  CF2_contact_list[[6]] <- gen_COMmat(CF2.comm) #Community -- do NOT include work, DO add public transit (polymod)
  
  #Call SEIR function
  StatesPhase2_CF2 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = CF2_contact_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.8*0.6, 
                                 propComplyHH = 0.5,
                                 redContacts = 0.75
  )
  
  state_full.CF2 <- StatesPhase2_CF2[["state_full"]]
  
  #extract summary indicators
  outcomeCF2 <- modelled_outcomes(state_full.CF2, synth_pop_df)
  gc()
  
  rm(StatesPhase2_CF2);
  rm(CF2_contact_list);
  
  ############Stage 2d: CF3: Socializing stay open, all else as observed ######################
  
  #define contact matrices
  CF3_contact_list <- contacts_orig_list
  CF3_contact_list[[2]] <- EssentialWorkMatsp + 2/20*CF3_contact_list[[2]] #Work, still see some friends outside
  CF3_contact_list[[3]] <- 0*CF3_contact_list[[3]] #School 
  CF3_contact_list[[4]] <- 0*CF3_contact_list[[4]] #Grade
  CF3_contact_list[[5]] <- 2/20*CF3_contact_list[[5]] #Class #still see some members of class
  CF3_contact_list[[6]] <- gen_COMmat(CF3.comm)
  
  #Call SEIR function
  StatesPhase2_CF3 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = CF3_contact_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.8*0.6, 
                                 propComplyHH = 0,
                                 redContacts = 0.75
  )
  
  state_full.CF3 <- StatesPhase2_CF3[["state_full"]]
  
  #extract summary indicators
  outcomeCF3 <- modelled_outcomes(state_full.CF3, synth_pop_df)
  gc()
  
  rm(StatesPhase2_CF3);
  rm(CF3_contact_list);
  
  ############Stage 2e: CF4: Schools + Work stays open, social distance ###################
  
  #define contact matrices
  CF4_contact_list <- contacts_orig_list
  CF4_contact_list[[6]] <- gen_COMmat(CF4.comm)#Community -- remove work in the COMmat here, add public trans (CF2)
  
  #call SEIR function
  StatesPhase2_CF4 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = CF4_contact_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.8*0.6, 
                                 propComplyHH = 0.5,
                                 redContacts = 0.75
  )
  
  state_full.CF4 <- StatesPhase2_CF4[["state_full"]]
  
  #extract summary indicators
  outcomeCF4 <- modelled_outcomes(state_full.CF4, synth_pop_df)
  gc()
  
  rm(StatesPhase2_CF4);
  rm(CF4_contact_list);
  
  ############Stage 2f: CF5: Schools + Socializing open, workplaces close #################
  
  #define contact matrices
  CF5_contact_list <- contacts_orig_list
  CF5_contact_list[[2]] <- EssentialWorkMatsp + 2/20*CF5_contact_list[[2]]#Work
  CF5_contact_list[[6]] <- gen_COMmat(CF5.comm)
  ### for [[4]] POLYMOD Community: remove public transit, remove work, add work from survey.
  
  #call SEIR function
  StatesPhase2_CF5 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = CF5_contact_list,  
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.8*0.6, 
                                 propComplyHH = 0,
                                 redContacts = 0.75
  )
  
  state_full.CF5 <- StatesPhase2_CF5[["state_full"]]
  
  #extract summary indicators
  outcomeCF5 <- modelled_outcomes(state_full.CF5, synth_pop_df)
  gc()
  
  rm(StatesPhase2_CF5);
  rm(CF5_contact_list);
  
  ################## Stage 2g: CF6: Schools + Socializing  + workplaces open #################
  #COMmat.CF6 is the original...
  #CF6_contact_list <- contacts_orig_list
  #CF6_contacts <- contacts_orig
  
  #call seir function
  StatesPhase2_CF6 <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, #june 1
                                 contacts_orig_list = contacts_orig_list, 
                                 alpha,
                                 beta = fates[["beta"]],
                                 time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase1[["time_next_state1"]],
                                 time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase1[["time_next_state2"]],
                                 time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase1[["time_next_state3"]], 
                                 fate = fates[["fate"]],
                                 state_full = StatesPhase1[["state_full"]],
                                 state = StatesPhase1[["state_full"]][,days_before_intervention + 1],
                                 propComplyCase = 0.8*0.6, 
                                 propComplyHH = 0,
                                 redContacts = 0.75
  )
  
  state_full.CF6 <- StatesPhase2_CF6[["state_full"]]
  
  #extract summary indicators
  outcomeCF6 <- modelled_outcomes(state_full.CF6, synth_pop_df)
  gc()
  
  outcomes <- list("OBS" = outcomeOBS, "CF1" = outcomeCF1, "CF2" = outcomeCF2, 
                   "CF3" = outcomeCF3, "CF4" = outcomeCF4, "CF5" = outcomeCF5, 
                   "CF6" = outcomeCF6, "df" = synth_pop_df, "fate" = fates[["fate"]])
  outcomes
}

#Save model simulations
save(outcomes, file = "ModelledOutcomes_args.RData")