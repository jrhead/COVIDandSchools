# Main Code File to:
# 1. Create the synthetic population
# 2. Create contact matrices for certain counterfactial (CF) situations
# 3. Run the SEIR model across various time periods:
#       A. Epidemic start, prior to shelter-in-place (Jan 17 - March 16)
#       B. spring semester closure (MArch 16 - June 1)
#       C. Summer -- high and moderate transmission scenarios (June 1 - Aug 15)
#       D. Fall semester (Aug 15 - Dec 20) -- various reopening strategies
# 4. Summarize and save the output

# MODEL PARAMETERS VARIED IN ANALYSES (see Fig. 4-5)
propKidHH <- 0.251 #proportion of HH with children < 18
alpha = 0.5 #ratio of asymptomatic to symptomatic transmission; set to 0.5 in article
susceptRatio = 0.5 #ratio of the susceptibility of children < (susceptAgeSplit) to SARS-CoV-2 vs. adults; varied between 0.5 and 1
susceptAgeSplit = 20 #agents < 10 assumed half as susceptible (set to 20 yr in article)

# COMMUNITY TRANSMISSION PARAMETERS VARIED IN ANALYSES
propMidEssentialTot = 0.75 #set to 75% of in-person work for high transmission scenario; 
#0.5 for moderate transmission scenario

CommContactIncr = 2 #indicates increases in socializing over summer; 
#set to 2 (twice as many community contacts as observed during survey) 
#for high transmission scenario, 1.28 for moderate
#Load needed R scipts/functions
source(".\\code\\fall_reopen\\synth_pop_reopen_hybridbygrade.R")
source(".\\code\\fall_reopen\\contact_matrix_reopen.R") #make contact matrices
source(".\\code\\fall_reopen\\model_functions_reopen.R")


#load libraries needed to run simulatons run in parallel
library(doParallel)
library(foreach)


###################################################################
######   1. Load data for community contact matrices ##############
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

#Load the polymod data for the early epidemic period
polymod_dat <- readRDS(".\\data\\POLYMOD_contact_data\\polymod_dat.RData")
matrix <- as.matrix(polymod_dat$matrix)

###determine community matrices for if schools are open
CF1.comm <- comm_survey - daycare_survey
###Obs is comm_survey
Obs.comm <- comm_survey - work_survey

######################################################################
######   2. Create the synthetic population ##########################
######################################################################
##Population parameters (see supplement for sources)

N =16000  
numCommunity <- 1 #numCommunity must be > any one school...
avgHHsize <- 2.5
avgFamsize <- 3.3
MedAge <- 33
propFamHH <- 0.544
propSingleParent <- 0.40
propGrandparent <- 0.03
propMultigen <- 0.04
prop65p <- 0.111
NWork <- round((N/2)/20,0)
propUnemployed <- 0.22 #unemployed + work from home
Nschools <- list(Elementary = 3, Middle = 1, High = 1)
ClassSize = 20
NShifts = 2 #2 shifts

## Model parameters (see manuscript table 1)
R0_init = 2.5 #basic reproduction number

##Define waiting times
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
######   3. Run simulations for each reopening scenario ##############
######################################################################

#Run all situations 1000 times, in parallel
outcomes <- foreach(reps = 1:1000, .packages = "dplyr") %dopar% {
  
  ################## 0. PREP BY CREATING SYNTHETIC POPULATION AND THEIR FATES ################################
  #Generate synetic population
  
  synth_pop_df <- synth_pop(N=N, avgHHsize, avgFamsize, numCommunity, MedAge, propKidHH, propFamHH, propUnemployed, 
                            propSingleParent, propGrandparent, propMultigen, prop65p, NWork, Nschools,NShifts,
                            ClassSize)
  
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
                                        propMidEssential = propMidEssentialTot-0.28,
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
                     numE = round(runif(1, min = 5, max = 10)))
  
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
                             redContacts = 0.5,
                             mask_ef_teacher = 0, #no masks at this point
                             mask_ef_elem = 0,
                             mask_ef_middle = 0,
                             mask_ef_HS = 0)
  
  gc()
  
  ################## Stage 2a: OBSERVED MARCH 16 - JUNE 1: Schools + Socializing  + workplaces CLOSED #################
  #define observed contact matrix
  OBS_contact_list_spring <- contacts_orig_list
  #Use essential workers
  ##Essential Workers
  EWrk.cont <- contacts_orig_list[[10]]
  EssentialWorkMat <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(EWrk.cont)[1]){
    EssentialWorkMat[EWrk.cont[i,1],EWrk.cont[i,2]] <- 5/7
    EssentialWorkMat[EWrk.cont[i,2],EWrk.cont[i,1]] <- 5/7
  }
  EssentialWorkMatsp <- as(EssentialWorkMat, "sparseMatrix")
  rm(EssentialWorkMat)
  
  
  OBS_contact_list_spring[[2]] <- EssentialWorkMatsp
  OBS_contact_list_spring[[3]] <- 0*OBS_contact_list_spring[[3]] #School
  OBS_contact_list_spring[[4]] <- 0*OBS_contact_list_spring[[4]] #Grade
  OBS_contact_list_spring[[5]] <- 0*OBS_contact_list_spring[[5]] #Class
  OBS_contact_list_spring[[6]] <- gen_COMmat(Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #call SEIR function
  StatesPhase2a <- SEIRQ_func(synth_pop_df, 
                                 start_day = days_before_intervention + 1, 
                                 end_day = 138, 
                                 contacts_orig_list = OBS_contact_list_spring, 
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
                                 redContacts = 0.75,
                              mask_ef_teacher = 0,
                              mask_ef_elem = 0,
                              mask_ef_middle = 0,
                              mask_ef_HS = 0
  )
  
  rm(OBS_contact_list_spring)
  rm(EssentialWorkMatsp)
  gc()
  
  ###################################################
  
  ################## Stage 3: Summer ######################
  
  #update contact matrices for summer
  OBS_contact_list_summer <- contacts_orig_list
  
  ##in-person work expands, according to either high or mod transmission scenarios
  EWrk.cont.mid <- contacts_orig_list[[11]]
  MidEssentialWorkMat <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(EWrk.cont.mid)[1]){
    MidEssentialWorkMat[EWrk.cont.mid[i,1],EWrk.cont.mid[i,2]] <- 5/7
    MidEssentialWorkMat[EWrk.cont.mid[i,2],EWrk.cont.mid[i,1]] <- 5/7
  }
  MidEssentialWorkMatsp <- as(MidEssentialWorkMat, "sparseMatrix")
  rm(MidEssentialWorkMat)
  
  OBS_contact_list_summer[[2]] <- MidEssentialWorkMatsp
  OBS_contact_list_summer[[3]] <- 0*OBS_contact_list_summer[[3]] #Schools closed over summer
  OBS_contact_list_summer[[4]] <- 0*OBS_contact_list_summer[[4]] #Grade
  OBS_contact_list_summer[[5]] <- 0*OBS_contact_list_summer[[5]] #Class
  OBS_contact_list_summer[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #Call SEIR function
  StatesPhase2 <- SEIRQ_func(synth_pop_df, 
                             start_day = 139, #june 1
                             end_day = 212, #aug 15
                             contacts_orig_list = OBS_contact_list_summer, 
                             alpha,
                             beta = fates[["beta"]],
                             time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2a[["time_next_state1"]],
                             time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2a[["time_next_state2"]],
                             time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2a[["time_next_state3"]], 
                             fate = fates[["fate"]],
                             state_full = StatesPhase2a[["state_full"]],
                             state = StatesPhase2a[["state_full"]][,139],
                             propComplyCase = 0.80*0.6, 
                             propComplyHH = 0.25,
                             redContacts = 0.75,
                             mask_ef_teacher = 0,
                             mask_ef_elem = 0,
                             mask_ef_middle = 0,
                             mask_ef_HS = 0)
  
  rm(OBS_contact_list_summer)
  gc()
  
  ################## Stage 4; Reopening (Aug 15 - Dec 20) ##################
  ################## Sim1: Schools Fully Closed ############################
  
  #Update contact matrices
  CLOSED_contact_list <- contacts_orig_list

  CLOSED_contact_list[[2]] <- MidEssentialWorkMatsp
  CLOSED_contact_list[[3]] <- 0*CLOSED_contact_list[[3]] #School
  CLOSED_contact_list[[4]] <- 0*CLOSED_contact_list[[4]] #Grade
  CLOSED_contact_list[[5]] <- 0*CLOSED_contact_list[[5]] #Class
  CLOSED_contact_list[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #Call SEIR function
  StatesPhase3_CLOSED <- SEIRQ_func(synth_pop_df, 
                                    start_day = 213, #Aug 15
                                    end_day = 340, #Dec 20
                                    contacts_orig_list = CLOSED_contact_list, 
                                    alpha,
                                    beta = fates[["beta"]],
                                    time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2[["time_next_state1"]],
                                    time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2[["time_next_state2"]],
                                    time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2[["time_next_state3"]], 
                                    fate = fates[["fate"]],
                                    state_full = StatesPhase2[["state_full"]], #don't need the whole backstory...
                                    state = StatesPhase2[["state_full"]][,213],
                                    propComplyCase = 0.80*0.6, 
                                    propComplyHH = 0.25,
                                    redContacts = 0.75,
                                    mask_ef_teacher = 0,
                                    mask_ef_elem = 0,
                                    mask_ef_middle = 0,
                                    mask_ef_HS = 0)
  
  state_full.CLOSED <- StatesPhase3_CLOSED[["state_full"]]
  
  #extract summary indicators
  outcomeCLOSED <- modelled_outcomes(state_full.CLOSED, synth_pop_df)
  
  rm(state_full.CLOSED);
  rm(CLOSED_contact_list)
  gc()
  #####################################
  
  ################## Int 1: Weak cohort ############################
  #Update contact matrix to reflect reduction in non-class contacts
  S1_contact_list <- contacts_orig_list

  S1_contact_list[[2]] <- MidEssentialWorkMatsp
  S1_contact_list[[3]] <- 0.5*S1_contact_list[[3]] #reduce non-class contacts by 50%
  S1_contact_list[[4]] <- 0.5*S1_contact_list[[4]] #reduce non-class contacts by 50%
  S1_contact_list[[5]] <- (5/7 - 0.5*1/7 - 0.5*.2/7)/(5/7 - 1/7 - 0.2/7)*S1_contact_list[[5]]
  S1_contact_list[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #Call SEIR function
  StatesPhase3_S1 <- SEIRQ_func(synth_pop_df, 
                                start_day = 213, #aug 15
                                end_day = 340, #dec 20
                                contacts_orig_list = S1_contact_list, 
                                alpha,
                                beta = fates[["beta"]],
                                time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2[["time_next_state1"]],
                                time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2[["time_next_state2"]],
                                time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2[["time_next_state3"]], 
                                fate = fates[["fate"]],
                                state_full = StatesPhase2[["state_full"]], #don't need the whole backstory...
                                state = StatesPhase2[["state_full"]][,213],
                                propComplyCase = 0.80*0.6, 
                                propComplyHH = 0.25,
                                redContacts = 0.75,
                                mask_ef_teacher = 0,
                                mask_ef_elem = 0,
                                mask_ef_middle = 0,
                                mask_ef_HS = 0)
  
  state_full.S1 <- StatesPhase3_S1[["state_full"]]
  
  #extract outcomes
  outcomeS1 <- modelled_outcomes(state_full.S1, synth_pop_df)
  
  rm(state_full.S1);
  rm(S1_contact_list)
  gc()
  ############################
  
  ################## Int2: staggered school days ############################
  #Update contact list to reflect students come 2 days per week by grade shifts
  S2_contact_list <- contacts_orig_list
  S2_contact_list[[2]] <- MidEssentialWorkMatsp
  S2_contact_list[[3]] <- (2/5)*contacts_orig_list[[7]] #School with shifts
  S2_contact_list[[4]] <- (2/5)*contacts_orig_list[[8]] #Grade with shifts
  S2_contact_list[[5]] <- (2/5)*contacts_orig_list[[9]] #Class with shifts
  S2_contact_list[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #Call SEIR function
  StatesPhase3_S2 <- SEIRQ_func(synth_pop_df, 
                                start_day = 213, #Aug 15
                                end_day = 340, #Dec 20
                                contacts_orig_list = S2_contact_list, 
                                alpha,
                                beta = fates[["beta"]],
                                time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2[["time_next_state1"]],
                                time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2[["time_next_state2"]],
                                time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2[["time_next_state3"]], 
                                fate = fates[["fate"]],
                                state_full = StatesPhase2[["state_full"]], #don't need the whole backstory...
                                state = StatesPhase2[["state_full"]][,213],
                                propComplyCase = 0.80*0.6, 
                                propComplyHH = 0.25,
                                redContacts = 0.75,
                                mask_ef_teacher = 0,
                                mask_ef_elem = 0,
                                mask_ef_middle = 0,
                                mask_ef_HS = 0)
  
  state_full.S2 <- StatesPhase3_S2[["state_full"]]
  
  #extract summary indicators
  outcomeS2 <- modelled_outcomes(state_full.S2, synth_pop_df)
  
  rm(state_full.S2);
  rm(S2_contact_list)
  gc()
  
  ###################
  
  ################## Int3 : staggerred school days + classrooms only############################
  
  #Update contact matrix to refect 50% reduction in non-classroom contacts 
  # + 2 days per week grade shifts
  S3_contact_list <- contacts_orig_list
  S3_contact_list[[2]] <- MidEssentialWorkMatsp
  S3_contact_list[[3]] <- (2/5)*0.5*S3_contact_list[[7]] #School shifts; 50% reduction
  S3_contact_list[[4]] <- (2/5)*0.5*S3_contact_list[[8]] #Grade shifts; 50% reduction
  S3_contact_list[[5]] <- (2/5)*(5/7 - 0.5*1/7 - 0.5*.2/7)/(5/7 - 1/7 - 0.2/7)*S3_contact_list[[9]] #Class
  S3_contact_list[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #call SEIR function
  StatesPhase3_S3 <- SEIRQ_func(synth_pop_df, 
                                start_day = 213, #Aug 15
                                end_day = 340, #Dec 20
                                contacts_orig_list = S3_contact_list, 
                                alpha,
                                beta = fates[["beta"]],
                                time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2[["time_next_state1"]],
                                time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2[["time_next_state2"]],
                                time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2[["time_next_state3"]], 
                                fate = fates[["fate"]],
                                state_full = StatesPhase2[["state_full"]], #don't need the whole backstory...
                                state = StatesPhase2[["state_full"]][,213],
                                propComplyCase = 0.80*0.6, 
                                propComplyHH = 0.25,
                                redContacts = 0.75,
                                mask_ef_teacher = 0,
                                mask_ef_elem = 0,
                                mask_ef_middle = 0,
                                mask_ef_HS = 0)
  
  
  state_full.S3 <- StatesPhase3_S3[["state_full"]]
  
  #extract summary indicators
  outcomeS3 <- modelled_outcomes(state_full.S3, synth_pop_df)
  
  rm(state_full.S3);
  rm(S3_contact_list)
  gc()
  ##########################
  
  ################## Sim6: Mask, monthly testing, weak cohorts ############################
  
  S1_contact_list <- contacts_orig_list
  S1_contact_list[[2]] <- MidEssentialWorkMatsp
  S1_contact_list[[3]] <- 0.5*S1_contact_list[[3]] #School; 50% reduction in non-grade contacts
  S1_contact_list[[4]] <- 0.5*S1_contact_list[[4]] #Grade; 50% reduction in non-grade contacts
  S1_contact_list[[5]] <- (5/7 - 0.5*1/7 - 0.5*.2/7)/(5/7 - 1/7 - 0.2/7)*S1_contact_list[[5]]
  S1_contact_list[[6]] <- gen_COMmat(CommContactIncr*Obs.comm) #Community, EXACTLY as observed, with work, daycare, public trans included
  
  #Call SEIR function, allowing for testing
  StatesPhase3_M.TT30.S1 <- SEIRtest_func(synth_pop_df, 
                                          start_day = 213, #aug 15 
                                          end_day = 340, #dec 20 
                                          test_day = 30, #test every 30 days
                                          test_students = 0, #do not test students
                                          contacts_orig_list = S1_contact_list, 
                                          alpha,
                                          beta = fates[["beta"]],
                                          time_state1 = fates[["time_state1"]], time_next_state1 = StatesPhase2[["time_next_state1"]],
                                          time_state2 = fates[["time_state2"]], time_next_state2 = StatesPhase2[["time_next_state2"]],
                                          time_state3 = fates[["time_state3"]], time_next_state3 = StatesPhase2[["time_next_state3"]], 
                                          fate = fates[["fate"]],
                                          state_full = StatesPhase2[["state_full"]], #don't need the whole backstory...
                                          state = StatesPhase2[["state_full"]][,213],
                                          propComplyCase = 0.80*0.6, 
                                          propComplyHH = 0.25,
                                          redContacts = 0.75,
                                          mask_ef_teacher = 0.5, #wear masks
                                          mask_ef_elem = 0.35,
                                          mask_ef_middle = 0.25,
                                          mask_ef_HS = 0.15)
  
  
  state_full.M.TT30.S1 <- StatesPhase3_M.TT30.S1[["state_full"]]
  
  #extracted summary indicators
  outcomeM.TT30.S1<- modelled_outcomes(state_full.M.TT30.S1, synth_pop_df)
  
  rm(state_full.M.TT30.S1);
  gc()
  
  #########################################################################
  # Combine output
  
  outcomes <- list("CLOSED" = outcomeCLOSED, 
                   "Int1" = outcomeS1, 
                   "Int2" = outcomeS2, 
                   "Int3" = outcomeS3,
                   "M.TT30.S1" = outcomeM.TT30.S1,
                   "df" <- synth_pop_df,
                   "fates" <- fates[["fate"]])
  outcomes
}

#Save model simulations
save(outcomes, file = "ModelledReopeningStrategies_hybridbygrade.RData")

