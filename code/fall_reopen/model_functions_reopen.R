#### Functions needed in the Agent Based Model Simulation for reopening strategies ####
##
## Functions include:
#  1. fate_func: define fates for N people and waiting times between stages
#  2. gen_COMmat: generates and NxN community matrix for the full population
#  3. SEIRQ_func: runs the SEIR model, including an intervention where symptomatic individuals isolate
#  4. SEIRtest_func: adds to the SEIRQ_func to permit testing on a particular interval, following by quarantine/isolation
#  5. modelled_outcomes: counts/summarizes important outcomes (e.g. cumulative infections, daily hospitalizations)
##
fate_func <- function(N, #Number of people in population
                    synth_pop_df, # dataframe of synthetic population
                    R0_init, #Transmission rate, AKA proportion of susceptible-infected contacts result in a new exposure (AGE DEPENDENT)
                    MAXEIG, #dominant eigenvalue of initial contact matrix
                    alpha,#prop of transmission from sub clinical infection
                    susceptRatio, #ratio of susceptibility of children to adults
                    susceptAgeSplit, #age at which susceptibility changes
                    shapeEI, scaleEI, #weibull parameters, waiting time  between exposure and infection
                    shapeIR, scaleIR, #weibull parameters, waiting time between infection and recovery
                    shapeIH, scaleIH, #weibull parameters, waiting time between infection and hospitalization
                    shapeHRD, scaleHRD, #weibull parameters, waiting time between hospitalization and recovery or death
                    rho, #probability of an infected case being clinical (AGE DEPENDENT)
                    h_rate, #vector of hospitalization rates in infected symptomatic individuals by age
                    mu, #death rate per individual
                    numE #The number of individuals originally exposed in the population)
){
  ## 1. Determine initial states (mostly S with some people exposed)
  State0 <- rep("S",N) #susceptible
  initI <- sample(1:N, numE, replace = F)
  
  State0[initI] <- "E" #Exposed
  
  ## 2. Determine fate of individual should they become exposed (A = Asymptomatic, C = infected, recover,
  #  H = hospitalized, recover, D = hospitalized, die)
  
  #Draw random number per respondent to decide fate
  fate <- rep(NA,N)
  A_prob <- 1-rho
  C_prob <- rho*(1-h_rate)
  H_prob <- rho*h_rate*(1-mu)
  D_prob <- rho*h_rate*mu
  
  #Draw random number per respondent to decide fate
  Rand <- runif(N, min = 0, max = 1)
  fate <- ifelse(Rand <= A_prob, "A",
                 ifelse(Rand > A_prob & Rand <= (A_prob + C_prob), "C",
                        ifelse(Rand > (A_prob + C_prob) & Rand <= (A_prob + C_prob + H_prob), "H", "D")))

  ## 3. Define waiting times for each person, should they fall into specific fates
  
  waitEI <- rweibull(N, shapeEI, scaleEI)
  waitIR <- rweibull(N, shapeIR, scaleIR)
  waitIH <- rweibull(N, shapeIH, scaleIH)
  waitHRD <- rweibull(N, shapeHRD, scaleHRD)
  
  #define waiting times based on fate
  time_state1 <- time_state2 <- time_state3 <- waitEI #initialize time to next state
  time_state2[fate == "A"|fate == "C"] <- waitEI[fate == "A"|fate == "C"] + waitIR[fate == "A"|fate == "C"]
  time_state2[fate == "H" | fate == "D"] <- waitEI[fate == "H" | fate == "D"] + waitIH[fate == "H" | fate == "D"]
  time_state3[fate == "A"|fate == "C"] <- 365 #some large number
  time_state3[fate == "H" | fate == "D"] <- waitEI[fate == "H" | fate == "D"] + waitIH[fate == "H" | fate == "D"] +
    waitHRD[fate == "H" | fate == "D"]
  
  #initialize wait times for the persons who start as exposed...
  state <- State0
  time_next_state1 <- time_next_state2 <- time_next_state3 <- rep(365,N) #initialize at a large #
  time_next_state1[state == "E"] <- 1 + time_state1[state == "E"]
  time_next_state2[state == "E"] <- 1 + time_state2[state == "E"]
  time_next_state3[state == "E"] <- 1 + time_state3[state == "E"]
  
  ## 4. Calculate beta
  pA <- sum(fate == "A")/N; gamma1 <- mean(waitIR)
  pC <- sum(fate == "C")/N; gamma2 <- mean(waitIR)
  pH <- sum(fate == "H")/N; gamma3 <- mean(waitIH)
  pD <- sum(fate == "D")/N; gamma4 <- mean(waitIH)
  
  # Formula for \bar{beta}
  beta0 <- R0_init/(pA*alpha*gamma1 + pC*gamma2 + pH*gamma3 + pD*gamma4)
  meanBeta <- beta0/MAXEIG
  
  # Adjust beta by suceptibility ratios
  p_young <- sum(synth_pop_df$Age < susceptAgeSplit)/N
  p_old <- sum(synth_pop_df$Age >= susceptAgeSplit)/N
  beta_old <- meanBeta/(p_old + susceptRatio*p_young)
  beta_young <- susceptRatio*beta_old
  beta <- rep(beta_old,N)
  beta[synth_pop_df$Age < susceptAgeSplit] <- beta_young

  ## 5. Return elements needed for SEIR
  out <- list("beta" = beta, 
              "time_state1" = time_state1,
              "time_state2" = time_state2,
              "time_state3" = time_state3,
              "time_next_state1" = time_next_state1, 
              "time_next_state2" = time_next_state2, 
              "time_next_state3" = time_next_state3, 
              "fate" = fate,
              "state" = state)
  return(out)
  
}

# Function to create and return an N x N community matrices for various reopening scenarios

gen_COMmat <- function(matrix_name #summary matrix of total # of age-structed daily contacts
){
       
  #Divide count of age-specific contacts by # in that age group                
  PostAgeContRates <- matrix(c(
    matrix_name[1,]/Age_Vect2[1],
    matrix_name[2,]/Age_Vect2[2],
    matrix_name[3,]/Age_Vect2[3],
    matrix_name[4,]/Age_Vect2[4],
    matrix_name[5,]/Age_Vect2[5],
    matrix_name[6,]/Age_Vect2[6]), ncol = 6)
  
  #Match rates to individuals based on age 
  rownames(PostAgeContRates) <- c("1", "2", "3", "4", "5", "6")
  AgeCat2 <- match(as.character(synth_pop_df$AgeCat2), rownames(PostAgeContRates))
  
  #Generate NxN matrix of age-structured community contacts
  COMmat <- matrix(0,N,N)
  for(i in 1:N) {
    COMmat[i,] <-  unlist(PostAgeContRates[AgeCat2[i], AgeCat2])
  }
  diag(COMmat) <- 0
  return(COMmat)
}

# Function to run the SEIR allowing for isolation of cases and quarantine of HH members
# differs from the SEIRQ_func applied in the spring semester because allows for masks
# mask effectiveness of 0 corresponds to no mask wearing at all.

SEIRQ_func <- function(synth_pop_df, #synthetic population
                      start_day, #initial day of simulation
                      end_day, #last day of simulation
                      contacts_orig_list, #original contact matrices broken up by household, school, workplace, community, grade and class
                      alpha, #ratio of infected cases transmitting to non-infected
                      beta, #scaling parameter
                      time_state1, time_next_state1, #distribution of waiting times to get to first state
                      time_state2, time_next_state2, #distribution of waiting times to get to second state
                      time_state3, time_next_state3, #distribution of waiting times to get to third state
                      fate, #Vector of pre-determined individual fates, if they got sick
                      state_full, #an N x day matrix of states
                      state, #a N x 1 vector of current state
                      quarantine_cases, #a vector indicating whether cases are being quarantined on that day
                      propComplyCase, #proportion of cases that comply with quarantine intervention
                      propComplyHH, #proportion of household members that also isolate
                      redContacts, #how much this reduces community contacts (school and work contacts are assumed to go to zero
                      mask_ef_teacher, #effectiveness of teacher's
                      mask_ef_elem, #effectiveness of mask for elementary student
                      mask_ef_middle, #effectiveness of mask for middle school student
                      mask_ef_HS #effectiveness of mask for high school student
){


  adultID <- synth_pop_df[which(synth_pop_df$AgeCat == "18-64"),]$ID

  #run simulations
  for (day in start_day:end_day){
    ## 1. Update contact matrices such that a given proportion of symptomatic individuals 
    #      and a given proportion of their HHmembers reduce school and work contacts by 100%, community by 75%
    
    # - Determine who the infectious individuals are to be quarantined
    # - Let them be infectious for two days before symptoms recognized
    # - Add quarantine intervention for days it is in place
    
    
    #Define individuals who are infectious and will be symptomatic
    SymptVect <- SymptVectSCH <- SymptVectWRK <- SymptVectCOM <- as.numeric(state %in% c("C", "H1", "D1"))
    ASymptVect <- ASymptVectSCH <- ASymptVectWRK <- ASymptVectCOM <- as.numeric(state == "A")
    
    if(day >= 3){ #only isolate on 3rd day of being infectious
      
      #determine ID of case
      qID <- synth_pop_df[which(state_full[,day-2] == "C" | state_full[,day-2] == "H1" |state_full[,day-2] == "D1"),]$ID
      qID <- as.numeric(as.character(qID))
      
        if(length(qID) != 0){
        qHH <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% qID),]$HH)) #find all households that have a case that should be quarantined
        qHH_IDs <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$HH %in% qHH),]$ID)) #find all persons in those households
        qHH_IDs <- qHH_IDs[!qHH_IDs %in% qID] #remove the actual cases; only keep household members
        
        #list IDs who will comply with isolation/quarantine
        complyCase <- c(qID*rbinom(length(qID), 1, propComplyCase), qHH_IDs*rbinom(length(qHH_IDs), 1, propComplyHH)) #does the individual comply? 1 = yes
        complyCase <- complyCase[complyCase != 0]
        
        if(length(complyCase>0)){
          
          SymptVectSCH[complyCase] <- 0 #reduce school and work contacts by 100%
          SymptVectWRK[complyCase] <- 0 #reduce school and work contacts by 100%
          SymptVectCOM[complyCase] <- (1-redContacts)*SymptVect[complyCase] #reduce community contacts by user input
        }
      }
    }
    
    
    ##Update beta to reflect reductions in the secondary attack rate due to masks
    mask_ef <- rep(1, N) #initialize vector of mask effectiveness by age
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age>=18] <- 1-mask_ef_teacher #teachers
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age<11] <- 1-mask_ef_elem #elementary school students
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age>=11 & synth_pop_df$Age < 13] <- 1-mask_ef_middle #middle school students
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age >= 13 & synth_pop_df$Age < 18] <- 1-mask_ef_HS #high school students
    
    #update outgoing spread -- add mask using only during schools (e.g. not at home, in community)
    SymptVectSCH <- SymptVectSCH*mask_ef 
    ASymptVectSCH <- ASymptVectSCH*mask_ef 
    
    ### 2. Movement of S to E 
    # Determine contact with symptomatic individuals 
    SymptContVect <- contacts_orig_list[[1]] %*% SymptVect +
      contacts_orig_list[[2]] %*% SymptVectWRK +
      mask_ef*contacts_orig_list[[3]] %*% SymptVectSCH + #add reductions in probability of exposure within school environment due to masks
      mask_ef*contacts_orig_list[[4]] %*% SymptVectSCH + #add reductions in probability of exposure within school environment due to masks
      mask_ef*contacts_orig_list[[5]] %*% SymptVectSCH + #add reductions in probability of exposure within school environment due to masks
      contacts_orig_list[[6]] %*% SymptVectCOM
    # Determine contact with asymptomatic individuals
    AsymptContVect <- contacts_orig_list[[1]] %*% ASymptVect +
      contacts_orig_list[[2]] %*% ASymptVectWRK +
      mask_ef*contacts_orig_list[[3]] %*% ASymptVectSCH + #add reductions in probability of exposure within school environment due to masks
      mask_ef*contacts_orig_list[[4]] %*% ASymptVectSCH+ #add reductions in probability of exposure within school environment due to masks
      mask_ef*contacts_orig_list[[5]] %*% ASymptVectSCH + #add reductions in probability of exposure within school environment due to masks
      contacts_orig_list[[6]] %*% ASymptVectCOM

    # Calculate overall force of infection for agent i
    exposed_prob <- 1 - exp(-beta*(SymptContVect + alpha*AsymptContVect))
    # Determine if individual will be exposed
    exposed <- rbinom(N, 1, p = exposed_prob@x)*as.numeric(state == "S")
    
    # Option for stochastic infection obtained outside the community (one or two exposed people per day)
    outside <- 1+rbinom(n = 1, size = 1, prob = 0.5)
    randID <- sample(adultID, outside)
    exposed[state == "S" & synth_pop_df$ID == randID] <- 1
  
    #Update times when the exposed individual's next events will occur
    time_next_state1[exposed == 1] <- day + time_state1[exposed == 1]
    time_next_state2[exposed == 1] <- day + time_state2[exposed == 1]
    time_next_state3[exposed == 1] <- day + time_state3[exposed == 1]
    
    ## 3. Update states
    
    #move from S to E (no waiting time)
    state[exposed == 1] <- "E"
    
    #move from E to A, C, H1, or D1 after waiting time 1, based on fates
    state[state == "E" & fate == "A" & day >= time_next_state1] <- "A"
    state[state == "E" & fate == "C" & day >= time_next_state1] <- "C"
    state[state == "E" & fate == "H" & day >= time_next_state1] <- "H1"
    state[state == "E" & fate == "D" & day >= time_next_state1] <- "D1"
    
    #move from A --> R; H1/D1 --> H2/D2 after waiting time 2
    state[(state == "A" | state == "C") & day >= time_next_state2] <- "R"
    state[state == "H1" & day >= time_next_state2] <- "H2"
    state[state == "D1"  & day >= time_next_state2] <- "D2"
    
    #move fate of people in hospital (H2 --> R; D2 --> M) after waiting time 3
    state[state == "H2" & day >= time_next_state3] <- "R"
    state[state == "D2" & day >= time_next_state3] <- "M"
    
    #Bind history of states
    state_full <- cbind(state_full, state)
    day #print
  }
  #Return history of states, 
  # and waiting times to next state on the last day of the simulation
  out <- list("state_full" = state_full,
              "time_next_state1" = time_next_state1,
              "time_next_state2" = time_next_state2,
              "time_next_state3" = time_next_state3)
  return(out)
}

SEIRtest_func <- function(synth_pop_df, #synthetic population
                       start_day, #initial day of simulation
                       end_day, #last day of simulation
                       test_day, #frequency of testing
                       test_students, #test students or not
                       contacts_orig_list, #original contact matrices broken up by household, school, workplace, community, grade and class
                       alpha, #ratio of infected cases transmitting to non-infected
                       beta, #scaling parameter
                       time_state1, time_next_state1, #distribution of waiting times to get to first state
                       time_state2, time_next_state2, #distribution of waiting times to get to second state
                       time_state3, time_next_state3, #distribution of waiting times to get to third state
                       fate, #Vector of pre-determined individual fates, if they got sick
                       state_full, #an N x day matrix of states
                       state, #a N x 1 vector of current state
                       quarantine_cases, #a vector indicating whether cases are being quarantined on that day
                       propComplyCase, #proportion of cases that comply with quarantine intervention
                       propComplyHH, #proportion of household members that also isolate
                       redContacts, #how much this reduces community contacts (school and work contacts are assumed to go to zero                      
                       mask_ef_teacher, #effectiveness of teacher's
                       mask_ef_elem, #effectiveness of mask for elementary student
                       mask_ef_middle, #effectiveness of mask for middle school student
                       mask_ef_HS #effectiveness of mask for high school student
){
  

  synth_pop_df$ID <- as.numeric(as.character(synth_pop_df$ID))
  adultID <- synth_pop_df[which(synth_pop_df$AgeCat == "18-64"),]$ID
  
  #Initialize dataframe that will contain the IDs of individuals who test positive,
  # the IDs of their classmates, and the day their quarantine will end
  quarantined <- data.frame("ID" = NA, "day_release" = NA)
  #initialize initial vector of IDs that test positive
  quarantinedID <- N+1 #initialize at something it cannot be to start
 
  #run simulations
  for (day in start_day:end_day){

    #Define individuals who are infectious and will be symptomatic
    SymptVect <- SymptVectSCH <- SymptVectWRK <- SymptVectCOM <- as.numeric(state %in% c("C", "H1", "D1"))
    #Define individuals who are infectious, yet asymptomatic -- may be detected during testing
    ASymptVect <- ASymptVectSCH <- ASymptVectWRK <- ASymptVectCOM <- as.numeric(state == "A")
    
    ## 1. Update contact matrices such that a given proportion of symptomatic individuals 
    #      and a given proportion of their HHmembers reduce school and work contacts by 100%, community by 75%
    
    # - Determine who the infectious individuals are to be quarantined
    # - Let them be infectious for two days before symptoms recognized
    # - Add quarantine intervention for days it is in place
    
    if(day >= 3){ #only isolate on 3rd day of being infectious
      
      #determine ID of case
      qID <- synth_pop_df[which(state_full[,day-2] == "C" | state_full[,day-2] == "H1" |state_full[,day-2] == "D1"),]$ID
      qID <- as.numeric(as.character(qID))
      
      if(length(qID) != 0){
        qHH <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% qID),]$HH)) #find all households that have a case that should be quarantined
        qHH_IDs <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$HH %in% qHH),]$ID)) #find all persons in those households
        qHH_IDs <- qHH_IDs[!qHH_IDs %in% qID] #remove the actual cases; only keep household members
        
        #list IDs who will comply with isolation/quarantine
        complyCase <- c(qID*rbinom(length(qID), 1, propComplyCase), qHH_IDs*rbinom(length(qHH_IDs), 1, propComplyHH)) #does the individual comply? 1 = yes
        complyCase <- complyCase[complyCase != 0]
        
        if(length(complyCase>0)){
          
          SymptVectWRK[complyCase] <- 0 #reduce school and work contacts by 100%
          SymptVectSCH[complyCase] <- 0 #reduce school and work contacts by 100%
          SymptVectCOM[complyCase] <- (1-redContacts)*SymptVectCOM[complyCase] #reduce community contacts by user input
        }

      }
    }
    
    ## 2. Update contact matrices such that if an individual tests positive, they will  
    #      reduce school and work contacts by 100%, community by 75%; so will their classmates
    
    # - Ascertain cases every n days, assuming some test sensitivity
    # - allow testing of teachers only OR both students & teachers
    # - if case is a teacher or student (NOT admin!), determine who classmates are, and quarantine them
    # - quarantine individuals for 14 days
    # - assume test takes 1 day to get results
    
    if (day %% test_day == 0){ #calculate remainder, for every 7th day, for instance
      
      #determine whether there is an infectious teacher on day of test
      #here use [day - 1] to permit circulation of virus for one additional day prior to obtaining test results
      infected.teacher <- synth_pop_df[which(!is.na(synth_pop_df$School) & synth_pop_df$Age >= 18 & #teacher
                                               (state_full[,day-1] == "A" |state_full[,day-1] == "C" | 
                                                  state_full[,day-1] == "H1" |state_full[,day-1] == "D1")),]$ID
      
      #if they are already on quarantine list, remove them so as not to restart the 14 days
      infected.teacher <- infected.teacher[!(infected.teacher %in% quarantinedID)] 
      
      if(length(infected.teacher)>0){ #there is an infectious teacher on day of interest
        
        #simulate if test comes back positive
        infected.teacher <- infected.teacher*rbinom(length(infected.teacher), 1, prob = 0.85) #account for sensitivity of test
        infected.teacher <- infected.teacher[infected.teacher!=0]

        if(length(infected.teacher)>0){
          
          #Identify the class of the infected teacher
          only.teachers <- infected.teacher*as.numeric(!is.na(synth_pop_df[which(synth_pop_df$ID %in% infected.teacher),]$Class))
          only.teachers <- only.teachers[only.teachers != 0]
          
          if(length(only.teachers) > 0){ #if loop needed because infected admin has no class
            #identify the school, grade and class of the infected teacher
            qSCH <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% only.teachers),]$School)) 
            qGRD <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% only.teachers),]$Grade)) 
            qCLS <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% only.teachers),]$Class)) 
              
            #find all persons in those classes
            exposed.teacherP0 <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$School == qSCH &
                                                                          synth_pop_df$Grade == qGRD &
                                                                          synth_pop_df$Class == qCLS),]$ID))
            exposed.teacherP0 <- exposed.teacherP0[-c(exposed.teacherP0 %in% infected.teacher)] #just the students
            
            exposed.teacherP0 <- c(exposed.teacherP0, infected.teacher)
            
          } else {
            exposed.teacherP0 <- infected.teacher
          }
          #update the quarantined dataframe to include the ID of the agent being quarantined and the day they will be released
          quarantined <- rbind(quarantined, (cbind("ID" = exposed.teacherP0, "day_release" = day + 14)))
          
        }
        
      }
     
      if (test_students == 1){ #run this if students will also be tested
        #indentify which students are infectious on the day of the test
        infected.student <- synth_pop_df[which(!is.na(synth_pop_df$School) & synth_pop_df$Age < 18 & #student
                                                 (state_full[,day-1] == "A" |state_full[,day-1] == "C" | 
                                                    state_full[,day-1] == "H1" |state_full[,day-1] == "D1")),]$ID
        
        infected.student <- infected.student[!(infected.student %in% quarantinedID)]
        
        if(length(infected.student)>0){
          #determine whether student will test positive
          #here, assumed lower sensitivity for students (less invasive test is more feasible)
          infected.student <- infected.student*rbinom(length(infected.student), 1, prob = 0.73) #account for sensitivity of test
          infected.student <- infected.student[infected.student!=0]
          
          if(length(infected.student)>0){
            
            #identify the school, grade and class of the infected student
            qSCH <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% infected.student),]$School)) 
            qGRD <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% infected.student),]$Grade)) 
            qCLS <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$ID %in% infected.student),]$Class)) 
            
            #find all persons in those classes
            exposed.studentP0 <- as.numeric(as.character(synth_pop_df[which(synth_pop_df$School == qSCH &
                                                                              synth_pop_df$Grade == qGRD &
                                                                              synth_pop_df$Class == qCLS),]$ID))
            
            #update the quarantined dataframe to include the ID of the agent being quarantined and the day they will be released
            quarantined <- rbind(quarantined, (cbind("ID" = exposed.studentP0, "day_release" = day + 14)))
          }
        }
      }
    }
    
    #Update quaratine list using yesterday's test results
    quarantined <- subset(quarantined, day_release > day) #remove those done with their 14 day quarantine
    quarantinedID <- quarantined$ID
    
    if(length(quarantinedID>0)){
      SymptVectSCH[quarantinedID] <- 0 #stay home from school
      SymptVectCOM[quarantinedID] <- (1-redContacts)*SymptVectCOM[quarantinedID] #reduce community contacts
      
      ASymptVectSCH[quarantinedID] <- 0 #stay home from school
      ASymptVectCOM[quarantinedID] <- (1-redContacts)*ASymptVect[quarantinedID] #reduce community contacts
    }
    
    ##Update beta to reflect reductions in the secondary attack rate due to masks
    mask_ef <- rep(1, N) #initialize vector of mask effectiveness by age
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age>=18] <- 1-mask_ef_teacher #teachers
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age<11] <- 1-mask_ef_elem #elementary school students
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age>=11 & synth_pop_df$Age < 13] <- 1-mask_ef_middle #middle school students
    mask_ef[!is.na(synth_pop_df$School) & synth_pop_df$Age >= 13 & synth_pop_df$Age < 18] <- 1-mask_ef_HS #high school students
    
    #update outgoing spread -- add mask using only during schools (e.g. not at home, in community)
    SymptVectSCH <- SymptVectSCH*mask_ef
    ASymptVectSCH <- ASymptVectSCH*mask_ef
    
    ### 3. Movement of S to E 
    # Determine contact with symptomatic individuals
    SymptContVect <- contacts_orig_list[[1]] %*% SymptVect +
      contacts_orig_list[[2]] %*% SymptVectWRK +
      mask_ef*contacts_orig_list[[3]] %*% SymptVectSCH +
      mask_ef*contacts_orig_list[[4]] %*% SymptVectSCH +
      mask_ef*contacts_orig_list[[5]] %*% SymptVectSCH +
      contacts_orig_list[[6]] %*% SymptVectCOM
    # Determine contact with asymptomatic individuals
    AsymptContVect <- contacts_orig_list[[1]] %*% ASymptVect +
      contacts_orig_list[[2]] %*% ASymptVectWRK +
      mask_ef*contacts_orig_list[[3]] %*% ASymptVectSCH +
      mask_ef*contacts_orig_list[[4]] %*% ASymptVectSCH +
      mask_ef*contacts_orig_list[[5]] %*% ASymptVectSCH +
      contacts_orig_list[[6]] %*% ASymptVectCOM
    
    # Calculate overall force of infection for agent i
    exposed_prob <- 1 - exp(-beta*(SymptContVect + alpha*AsymptContVect))
    # Determine if individual will be exposed
    exposed <- rbinom(N, 1, p = exposed_prob@x)*as.numeric(state == "S")
    
    # Option for stochastic infection obtained outside the community (one or two exposed people per day)
    outside <- 1+rbinom(n = 1, size = 1, prob = 0.5)
    randID <- sample(adultID, outside)
    exposed[state == "S" & synth_pop_df$ID == randID] <- 1
    
    #Update times when the exposed individual's next events will occur
    time_next_state1[exposed == 1] <- day + time_state1[exposed == 1]
    time_next_state2[exposed == 1] <- day + time_state2[exposed == 1]
    time_next_state3[exposed == 1] <- day + time_state3[exposed == 1]
    
    ## 4. Update states
    
    #move from S to E (no waiting time)
    state[exposed == 1] <- "E" #movement from S to E, without a waiting time
    
    #move from E to A, C, H1, or D1 after waiting time 1, based on fates
    state[state == "E" & fate == "A" & day >= time_next_state1] <- "A"
    state[state == "E" & fate == "C" & day >= time_next_state1] <- "C"
    state[state == "E" & fate == "H" & day >= time_next_state1] <- "H1"
    state[state == "E" & fate == "D" & day >= time_next_state1] <- "D1"
    
    #move from A --> R; H1/D1 --> H2/D2 after waiting time 2
    state[(state == "A" | state == "C") & day >= time_next_state2] <- "R"
    state[state == "H1" & day >= time_next_state2] <- "H2"
    state[state == "D1"  & day >= time_next_state2] <- "D2"
    
    #move fate of people in hospital (H2 --> R; D2 --> M) after waiting time 3
    state[state == "H2" & day >= time_next_state3] <- "R"
    state[state == "D2" & day >= time_next_state3] <- "M"
    
    #Bind history of states
    state_full <- cbind(state_full, state)
    day #print
    
  }
  #Return history of states, 
  # and waiting times to next state on the last day of the simulation
  out <- list("state_full" = state_full,
              "time_next_state1" = time_next_state1,
              "time_next_state2" = time_next_state2,
              "time_next_state3" = time_next_state3)
  return(out)
}


# Function to summarize important outcomes over time given full history of states per agent

modelled_outcomes <- function(state_full,#output of SEIR function - dataframe of states per agent per day
                              synth_pop_df #dataframe on synthetic population 
){
  
  ## 1. Outcomes among the general community
  # Daily infections, hospitalizations, mortality
  TimeSeries_I <- colSums(state_full == "C" | state_full == "A"| state_full == "H1" | state_full == "D1")
  TimeSeries_IC <- colSums(state_full == "C"| state_full == "H1" | state_full == "D1")
  TimeSeries_M <- colSums(state_full == "M")
  TimeSeries_Hosp <- colSums(state_full == "H1" | state_full == "D1")
  
  # Cumulative infections
  cum_full <- death_full <- state_full
  cum_full <- ifelse(cum_full == "S" | cum_full == "E", 0, 1)
  CumTotal_I <- colSums(cum_full)
  
  # Cumulatice symptomatic infections
  #Remove the asymptomatics to count only clinical cases
  Acase <- as.numeric(rowSums(state_full == "A")>0) == 0 #FALSE if they are asymptomatic
  state_full_As <- state_full[Acase,]
  CumTotal_IC <- colSums(ifelse(state_full_As == "S" | state_full_As == "E", 0, 1))
  
  # Cumulative deaths
  death_full <- ifelse(death_full == "M", 1, 0)
  CumTotal_M <- colSums(death_full)
  

  ## 2. Outcomes among households with ties to a school (student, teacher, family members)
  
  # Subset data to only school households
  # find HH id of student or teacher
  schoolHHs <- synth_pop_df[which(!is.na(synth_pop_df$School)),]$HH
  keep <- synth_pop_df$HH %in% schoolHHs
  state_full_subs <- state_full[keep,]
  
  #Cumulative infections and hospitalizations
  cum_full_subs <- death_full_subs <- state_full_subs
  CumSchoolHH_I <- colSums(ifelse(cum_full_subs == "S" | cum_full_subs == "E", 0, 1))
  CumSchoolHH_M <- colSums(ifelse(death_full_subs == "M", 1, 0))
  
  #Cumulative symptomatic infections
  #Remove the asymptomatics to count only clinical cases
  Acase <- as.numeric(rowSums(state_full_subs == "A")>0) == 0 #FALSE if they are asymptomatic
  state_full_subs_As <- state_full_subs[Acase,]
  CumSchoolHH_IC <- colSums(ifelse(state_full_subs_As == "S" | state_full_subs_As == "E", 0, 1))

  ## 3. Record final and intial state
  days <- dim(state_full)[2]
  final_state <- state_full[,days]
  
  school_start_state <- state_full[,213] #initial state
  
  res <- list("TimeSeries_I" = TimeSeries_I, "TimeSeries_IC" = TimeSeries_IC, "TimeSeries_M" = TimeSeries_M,
              "TimeSeries_Hosp" = TimeSeries_Hosp,
                "CumTotal_I" = CumTotal_I, "CumTotal_IC" = CumTotal_IC, "CumTotal_M" = CumTotal_M,
              "CumSchoolHH_I" = CumSchoolHH_I, "CumSchoolHH_IC" = CumSchoolHH_IC, "CumSchoolHH_M" = CumSchoolHH_M,
              "school_start_state" = school_start_state, "final_state" = final_state)
  return(res)
}