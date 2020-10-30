###################################################################################################
##   Functions for absolute excess cases/hospitalizations/deaths by subgroup and intervention    ##
###################################################################################################

#function to calculate the mean and confidence interval across all simulations of the 
# total excess seroprevalence, and who comprises it, using scenarios in main_reopening_scenarios.R
calc_excess_seroprev <- function(outcomes, #.RData containing results of model simulations
                                 lci = 0.025, #lower CI across simulations
                                 uci = 0.975 #upper CI across simulations
                                 ){
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTT7 <- allocTB7<- allocTT30 <-allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocOPEN <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocTT7 <- 
    p.allocTB7<- p.allocTT30 <- p.allocTB30 <-  p.allocMASK <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTT7 <- AT.allocTB7<- AT.allocTT30 <- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocOPEN <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocTT7 <- 
    AT.p.allocTB7<- AT.p.allocTT30 <- AT.p.allocTB30 <-  AT.p.allocMASK <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(unlist(outcomes[[i]][13])!= "A" & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"),
                        sum(NewCaseOPEN == 1 & Classification == "Community member"))
    
    p.allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseOPEN == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseOPEN == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseOPEN == 1 & Classification == "Community member")/NCM)
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] - allocCLOSED[[i]] 
    AT.p.allocOPEN[[i]] <-  p.allocOPEN[[i]] - p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]]- allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]]- p.allocCLOSED[[i]] 
    
    ##
    
    allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTT7 == 1 & Classification == "Student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTT7 == 1 & Classification == "HH member"),
                       sum(NewCaseTT7 == 1 & Classification == "Community member"))
    
    p.allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTT7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTT7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTT7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT7[[i]] <-  allocTT7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT7[[i]] <-  p.allocTT7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"),
                       sum(NewCaseTB7 == 1 & Classification == "Community member"))
    
    p.allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTB7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTB7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTB7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB7[[i]] <-  p.allocTB7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTT30 == 1 & Classification == "Student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTT30 == 1 & Classification == "HH member"),
                        sum(NewCaseTT30 == 1 & Classification == "Community member"))
    
    p.allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTT30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTT30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT30[[i]] <-  allocTT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT30[[i]] <-  p.allocTT30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"),
                        sum(NewCaseTB30 == 1 & Classification == "Community member"))
    
    p.allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTB30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTB30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTB30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB30[[i]] <-  p.allocTB30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"),
                        sum(NewCaseMASK == 1 & Classification == "Community member"))
    
    p.allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseMASK == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseMASK == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseMASK == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseMASK == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseMASK == 1 & Classification == "Community member")/NCM)
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] - allocCLOSED[[i]] 
    AT.p.allocMASK[[i]] <-  p.allocMASK[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]] - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    colMeans(do.call(rbind,allocOPEN)),
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    colMeans(do.call(rbind,allocTT7)),
                    colMeans(do.call(rbind,allocTB7)),
                    colMeans(do.call(rbind,allocTT30)),
                    colMeans(do.call(rbind,allocTB30)),
                    colMeans(do.call(rbind,allocMASK)),
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   colMeans(do.call(rbind,AT.allocOPEN)),
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   colMeans(do.call(rbind,AT.allocTT7)),
                   colMeans(do.call(rbind,AT.allocTB7)),
                   colMeans(do.call(rbind,AT.allocTT30)),
                   colMeans(do.call(rbind,AT.allocTB30)),
                   colMeans(do.call(rbind,AT.allocMASK)),
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                colMeans(do.call(rbind,AT.p.allocOPEN)),
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                colMeans(do.call(rbind,AT.p.allocTT7)),
                colMeans(do.call(rbind,AT.p.allocTB7)),
                colMeans(do.call(rbind,AT.p.allocTT30)),
                colMeans(do.call(rbind,AT.p.allocTB30)),
                colMeans(do.call(rbind,AT.p.allocMASK)),
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),11)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("No additional precautions", 10), rep("Stable cohorts (strong)", 10),
                               rep("2-day half class shifts", 10), rep("2-day half class shifts, stable cohorts", 10),
                               rep("Weekly testing of teachers",10), rep("Weekly testing of teachers + students", 10), 
                               rep("Monthly testing of teachers", 10), rep("Monthly testing of teachers + students", 10), rep("Masks", 10),
                               rep("Stable cohorts (strong), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess seroprevalence, and who comprises it, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_seroprev2 <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]] - p.allocCLOSED[[i]] 
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]]  - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),5)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("Stable cohorts (weak)", 10),
                               rep("2-day staggered grades", 10), rep("2-day staggered grades + stable cohorts", 10),
                               rep("Stable cohorts (weak), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess incidence (symptomatic), and who comprises it, using scenarios in main_reopening_scenarios.R
calc_excess_incidence <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTB7<- allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTB7<- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(unlist(outcomes[[i]][13])!= "A" & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Community member"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"))
    
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Community member"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"))
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Community member"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"))
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Community member"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"))
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    colMeans(do.call(rbind,AT.allocOPEN)),
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    colMeans(do.call(rbind,AT.allocTB7)),
    colMeans(do.call(rbind,AT.allocTB30)),
    colMeans(do.call(rbind,AT.allocMASK)),
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
    colQuantiles(do.call(rbind,AT.allocOPEN),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
    colQuantiles(do.call(rbind,AT.allocTB7),probs = lci),
    colQuantiles(do.call(rbind,AT.allocTB30),probs = lci),
    colQuantiles(do.call(rbind,AT.allocMASK),probs = lci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
    colQuantiles(do.call(rbind,AT.allocOPEN),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
    colQuantiles(do.call(rbind,AT.allocTB7),probs = uci),
    colQuantiles(do.call(rbind,AT.allocTB30),probs = uci),
    colQuantiles(do.call(rbind,AT.allocMASK),probs = uci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  
  
  allocation.clin <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  
  allocation.clin$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),8)
  allocation.clin$Intervention <- c(rep("No additional precautions", 4), rep("Stable cohorts (strong)", 4),
                    rep("2-day half class shifts", 4), rep("2-day half class shifts, stable cohorts", 4),
                    rep("Weekly testing of teachers + students", 4), 
                    rep("Monthly testing of teachers + students", 4), rep("Masks", 4),
                    rep("Stable cohorts (strong), masks, monthly teacher testing", 4))

  return(allocation.clin)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess incidence (symptomatic), and who comprises it, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_incidence2 <- function(outcomes){
  reps <- length(outcomes)
  
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED  <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
     AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
   
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
    colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
    colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  
  allocation.clin <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  allocation.clin$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),4)
  allocation.clin$Intervention <- c(rep("Stable cohorts (weak)", 4),
                    rep("2-day staggered grades", 4), rep("2-day staggered grades + stable cohorts", 4),
                    rep("Stable cohorts (weak), masks, monthly teacher testing", 4))

  return(allocation.clin)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess hospitalizations, and who comprises it, using scenarios in main_reopening_scenarios.R
calc_excess_hosp <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTB7<- allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTB7<- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Community member"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"))
    
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Community member"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"))
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Community member"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"))
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Community member"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"))
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    colMeans(do.call(rbind,AT.allocOPEN)),
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    colMeans(do.call(rbind,AT.allocTB7)),
    colMeans(do.call(rbind,AT.allocTB30)),
    colMeans(do.call(rbind,AT.allocMASK)),
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
    colQuantiles(do.call(rbind,AT.allocOPEN),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
    colQuantiles(do.call(rbind,AT.allocTB7),probs = lci),
    colQuantiles(do.call(rbind,AT.allocTB30),probs = lci),
    colQuantiles(do.call(rbind,AT.allocMASK),probs = lci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
    colQuantiles(do.call(rbind,AT.allocOPEN),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
    colQuantiles(do.call(rbind,AT.allocTB7),probs = uci),
    colQuantiles(do.call(rbind,AT.allocTB30),probs = uci),
    colQuantiles(do.call(rbind,AT.allocMASK),probs = uci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  
  
  allocation.hosp <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  allocation.hosp$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),8)
  allocation.hosp$Intervention <- c(rep("No additional precautions", 4), rep("Stable cohorts (strong)", 4),
                    rep("2-day half class shifts", 4), rep("2-day half class shifts, stable cohorts", 4),
                    rep("Weekly testing of teachers + students", 4), 
                    rep("Monthly testing of teachers + students", 4), rep("Masks", 4),
                    rep("Stable cohorts (strong), masks, monthly teacher testing", 4))

  
  return(allocation.hosp)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess hospitalizations, and who comprises it, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_hosp2 <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED  <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
    colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
    colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
    colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  
  allocation.hosp <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  allocation.hosp$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),4)
  allocation.hosp$Intervention <- c(rep("Stable cohorts (weak)", 4),
                    rep("2-day staggered grades", 4), rep("2-day staggered grades + stable cohorts", 4),
                    rep("Stable cohorts (weak), masks, monthly teacher testing", 4))

  
  return(allocation.hosp)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess deaths, and who comprises it, using scenarios in main_reopening_scenarios.R
calc_excess_mort <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTB7<- allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTB7<- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][13]) == "D"  & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Community member"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"))
    
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
    
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Community member"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"))
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Community member"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"))
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Community member"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"))
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    colMeans(do.call(rbind,AT.allocOPEN)),
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    colMeans(do.call(rbind,AT.allocTB7)),
    colMeans(do.call(rbind,AT.allocTB30)),
    colMeans(do.call(rbind,AT.allocMASK)),
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
                    colQuantiles(do.call(rbind,AT.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
              colQuantiles(do.call(rbind,AT.allocOPEN),probs = uci),
              colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
              colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
              colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
              colQuantiles(do.call(rbind,AT.allocTB7),probs = uci),
              colQuantiles(do.call(rbind,AT.allocTB30),probs = uci),
              colQuantiles(do.call(rbind,AT.allocMASK),probs = uci),
              colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  

  allocation.mort <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  allocation.mort$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),8)
  allocation.mort$Intervention <- c(rep("No additional precautions", 4), rep("Stable cohorts (strong)", 4),
                    rep("2-day half class shifts", 4), rep("2-day half class shifts, stable cohorts", 4),
                    rep("Weekly testing of teachers + students", 4), 
                    rep("Monthly testing of teachers + students", 4), rep("Masks", 4),
                    rep("Stable cohorts (strong), masks, monthly teacher testing", 4))


  
  return(allocation.mort)
}

#function to calculate the mean and confidence interval across all simulations of the 
# total excess deaths, and who comprises it, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_mort2 <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED  <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher" 
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[(synth_pop_i$HH %in% HHteacher | synth_pop_i$HH %in% HHstudent) 
                   & Classification == "Community member"] <- "HH member" 
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Community member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"))
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Community member"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"))
    
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] -  allocCLOSED[[i]]
    
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Community member"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"))
    
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] -  allocCLOSED[[i]]
    
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Community member"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"))
    
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] -  allocCLOSED[[i]]
    
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"))
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] -  allocCLOSED[[i]]
    
  }
  
  #Create summary of cases averted
  
  Attribution <- c(
    
    colMeans(do.call(rbind,AT.allocInt1)),
    colMeans(do.call(rbind,AT.allocInt2)),
    colMeans(do.call(rbind,AT.allocInt3)),
    
    colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  LwerCI <- c(
              colQuantiles(do.call(rbind,AT.allocInt1),probs = lci),
              colQuantiles(do.call(rbind,AT.allocInt2),probs = lci),
              colQuantiles(do.call(rbind,AT.allocInt3),probs = lci),
              
              colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = lci))
  
  UpperCI <- c(
               colQuantiles(do.call(rbind,AT.allocInt1),probs = uci),
               colQuantiles(do.call(rbind,AT.allocInt2),probs = uci),
               colQuantiles(do.call(rbind,AT.allocInt3),probs = uci),
              
               colQuantiles(do.call(rbind,AT.allocM.S1.TT30),probs = uci))
  
  

  allocation.mort <- as.data.frame(cbind(Attribution, LwerCI, UpperCI))
  
  allocation.mort$Classification <- rep(c("Community member", "Student", "Teacher", "HH member"),4)
  allocation.mort$Intervention <- c(rep("Stable cohorts (weak)", 4),
                                    rep("2-day staggered grades", 4), rep("2-day staggered grades + stable cohorts", 4),
                                    rep("Stable cohorts (weak), masks, monthly teacher testing", 4))

  
  return(allocation.mort)
}

######################################################################################################
##   Functions for excess risk of infection/hospitalizations/deaths by subgroup and intervention    ##
######################################################################################################

#function to calculate the mean and confidence interval across all simulations of the 
# excess seroprevalence per group, using scenarios in main_reopening_scenarios.R
calc_excess_risk_seroprev <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTT7 <- allocTB7<- allocTT30 <-allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocOPEN <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocTT7 <- 
    p.allocTB7<- p.allocTT30 <- p.allocTB30 <-  p.allocMASK <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTT7 <- AT.allocTB7<- AT.allocTT30 <- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocOPEN <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocTT7 <- 
    AT.p.allocTB7<- AT.p.allocTT30 <- AT.p.allocTB30 <-  AT.p.allocMASK <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse(outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse(outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"),
                        sum(NewCaseOPEN == 1 & Classification == "Community member"))
    
    p.allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseOPEN == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseOPEN == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseOPEN == 1 & Classification == "Community member")/NCM)
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] - allocCLOSED[[i]] 
    AT.p.allocOPEN[[i]] <-  p.allocOPEN[[i]] - p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]]- allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]]- p.allocCLOSED[[i]] 
    
    ##
    
    allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTT7 == 1 & Classification == "Student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTT7 == 1 & Classification == "HH member"),
                       sum(NewCaseTT7 == 1 & Classification == "Community member"))
    
    p.allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTT7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTT7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTT7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT7[[i]] <-  allocTT7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT7[[i]] <-  p.allocTT7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"),
                       sum(NewCaseTB7 == 1 & Classification == "Community member"))
    
    p.allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTB7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTB7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTB7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB7[[i]] <-  p.allocTB7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTT30 == 1 & Classification == "Student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTT30 == 1 & Classification == "HH member"),
                        sum(NewCaseTT30 == 1 & Classification == "Community member"))
    
    p.allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTT30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTT30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT30[[i]] <-  allocTT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT30[[i]] <-  p.allocTT30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"),
                        sum(NewCaseTB30 == 1 & Classification == "Community member"))
    
    p.allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTB30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTB30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTB30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB30[[i]] <-  p.allocTB30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"),
                        sum(NewCaseMASK == 1 & Classification == "Community member"))
    
    p.allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseMASK == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseMASK == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseMASK == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseMASK == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseMASK == 1 & Classification == "Community member")/NCM)
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] - allocCLOSED[[i]] 
    AT.p.allocMASK[[i]] <-  p.allocMASK[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]] - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    colMeans(do.call(rbind,allocOPEN)),
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    colMeans(do.call(rbind,allocTT7)),
                    colMeans(do.call(rbind,allocTB7)),
                    colMeans(do.call(rbind,allocTT30)),
                    colMeans(do.call(rbind,allocTB30)),
                    colMeans(do.call(rbind,allocMASK)),
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   colMeans(do.call(rbind,AT.allocOPEN)),
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   colMeans(do.call(rbind,AT.allocTT7)),
                   colMeans(do.call(rbind,AT.allocTB7)),
                   colMeans(do.call(rbind,AT.allocTT30)),
                   colMeans(do.call(rbind,AT.allocTB30)),
                   colMeans(do.call(rbind,AT.allocMASK)),
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                colMeans(do.call(rbind,AT.p.allocOPEN)),
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                colMeans(do.call(rbind,AT.p.allocTT7)),
                colMeans(do.call(rbind,AT.p.allocTB7)),
                colMeans(do.call(rbind,AT.p.allocTT30)),
                colMeans(do.call(rbind,AT.p.allocTB30)),
                colMeans(do.call(rbind,AT.p.allocMASK)),
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),11)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("No additional precautions", 10), rep("Stable cohorts (strong)", 10),
                               rep("2-day half class shifts", 10), rep("2-day half class shifts, stable cohorts", 10),
                               rep("Weekly testing of teachers",10), rep("Weekly testing of teachers + students", 10), 
                               rep("Monthly testing of teachers", 10), rep("Monthly testing of teachers + students", 10), rep("Masks", 10),
                               rep("Stable cohorts (strong), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess seroprevalence per group, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_risk_seroprev2 <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]] - p.allocCLOSED[[i]] 
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]]  - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),5)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("Stable cohorts (weak)", 10),
                               rep("2-day staggered grades", 10), rep("2-day staggered grades + stable cohorts", 10),
                               rep("Stable cohorts (weak), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess symptomatic infection per group, using scenarios in main_reopening_scenarios.R
calc_excess_risk <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTT7 <- allocTB7<- allocTT30 <-allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocOPEN <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocTT7 <- 
    p.allocTB7<- p.allocTT30 <- p.allocTB30 <-  p.allocMASK <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTT7 <- AT.allocTB7<- AT.allocTT30 <- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocOPEN <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocTT7 <- 
    AT.p.allocTB7<- AT.p.allocTT30 <- AT.p.allocTB30 <-  AT.p.allocMASK <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][13]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][13]) != "A" &outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"),
                        sum(NewCaseOPEN == 1 & Classification == "Community member"))
    
    p.allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseOPEN == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseOPEN == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseOPEN == 1 & Classification == "Community member")/NCM)
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] - allocCLOSED[[i]] 
    AT.p.allocOPEN[[i]] <-  p.allocOPEN[[i]] - p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]]- allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]]- p.allocCLOSED[[i]] 
    
    ##
    
    allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTT7 == 1 & Classification == "Student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTT7 == 1 & Classification == "HH member"),
                       sum(NewCaseTT7 == 1 & Classification == "Community member"))
    
    p.allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTT7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTT7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTT7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT7[[i]] <-  allocTT7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT7[[i]] <-  p.allocTT7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"),
                       sum(NewCaseTB7 == 1 & Classification == "Community member"))
    
    p.allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTB7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTB7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTB7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB7[[i]] <-  p.allocTB7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTT30 == 1 & Classification == "Student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTT30 == 1 & Classification == "HH member"),
                        sum(NewCaseTT30 == 1 & Classification == "Community member"))
    
    p.allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTT30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTT30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT30[[i]] <-  allocTT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT30[[i]] <-  p.allocTT30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"),
                        sum(NewCaseTB30 == 1 & Classification == "Community member"))
    
    p.allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTB30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTB30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTB30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB30[[i]] <-  p.allocTB30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"),
                        sum(NewCaseMASK == 1 & Classification == "Community member"))
    
    p.allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseMASK == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseMASK == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseMASK == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseMASK == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseMASK == 1 & Classification == "Community member")/NCM)
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] - allocCLOSED[[i]] 
    AT.p.allocMASK[[i]] <-  p.allocMASK[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]] - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    colMeans(do.call(rbind,allocOPEN)),
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    colMeans(do.call(rbind,allocTT7)),
                    colMeans(do.call(rbind,allocTB7)),
                    colMeans(do.call(rbind,allocTT30)),
                    colMeans(do.call(rbind,allocTB30)),
                    colMeans(do.call(rbind,allocMASK)),
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   colMeans(do.call(rbind,AT.allocOPEN)),
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   colMeans(do.call(rbind,AT.allocTT7)),
                   colMeans(do.call(rbind,AT.allocTB7)),
                   colMeans(do.call(rbind,AT.allocTT30)),
                   colMeans(do.call(rbind,AT.allocTB30)),
                   colMeans(do.call(rbind,AT.allocMASK)),
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                colMeans(do.call(rbind,AT.p.allocOPEN)),
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                colMeans(do.call(rbind,AT.p.allocTT7)),
                colMeans(do.call(rbind,AT.p.allocTB7)),
                colMeans(do.call(rbind,AT.p.allocTT30)),
                colMeans(do.call(rbind,AT.p.allocTB30)),
                colMeans(do.call(rbind,AT.p.allocMASK)),
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),11)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("No additional precautions", 10), rep("Stable cohorts (strong)", 10),
                               rep("2-day half class shifts", 10), rep("2-day half class shifts, stable cohorts", 10),
                               rep("Weekly testing of teachers",10), rep("Weekly testing of teachers + students", 10), 
                               rep("Monthly testing of teachers", 10), rep("Monthly testing of teachers + students", 10), rep("Masks", 10),
                               rep("Stable cohorts (strong), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the  
# excess risk of symptomatic infection per group, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_risk2 <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][7]) != "A" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]] - p.allocCLOSED[[i]] 
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]]  - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),5)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("Stable cohorts (weak)", 10),
                               rep("2-day staggered grades", 10), rep("2-day staggered grades + stable cohorts", 10),
                               rep("Stable cohorts (weak), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess risk of hospitalization per group, using scenarios in main_reopening_scenariose.R
calc_excess_risk_hosp <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTT7 <- allocTB7<- allocTT30 <-allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocOPEN <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocTT7 <- 
    p.allocTB7<- p.allocTT30 <- p.allocTB30 <-  p.allocMASK <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTT7 <- AT.allocTB7<- AT.allocTT30 <- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocOPEN <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocTT7 <- 
    AT.p.allocTB7<- AT.p.allocTT30 <- AT.p.allocTB30 <-  AT.p.allocMASK <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse((unlist(outcomes[[i]][13]) == "H" |unlist(outcomes[[i]][13]) == "D") & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"),
                        sum(NewCaseOPEN == 1 & Classification == "Community member"))
    
    p.allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseOPEN == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseOPEN == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseOPEN == 1 & Classification == "Community member")/NCM)
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] - allocCLOSED[[i]] 
    AT.p.allocOPEN[[i]] <-  p.allocOPEN[[i]] - p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]]- allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]]- p.allocCLOSED[[i]] 
    
    ##
    
    allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTT7 == 1 & Classification == "Student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTT7 == 1 & Classification == "HH member"),
                       sum(NewCaseTT7 == 1 & Classification == "Community member"))
    
    p.allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTT7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTT7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTT7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT7[[i]] <-  allocTT7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT7[[i]] <-  p.allocTT7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"),
                       sum(NewCaseTB7 == 1 & Classification == "Community member"))
    
    p.allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTB7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTB7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTB7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB7[[i]] <-  p.allocTB7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTT30 == 1 & Classification == "Student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTT30 == 1 & Classification == "HH member"),
                        sum(NewCaseTT30 == 1 & Classification == "Community member"))
    
    p.allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTT30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTT30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT30[[i]] <-  allocTT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT30[[i]] <-  p.allocTT30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"),
                        sum(NewCaseTB30 == 1 & Classification == "Community member"))
    
    p.allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTB30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTB30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTB30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB30[[i]] <-  p.allocTB30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"),
                        sum(NewCaseMASK == 1 & Classification == "Community member"))
    
    p.allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseMASK == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseMASK == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseMASK == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseMASK == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseMASK == 1 & Classification == "Community member")/NCM)
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] - allocCLOSED[[i]] 
    AT.p.allocMASK[[i]] <-  p.allocMASK[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]] - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    colMeans(do.call(rbind,allocOPEN)),
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    colMeans(do.call(rbind,allocTT7)),
                    colMeans(do.call(rbind,allocTB7)),
                    colMeans(do.call(rbind,allocTT30)),
                    colMeans(do.call(rbind,allocTB30)),
                    colMeans(do.call(rbind,allocMASK)),
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   colMeans(do.call(rbind,AT.allocOPEN)),
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   colMeans(do.call(rbind,AT.allocTT7)),
                   colMeans(do.call(rbind,AT.allocTB7)),
                   colMeans(do.call(rbind,AT.allocTT30)),
                   colMeans(do.call(rbind,AT.allocTB30)),
                   colMeans(do.call(rbind,AT.allocMASK)),
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                colMeans(do.call(rbind,AT.p.allocOPEN)),
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                colMeans(do.call(rbind,AT.p.allocTT7)),
                colMeans(do.call(rbind,AT.p.allocTB7)),
                colMeans(do.call(rbind,AT.p.allocTT30)),
                colMeans(do.call(rbind,AT.p.allocTB30)),
                colMeans(do.call(rbind,AT.p.allocMASK)),
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),11)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("No additional precautions", 10), rep("Stable cohorts (strong)", 10),
                               rep("2-day half class shifts", 10), rep("2-day half class shifts, stable cohorts", 10),
                               rep("Weekly testing of teachers",10), rep("Weekly testing of teachers + students", 10), 
                               rep("Monthly testing of teachers", 10), rep("Monthly testing of teachers + students", 10), rep("Masks", 10),
                               rep("Stable cohorts (strong), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess risk of hospitalization per group, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_risk_hosp2 <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse((unlist(outcomes[[i]][7]) == "H" |unlist(outcomes[[i]][7]) == "D") & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]] - p.allocCLOSED[[i]] 
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]]  - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),5)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("Stable cohorts (weak)", 10),
                               rep("2-day staggered grades", 10), rep("2-day staggered grades + stable cohorts", 10),
                               rep("Stable cohorts (weak), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess risk of death per group, using scenarios in main_reopening_scenarios.R
calc_excess_risk_mort <- function(outcomes, lci = 0.025, uci = 0.975){
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocOPEN <- allocInt1 <-allocInt2 <- allocInt3 <- 
    allocTT7 <- allocTB7<- allocTT30 <-allocTB30 <- allocMASK <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocOPEN <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocTT7 <- 
    p.allocTB7<- p.allocTT30 <- p.allocTB30 <-  p.allocMASK <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocOPEN <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- 
    AT.allocTT7 <- AT.allocTB7<- AT.allocTT30 <- AT.allocTB30 <- AT.allocMASK <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocOPEN <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocTT7 <- 
    AT.p.allocTB7<- AT.p.allocTT30 <- AT.p.allocTB30 <-  AT.p.allocMASK <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][12])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseOPEN <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$OPEN$school_start_state == "S" & outcomes[[i]]$OPEN$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT7 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$TT7$school_start_state == "S" & outcomes[[i]]$TT7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB7 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$TB7$school_start_state == "S" & outcomes[[i]]$TB7$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTT30 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$TT30$school_start_state == "S" & outcomes[[i]]$TT30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseTB30 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$TB30$school_start_state == "S" & outcomes[[i]]$TB30$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseMASK <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$MASK$school_start_state == "S" & outcomes[[i]]$MASK$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][13]) == "D" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseOPEN == 1 & Classification == "Student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "High school student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Middle student"),
                        sum(NewCaseOPEN == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseOPEN == 1 & Classification == "HH member"),
                        sum(NewCaseOPEN == 1 & Classification == "Community member"))
    
    p.allocOPEN[[i]] <- c(sum(NewCaseOPEN == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseOPEN == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseOPEN == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseOPEN == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseOPEN == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseOPEN == 1 & Classification == "Community member")/NCM)
    
    AT.allocOPEN[[i]] <-  allocOPEN[[i]] - allocCLOSED[[i]] 
    AT.p.allocOPEN[[i]] <-  p.allocOPEN[[i]] - p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]]- allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]]- p.allocCLOSED[[i]] 
    
    ##
    
    allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTT7 == 1 & Classification == "Student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTT7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTT7 == 1 & Classification == "HH member"),
                       sum(NewCaseTT7 == 1 & Classification == "Community member"))
    
    p.allocTT7[[i]] <- c(sum(NewCaseTT7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTT7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTT7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTT7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTT7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTT7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT7[[i]] <-  allocTT7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT7[[i]] <-  p.allocTT7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher"),
                       sum(NewCaseTB7 == 1 & Classification == "Student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "High school student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Middle student"),
                       sum(NewCaseTB7 == 1 & Classification2 == "Elementary student"),
                       sum(NewCaseTB7 == 1 & Classification == "HH member"),
                       sum(NewCaseTB7 == 1 & Classification == "Community member"))
    
    p.allocTB7[[i]] <- c(sum(NewCaseTB7 == 1 & Classification == "Teacher")/Nteacher,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school teacher")/NHteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle teacher")/NMteach,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary teacher")/NEteach,
                         sum(NewCaseTB7 == 1 & Classification == "Student")/Nstudent,
                         sum(NewCaseTB7 == 1 & Classification2 == "High school student")/NHstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Middle student")/NMstud,
                         sum(NewCaseTB7 == 1 & Classification2 == "Elementary student")/NEstud,
                         sum(NewCaseTB7 == 1 & Classification == "HH member")/NHHmember,
                         sum(NewCaseTB7 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB7[[i]] <-  allocTB7[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB7[[i]] <-  p.allocTB7[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTT30 == 1 & Classification == "Student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTT30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTT30 == 1 & Classification == "HH member"),
                        sum(NewCaseTT30 == 1 & Classification == "Community member"))
    
    p.allocTT30[[i]] <- c(sum(NewCaseTT30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTT30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTT30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTT30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTT30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTT30[[i]] <-  allocTT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTT30[[i]] <-  p.allocTT30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseTB30 == 1 & Classification == "Student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "High school student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseTB30 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseTB30 == 1 & Classification == "HH member"),
                        sum(NewCaseTB30 == 1 & Classification == "Community member"))
    
    p.allocTB30[[i]] <- c(sum(NewCaseTB30 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseTB30 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseTB30 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseTB30 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseTB30 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseTB30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocTB30[[i]] <-  allocTB30[[i]] - allocCLOSED[[i]] 
    AT.p.allocTB30[[i]] <-  p.allocTB30[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseMASK == 1 & Classification == "Student"),
                        sum(NewCaseMASK == 1 & Classification2 == "High school student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Middle student"),
                        sum(NewCaseMASK == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseMASK == 1 & Classification == "HH member"),
                        sum(NewCaseMASK == 1 & Classification == "Community member"))
    
    p.allocMASK[[i]] <- c(sum(NewCaseMASK == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseMASK == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseMASK == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseMASK == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseMASK == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseMASK == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseMASK == 1 & Classification == "Community member")/NCM)
    
    AT.allocMASK[[i]] <-  allocMASK[[i]] - allocCLOSED[[i]] 
    AT.p.allocMASK[[i]] <-  p.allocMASK[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]] - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    colMeans(do.call(rbind,allocOPEN)),
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    colMeans(do.call(rbind,allocTT7)),
                    colMeans(do.call(rbind,allocTB7)),
                    colMeans(do.call(rbind,allocTT30)),
                    colMeans(do.call(rbind,allocTB30)),
                    colMeans(do.call(rbind,allocMASK)),
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   colMeans(do.call(rbind,AT.allocOPEN)),
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   colMeans(do.call(rbind,AT.allocTT7)),
                   colMeans(do.call(rbind,AT.allocTB7)),
                   colMeans(do.call(rbind,AT.allocTT30)),
                   colMeans(do.call(rbind,AT.allocTB30)),
                   colMeans(do.call(rbind,AT.allocMASK)),
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                colMeans(do.call(rbind,AT.p.allocOPEN)),
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                colMeans(do.call(rbind,AT.p.allocTT7)),
                colMeans(do.call(rbind,AT.p.allocTB7)),
                colMeans(do.call(rbind,AT.p.allocTT30)),
                colMeans(do.call(rbind,AT.p.allocTB30)),
                colMeans(do.call(rbind,AT.p.allocMASK)),
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocOPEN),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB7),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTT30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocTB30),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocMASK),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),11)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("No additional precautions", 10), rep("Stable cohorts (strong)", 10),
                               rep("2-day half class shifts", 10), rep("2-day half class shifts, stable cohorts", 10),
                               rep("Weekly testing of teachers",10), rep("Weekly testing of teachers + students", 10), 
                               rep("Monthly testing of teachers", 10), rep("Monthly testing of teachers + students", 10), rep("Masks", 10),
                               rep("Stable cohorts (strong), masks, monthly teacher testing", 10))
  
  return(allocation)
}

#function to calculate the mean and confidence interval across all simulations of the 
# excess risk of death per group, using scenarios in main_reopening_scenarios_hybridbygrade.R
calc_excess_risk_mort2 <- function(outcomes, lci = 0.025, uci = 0.975){
  
  reps <- length(outcomes)
  
  ####Determine who the excess cases were in...
  allocCLOSED <- allocInt1 <-allocInt2 <- allocInt3 <- allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  p.allocCLOSED <- p.allocInt1 <- p.allocInt2 <- p.allocInt3 <- p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.allocCLOSED <- AT.allocInt1 <- AT.allocInt2 <- AT.allocInt3 <- AT.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  AT.p.allocCLOSED <- AT.p.allocInt1 <- AT.p.allocInt2 <- AT.p.allocInt3 <- AT.p.allocM.S1.TT30 <- vector(mode = "list", length = reps)
  
  
  for (i in 1:reps){
    
    #Define classifications of persons
    Classification <- rep("Community member", N)
    synth_pop_i <- data.frame(outcomes[[i]][6])
    Classification[synth_pop_i$Age < 18 & !is.na(synth_pop_i$School)] <- "Student" 
    Classification[synth_pop_i$Age >= 18 & !is.na(synth_pop_i$School)] <- "Teacher"
    
    HHteacher <- unique(synth_pop_i[which(Classification == "Teacher"),]$HH)
    HHstudent <- unique(synth_pop_i[which(Classification == "Student"),]$HH)
    Classification[synth_pop_i$HH %in% HHstudent & Classification == "Community member" |
                     synth_pop_i$HH %in% HHteacher & Classification == "Community member"] <- "HH member" 
    
    Nstudent <- sum(Classification == "Student")
    Nteacher <- sum(Classification == "Teacher")
    NHHmember <- sum(Classification == "HH member")
    NCM <- sum(Classification == "Community member")
    
    Classification2 <- rep("Other", N)
    synth_pop_i$School <- as.numeric(as.character(synth_pop_i$School))
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School < 4] <- "Elementary teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle teacher" 
    Classification2[synth_pop_i$Age >= 18 & synth_pop_i$School >10000] <- "High school teacher" 
    
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School < 4] <- "Elementary student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >= 5 & synth_pop_i$School <10000] <- "Middle student" 
    Classification2[synth_pop_i$Age < 18 & synth_pop_i$School >10000] <- "High school student" 
    
    NEteach <- sum(Classification2 == "Elementary teacher")
    NMteach <- sum(Classification2 == "Middle teacher")
    NHteach <- sum(Classification2 == "High school teacher")
    
    NEstud <- sum(Classification2 == "Elementary student")
    NMstud <- sum(Classification2 == "Middle student")
    NHstud <- sum(Classification2 == "High school student")
    
    
    #Identify if persons sick originally
    
    NewCaseCLOSED <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$CLOSED$school_start_state == "S" & outcomes[[i]]$CLOSED$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt1 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int1$school_start_state == "S" & outcomes[[i]]$Int1$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt2 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int2$school_start_state == "S" & outcomes[[i]]$Int2$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseInt3 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$Int3$school_start_state == "S" & outcomes[[i]]$Int3$final_state != "S",1,0) #update to have status at start of school too..
    NewCaseM.S1.TT30 <- ifelse(unlist(outcomes[[i]][7]) == "D" & outcomes[[i]]$S1.TT30.Mask$school_start_state == "S" & outcomes[[i]]$S1.TT30.Mask$final_state != "S",1,0) #update to have status at start of school too..
    
    allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher"),
                          sum(NewCaseCLOSED == 1 & Classification == "Student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "High school student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Middle student"),
                          sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student"),
                          sum(NewCaseCLOSED == 1 & Classification == "HH member"),
                          sum(NewCaseCLOSED == 1 & Classification == "Community member"))
    
    p.allocCLOSED[[i]] <- c(sum(NewCaseCLOSED == 1 & Classification == "Teacher")/Nteacher,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school teacher")/NHteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle teacher")/NMteach,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary teacher")/NEteach,
                            sum(NewCaseCLOSED == 1 & Classification == "Student")/Nstudent,
                            sum(NewCaseCLOSED == 1 & Classification2 == "High school student")/NHstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Middle student")/NMstud,
                            sum(NewCaseCLOSED == 1 & Classification2 == "Elementary student")/NEstud,
                            sum(NewCaseCLOSED == 1 & Classification == "HH member")/NHHmember,
                            sum(NewCaseCLOSED == 1 & Classification == "Community member")/NCM)
    
    AT.allocCLOSED[[i]] <-  allocCLOSED[[i]] 
    AT.p.allocCLOSED[[i]] <-  p.allocCLOSED[[i]] 
    
    ##
    allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt1 == 1 & Classification == "Student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt1 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt1 == 1 & Classification == "HH member"),
                        sum(NewCaseInt1 == 1 & Classification == "Community member"))
    
    p.allocInt1[[i]] <- c(sum(NewCaseInt1 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt1 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt1 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt1 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt1 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt1 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt1[[i]] <-  allocInt1[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt1[[i]] <-  p.allocInt1[[i]] - p.allocCLOSED[[i]] 
    ##
    
    allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt2 == 1 & Classification == "Student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt2 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt2 == 1 & Classification == "HH member"),
                        sum(NewCaseInt2 == 1 & Classification == "Community member"))
    
    p.allocInt2[[i]] <- c(sum(NewCaseInt2 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt2 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt2 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt2 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt2 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt2 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt2[[i]] <-  allocInt2[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt2[[i]] <-  p.allocInt2[[i]] - p.allocCLOSED[[i]] 
    ##
    allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher"),
                        sum(NewCaseInt3 == 1 & Classification == "Student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "High school student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Middle student"),
                        sum(NewCaseInt3 == 1 & Classification2 == "Elementary student"),
                        sum(NewCaseInt3 == 1 & Classification == "HH member"),
                        sum(NewCaseInt3 == 1 & Classification == "Community member"))
    
    p.allocInt3[[i]] <- c(sum(NewCaseInt3 == 1 & Classification == "Teacher")/Nteacher,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school teacher")/NHteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle teacher")/NMteach,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary teacher")/NEteach,
                          sum(NewCaseInt3 == 1 & Classification == "Student")/Nstudent,
                          sum(NewCaseInt3 == 1 & Classification2 == "High school student")/NHstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Middle student")/NMstud,
                          sum(NewCaseInt3 == 1 & Classification2 == "Elementary student")/NEstud,
                          sum(NewCaseInt3 == 1 & Classification == "HH member")/NHHmember,
                          sum(NewCaseInt3 == 1 & Classification == "Community member")/NCM)
    
    AT.allocInt3[[i]] <-  allocInt3[[i]] - allocCLOSED[[i]] 
    AT.p.allocInt3[[i]] <-  p.allocInt3[[i]] - p.allocCLOSED[[i]] 
    
    ##
    
    allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member"),
                             sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member"))
    
    p.allocM.S1.TT30[[i]] <- c(sum(NewCaseM.S1.TT30 == 1 & Classification == "Teacher")/Nteacher,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school teacher")/NHteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle teacher")/NMteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary teacher")/NEteach,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Student")/Nstudent,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "High school student")/NHstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Middle student")/NMstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification2 == "Elementary student")/NEstud,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "HH member")/NHHmember,
                               sum(NewCaseM.S1.TT30 == 1 & Classification == "Community member")/NCM)
    
    AT.allocM.S1.TT30[[i]] <-  allocM.S1.TT30[[i]] - allocCLOSED[[i]] 
    AT.p.allocM.S1.TT30[[i]] <-  p.allocM.S1.TT30[[i]]  - p.allocCLOSED[[i]] 
  }
  
  #Create summary of cases averted
  NewExposures <- c(colMeans(do.call(rbind,allocCLOSED)),
                    
                    colMeans(do.call(rbind,allocInt1)),
                    colMeans(do.call(rbind,allocInt2)),
                    colMeans(do.call(rbind,allocInt3)),
                    
                    colMeans(do.call(rbind,allocM.S1.TT30)))
  
  
  Attribution <- c(colMeans(do.call(rbind,AT.allocCLOSED)),
                   
                   colMeans(do.call(rbind,AT.allocInt1)),
                   colMeans(do.call(rbind,AT.allocInt2)),
                   colMeans(do.call(rbind,AT.allocInt3)),
                   
                   colMeans(do.call(rbind,AT.allocM.S1.TT30)))
  
  P.Infect <- c(colMeans(do.call(rbind,AT.p.allocCLOSED)),
                
                colMeans(do.call(rbind,AT.p.allocInt1)),
                colMeans(do.call(rbind,AT.p.allocInt2)),
                colMeans(do.call(rbind,AT.p.allocInt3)),
                
                colMeans(do.call(rbind,AT.p.allocM.S1.TT30)))
  
  P.Infect.Low <- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = lci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = lci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = lci))
  
  P.Infect.High<- c(colQuantiles(do.call(rbind,AT.p.allocCLOSED),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocInt1),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt2),probs = uci),
                    colQuantiles(do.call(rbind,AT.p.allocInt3),probs = uci),
                    
                    colQuantiles(do.call(rbind,AT.p.allocM.S1.TT30),probs = uci))
  
  
  allocation <- as.data.frame(cbind(NewExposures, Attribution, P.Infect, P.Infect.Low, P.Infect.High))
  
  allocation$Classification <- rep(c("Teacher", "High school teacher", "Middle school teacher", "Elementary teacher",
                                     "Student", "High school student", "Middle school student", "Elementary student",
                                     "HH member", "Community member"),5)
  
  allocation$Intervention <- c(rep("Schools closed", 10), rep("Stable cohorts (weak)", 10),
                               rep("2-day staggered grades", 10), rep("2-day staggered grades + stable cohorts", 10),
                               rep("Stable cohorts (weak), masks, monthly teacher testing", 10))
  
  return(allocation)
}
