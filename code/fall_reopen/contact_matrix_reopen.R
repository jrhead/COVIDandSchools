#Function to return an NxN sparse matrix indicating contacts between pairs in the synthetic population
# Contact matrices returned are:
#   1. Household
#   2. Work (essential + non-essential)
#   3-5. School/Grade/Class
#   6. Community
#   7-9. School/Grade/Class groups that arrive in shifts (hybrid schedule of half classes)
#   10-11. Work (membership in essential workplaces only for strong restrictions & moderate restrictions on work)

Rcontact_matrix <- function(synth_pop_df, #dataframe of synthetic population
           HHcontact, # contact rate among HH members (=1)
           SCHcontact,# contact rate among school members not in same grade (=1/35)
           WORKcontact, # contact rate among agents in same workplace (= 5/7)
           CLASScontact, # contact rate among agents in same class (=5/7 - (SCHcontact + GRADEcontact))
           GRADEcontact, # daily contact rate among agents in same grade (=1/7 - SCHcontact)
           AgeContRates, # community contact rates by age
           propEssential, #porportion of workplaces deemed essential during initial shelter-in-place orders
           propMidEssential, # proportion of workplaces allowed for in-person work later on in epidemic
           NWork #number of workplaces
  ){
    
  require(Matrix)
  
  # number of agents
  N = nrow(synth_pop_df)
  agents = 1:nrow(synth_pop_df)
  
  ## 1. Determine household contact matrix
  HH <- as.numeric(as.character(synth_pop_df$HH))
  HHsize = tapply(HH,HH,length) 
  
  agentsHH = agents[HH %in% which(HHsize > 1)]
  HHdo = HH[HH %in% which(HHsize > 1)]
  
  #list of agents that share membership in a household
  HH.cont<-t(do.call(cbind,tapply(agentsHH,HHdo,combn,m=2)))
  
  #household contact matrix based on list of shared membership
  matrixHH <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(HH.cont)[1]){
    matrixHH[HH.cont[i,1],HH.cont[i,2]] <- HHcontact
    matrixHH[HH.cont[i,2],HH.cont[i,1]] <- HHcontact
  }
 
  ## 2. Determine school contact matrix
  School <- as.numeric(as.character(synth_pop_df$School))
  
  Schsize = tapply(School,School,length) 
  Schsize = Schsize[Schsize > 1]
  agentsSch = agents[School %in% names(Schsize)]
  Schdo = School[School %in% names(Schsize)]
  
  #list of agents that share membership in a school
  Sch.cont<-t(do.call(cbind,tapply(agentsSch,Schdo,combn,m=2)))
  
  #school contact matrix based on list of shared membership
  matrixSCH <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(Sch.cont)[1]){
    matrixSCH[Sch.cont[i,1],Sch.cont[i,2]] <- SCHcontact
    matrixSCH[Sch.cont[i,2],Sch.cont[i,1]] <- SCHcontact
  }
  
  ## 3. Determine work contact matrix
  Work <- as.numeric(as.character(synth_pop_df$Work))
  
  Wrksize = tapply(Work,Work,length) 
  Wrksize = Wrksize[Wrksize > 1]
  agentsWrk = agents[Work %in% names(Wrksize)]
  Wrkdo = Work[Work %in% names(Wrksize)]
  
  #list of agents that share membership in a workplace
  Wrk.cont<-t(do.call(cbind,tapply(agentsWrk,Wrkdo,combn,m=2)))
  
  #work contact matrix based on list of shared membership
  matrixWORK <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(Wrk.cont)[1]){
    matrixWORK[Wrk.cont[i,1],Wrk.cont[i,2]] <- WORKcontact
    matrixWORK[Wrk.cont[i,2],Wrk.cont[i,1]] <- WORKcontact
  }
  
  ## 4. Work contact matrix for essential workers only -- most restrictive level
  
  #determine if workplace is essential
  EWrkN <- sample(1:NWork, round(propEssential*NWork,0), replace = F)
  EWrksize <- Wrksize[EWrkN]
  
  agentsEWrk = agents[Work %in% names(EWrksize)]
  EWrkdo = Work[Work %in% names(EWrksize)]
  
  #list of agents that share membership in a shared essential workplace
  EWrk.cont<-t(do.call(cbind,tapply(agentsEWrk,EWrkdo,combn,m=2)))
  
  #determine which workplaces are essential/in-person during more relaxed intervention
  #combine the always essential with the newly in-person workplaces
  RemainingWork <- (1:NWork)[-EWrkN]
  EWrkNew <- sample(1:NWork, round(propMidEssential*NWork,0), replace = F)
  EWrkN <- c(EWrkN, EWrkNew) #add new ones to old ones
  EWrksize <- Wrksize[EWrkN]
  
  agentsEWrk = agents[Work %in% names(EWrksize)]
  EWrkdo = Work[Work %in% names(EWrksize)]
  
  #list of agents that share membership in a shared in-person workplace
  EWrk.cont.mid<-t(do.call(cbind,tapply(agentsEWrk,EWrkdo,combn,m=2)))

  
  ## 5. Community contact matrix
  Community <- as.numeric(as.character(synth_pop_df$Community))
  AgeCat <- match(as.character(synth_pop_df$AgeCat), rownames(AgeContRates))
  numCommunity <- length(unique(Community))
  
  #age-strucutre matrix
  COMmat <- matrix(0,N,N)
  for(i in 1:N) {
    COMmat[i,] <-  AgeContRates[AgeCat[i], AgeCat]
  }
  diag(COMmat) <- 0
  
  ## 6. Grade contact matrix 
  Grade <- as.numeric(as.character(synth_pop_df$Grade))
  #create unique ID for grade+school
  Gradesch = paste(Grade,School)
  #school admin only assigned a school -- remove them from grade contacts
  Gradesch[grepl('NA',Gradesch)] = NA
  Grdsize = tapply(Gradesch,Gradesch,length) 
  Grdsize = Grdsize[Grdsize > 1]
  agentsGrd = agents[Gradesch %in% names(Grdsize)]
  Grddo = Gradesch[Gradesch %in% names(Grdsize)]
  
  #list of agents that share membership in a grade
  Grd.cont<-t(do.call(cbind,tapply(agentsGrd,Grddo,combn,m=2)))
  
  #grade contact matrix based on list of shared membership
  matrixGRD <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(Grd.cont)[1]){
    matrixGRD[Grd.cont[i,1],Grd.cont[i,2]] <- GRADEcontact
    matrixGRD[Grd.cont[i,2],Grd.cont[i,1]] <- GRADEcontact
  }

  ## 7. Grade contact matrix 
  Class <- as.numeric(as.character(synth_pop_df$Class))
  #create unique ID for class+grade+school
  ClsGrdSch = paste(Class,Grade,School)
  #school admin only assigned a school -- remove them from class contacts
  ClsGrdSch[grepl('NA',ClsGrdSch)] = NA
  Clssize = tapply(ClsGrdSch,ClsGrdSch,length) 
  Clssize = Clssize[Clssize > 1]
  agentsCls = agents[ClsGrdSch %in% names(Clssize)]
  Clsdo = ClsGrdSch[ClsGrdSch %in% names(Clssize)]
  
  #list of agents that share membership in a class
  Cls.cont<-t(do.call(cbind,tapply(agentsCls,Clsdo,combn,m=2)))
  
  #class contact matrix based on list of shared membership
  matrixCLS <- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(Cls.cont)[1]){
    matrixCLS[Cls.cont[i,1],Cls.cont[i,2]] <- CLASScontact
    matrixCLS[Cls.cont[i,2],Cls.cont[i,1]] <- CLASScontact
  }
  
  ### 8 - 10. Shifts of school, grade, and class; assuming 2 days shifts
  
  # For half class shifts: admin and teachers come 4 days per week, & contact both shifts
  #         in synth_pop_reopen.R, admin AND teachers assigned shift = 0
  
  # For shifts by grade groups: admin come 4 days per week & contact both shifts
  #                             teachers come 2 days per week, just with their classrooms
  #         in synth_pop_reopen_gradeshifts.R, admin ONLY assigned shift = 0; 
  #                                            admin only have school level contact with others

  ## 8. School -- 

  #For Shift = 1
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  admin <- agents[which(Shift == 0)] # teachers and students if half-classes; just admin for grade groups
  Shift[admin] <- 1
  Shift[Shift == 2] <- NA
  
  SchShft = paste(School,Shift)
  SchShft[grepl('NA',SchShft)] = NA
  SchShftsize = tapply(SchShft,SchShft,length) 
  SchShftsize = SchShftsize[SchShftsize > 1]
  agentsSchShft = agents[SchShft %in% names(SchShftsize)]
  SchShftdo = SchShft[SchShft %in% names(SchShftsize)]
  
  #list of agents in same school (shift 1)
  SchShft1.cont<-t(do.call(cbind,tapply(agentsSchShft,SchShftdo,combn,m=2)))
  
  #For Shift = 2
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  admin <- agents[which(Shift == 0)]
  Shift[admin] <- 2
  Shift[Shift == 1] <- NA

  SchShft = paste(School,Shift)
  SchShft[grepl('NA',SchShft)] = NA
  SchShftsize = tapply(SchShft,SchShft,length) 
  SchShftsize = SchShftsize[SchShftsize > 1]
  agentsSchShft = agents[SchShft %in% names(SchShftsize)]
  SchShftdo = SchShft[SchShft %in% names(SchShftsize)]
  
  #list of agents in same school (shift 2)
  SchShft2.cont<-t(do.call(cbind,tapply(agentsSchShft,SchShftdo,combn,m=2)))
  
  #Combine to make one matrix
  SchShft.cont<- rbind(SchShft1.cont, SchShft2.cont)
  
  #school matrix when half-class shifts are in place
  matrixSchShft<- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(SchShft.cont)[1]){
    matrixSchShft[SchShft.cont[i,1],SchShft.cont[i,2]] <- SCHcontact
    matrixSchShft[SchShft.cont[i,2],SchShft.cont[i,1]] <- SCHcontact
  }
  
  ## 9. Grade -- 
  # half class shifts: teachers contact both shifts so have membership in shifts 1 & 2
  
  #For Shift = 1
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  teachers <- agents[which(Shift == 0 & !is.na(Class))] #teachers + admin assigned shift of 0 if half-class shifts; admin only else
  Shift[teachers] <- 1
  Shift[Shift == 2] <- NA
  
  GrdShft = paste(Grade,School,Shift)
  GrdShft[grepl('NA',GrdShft)] = NA
  GrdShftsize = tapply(GrdShft,GrdShft,length) 
  GrdShftsize = GrdShftsize[GrdShftsize > 1]
  agentsGrdShft = agents[GrdShft %in% names(GrdShftsize)]
  GrdShftdo = GrdShft[GrdShft %in% names(GrdShftsize)]
  
  #list of agents in same grade (shift 1)
  GrdShft1.cont<-t(do.call(cbind,tapply(agentsGrdShft,GrdShftdo,combn,m=2)))

  #For Shift = 2
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  teachers <- agents[which(Shift == 0 & !is.na(Class))]
  Shift[teachers] <- 2
  Shift[Shift == 1] <- NA
  
  GrdShft = paste(Grade,School,Shift)
  GrdShft[grepl('NA',GrdShft)] = NA
  GrdShftsize = tapply(GrdShft,GrdShft,length) 
  GrdShftsize = GrdShftsize[GrdShftsize > 1]
  agentsGrdShft = agents[GrdShft %in% names(GrdShftsize)]
  GrdShftdo = GrdShft[GrdShft %in% names(GrdShftsize)]
  
  #list of agents in same grade (shift 2)
  GrdShft2.cont<-t(do.call(cbind,tapply(agentsGrdShft,GrdShftdo,combn,m=2)))

  #Combine to make one matrix
  GrdShft.cont<- rbind(GrdShft1.cont, GrdShft2.cont)
  
  #grade matrix when half-class shifts are in place
  matrixGrdShft<- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(GrdShft.cont)[1]){
    matrixGrdShft[GrdShft.cont[i,1],GrdShft.cont[i,2]] <- GRADEcontact
    matrixGrdShft[GrdShft.cont[i,2],GrdShft.cont[i,1]] <- GRADEcontact
  }

  
  ## 10. Class -- 
  # half-class shifts: teachers contact both shifts so have membership in shifts 1 & 2
  
  #For Shift = 1
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  teachers <- agents[which(Shift == 0 & !is.na(Class))] #teachers + admin assigned shift of 0 if half-class shifts; admin only else
  Shift[teachers] <- 1
  Shift[Shift == 2] <- NA
  
  ClsShft = paste(Class,Grade,School,Shift)
  ClsShft[grepl('NA',ClsShft)] = NA
  ClsShftsize = tapply(ClsShft,ClsShft,length) 
  ClsShftsize = ClsShftsize[ClsShftsize > 1]
  agentsClsShft = agents[ClsShft %in% names(ClsShftsize)]
  ClsShftdo = ClsShft[ClsShft %in% names(ClsShftsize)]
  
  #list of agents in same class (shift 1)
  ClsShft1.cont<-t(do.call(cbind,tapply(agentsClsShft,ClsShftdo,combn,m=2)))
  
  #For Shift = 2
  Shift <- as.numeric(as.character(synth_pop_df$Shift))
  teachers <- agents[which(Shift == 0 & !is.na(Class))]
  Shift[teachers] <- 2
  Shift[Shift == 1] <- NA
  
  ClsShft = paste(Class,Grade,School,Shift)
  ClsShft[grepl('NA',ClsShft)] = NA
  ClsShftsize = tapply(ClsShft,ClsShft,length) 
  ClsShftsize = ClsShftsize[ClsShftsize > 1]
  agentsClsShft = agents[ClsShft %in% names(ClsShftsize)]
  ClsShftdo = ClsShft[ClsShft %in% names(ClsShftsize)]
  
  #list of agents in same class (shift 2)
  ClsShft2.cont<-t(do.call(cbind,tapply(agentsClsShft,ClsShftdo,combn,m=2)))
  
  #Combine to make one matrix
  ClsShft.cont<- rbind(ClsShft1.cont, ClsShft2.cont)
  
  #class matrix when half-class shifts are in place
  matrixClsShft<- matrix(0, ncol = N, nrow = N)
  for (i in 1:dim(ClsShft.cont)[1]){
    matrixClsShft[ClsShft.cont[i,1],ClsShft.cont[i,2]] <- CLASScontact
    matrixClsShft[ClsShft.cont[i,2],ClsShft.cont[i,1]] <- CLASScontact
  }
  
  ####################
  # Convert to sparse matrices
  matrixHHsp <- as(matrixHH, "sparseMatrix")
  matrixWORKsp <- as(matrixWORK, "sparseMatrix")
  matrixSCHsp <- as(matrixSCH, "sparseMatrix")
  matrixGRDsp <- as(matrixGRD, "sparseMatrix")
  matrixCLSsp <- as(matrixCLS, "sparseMatrix")
  
  matrixSchShftsp <- as(matrixSchShft, "sparseMatrix")
  matrixGrdShftsp <- as(matrixGrdShft, "sparseMatrix")
  matrixClsShftsp <- as(matrixClsShft, "sparseMatrix")

  return(list(matrixHHsp, matrixWORKsp, matrixSCHsp, 
              matrixGRDsp, matrixCLSsp, COMmat,
              matrixSchShftsp, matrixGrdShftsp, matrixClsShftsp, 
              EWrk.cont, EWrk.cont.mid))
}



