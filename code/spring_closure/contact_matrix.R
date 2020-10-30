#Function to return an NxN sparse matrix indicating contacts between pairs in the synthetic population
# Contact matrices returned are:
#   1. Household
#   2. Work (essential + non-essential)
#   3. School
#   4. Grade
#   5. Class
#   6. Community
#   7. Work (membership in essential workplace only)

Rcontact_matrix <- function(synth_pop_df, #dataframe of synthetic population
                            HHcontact, # contact rate among HH members (=1)
                            SCHcontact,# contact rate among school members not in same grade (=1/35)
                            WORKcontact, # contact rate among agents in same workplace (= 5/7)
                            CLASScontact, # contact rate among agents in same class (=5/7 - (SCHcontact + GRADEcontact))
                            GRADEcontact, # daily contact rate among agents in same grade (=1/7 - SCHcontact)
                            AgeContRates, # community contact rates by age
                            propEssential, #porportion of workplaces deemed essential
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
  
  ## 4. Work contact matrix for essential workers only
  
  #determine if workplace is essential
  EWrkN <- sample(1:NWork, round(propEssential*NWork,0), replace = F)
  EWrksize <- Wrksize[EWrkN]
  
  agentsEWrk = agents[Work %in% names(EWrksize)]
  EWrkdo = Work[Work %in% names(EWrksize)]
  
  #list of agents that share membership in a shared essential workplace
  EWrk.cont<-t(do.call(cbind,tapply(agentsEWrk,EWrkdo,combn,m=2)))
  
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
  
  ### Convert to sparse matrices
  matrixHHsp <- as(matrixHH, "sparseMatrix")
  matrixWORKsp <- as(matrixWORK, "sparseMatrix")
  matrixSCHsp <- as(matrixSCH, "sparseMatrix")
  matrixGRDsp <- as(matrixGRD, "sparseMatrix")
  matrixCLSsp <- as(matrixCLS, "sparseMatrix")

  return(list(matrixHHsp, matrixWORKsp, matrixSCHsp, 
              matrixGRDsp, matrixCLSsp, COMmat, EWrk.cont))
}



