### Function to define synthetic population

synth_pop <- function(N, #number of agents
                      avgHHsize, #average household size (all houses)
                      avgFamsize, #average size of "family household" as defined by census
                      numCommunity = 1, #number of communities 
                      MedAge, #median age
                      propKidHH, #proportion of households with children <18
                      propFamHH, #proportion of households defined as family households
                      propUnemployed, #proportion unemployed
                      propSingleParent, #proportion of single parent household
                      propGrandparent, #proportion of children raised by grandparents
                      propMultigen,  #proportion of multigenerational households
                      prop65p, #proportion of population over 65 years
                      NWork,  #number of work places
                      Nschools = list(Elementary = 0, Middle = 0,High =0), #number of schools by level
                      ClassSize #average class size
                      ){
  
  require(dplyr)
  
  ## 1. Assign agents as part of a household with children or not
  # determine average size of households with children
  avgKidFamSize <- (avgFamsize  - avgHHsize*(1-propKidHH/propFamHH))/(propKidHH/propFamHH)
  # determine proportion of population in a household with children
  prop_in_kidHH <- propKidHH*avgKidFamSize/(propKidHH*avgKidFamSize + (1-propKidHH)*avgHHsize)
  # random assigment based on proportion
  KidHH <- as.numeric(runif(N) < prop_in_kidHH)
  KidHH <- sort(KidHH)
  
  ## 2. Assign agent IDs
  ID <- c(1:N)
  
  ## 3. Assign HH id's
  #determine size of each household without kids
  n <- 1
  HH_nf <- rep(n, round(rgamma(1, shape = avgHHsize-1, scale = 1),0)+1) #add 1 to ensure no zeros
  while(length(HH_nf) < sum(KidHH==0)){
    HH_nf <- c(HH_nf, rep(n+1, round(rgamma(1, shape = avgHHsize-1, scale = 1),0)+1))
    n<-n+1
  }
  HH_nf <- HH_nf[1:sum(KidHH==0)]
  
  #determine size of each household with kids
  n<-max(HH_nf)+1
  HH_f <- rep(n, round(rgamma(1, shape = avgKidFamSize-2, scale = 1),0)+2) #add 2 to ensure no singles in family HH
  
  #assign the right number of agents into households based on size
  while(length(HH_f) < sum(KidHH==1)){
    HH_f <- c(HH_f, rep(n+1, round(rgamma(1, shape = avgFamsize-2, scale = 1),0)+2))
    n<-n+1
  }
  HH_f<- HH_f[1:sum(KidHH==1)]
  
  HH <- c(HH_nf, HH_f)

  #check and summarize HH's
  HHsummary <- data.frame(HH) %>% group_by(HH) %>% summarize(size = n())
  numHH <- length(unique(HH)) #number of households
  
  ## 4. Assign ages based on HH ids
  
  #get ids for the different types of HH's
  HH_nokids <- c(1:max(HH_nf)) #non-kid HH
  HH_kids <- c(max(HH_nf)+1:max(HH_f)) #HH with kid

  #bind together for easier viewing
  data <- left_join(data.frame(HH), HHsummary, by = "HH")
  data$kids <- ifelse(data$HH %in% HH_kids, 1, 0)
 
  #Which kid households have a single parent? which have no parents but grandparent(s)? which are multigenerational?
  #There are 7 combinations possible:
    #SP: 1 parent, 0 GP = single parent, family size 2, 3+. Probability = 0.40 - 0.03*0.40 - 0.04*0.40
    #SGP: 0 parent, 1 GP = single parent, GP only, family size 2, 3+. Probability = 0.03*0.40
    #SPMG: 1 parent, 1 GP = single parent, multigen, family size 3+. Probability = 0.04*0.40/2
    #SPMG2: 1 parent, 2 GP = single parent, multigen, family size 4+. Probability = 0.04*0.40/2
    #GPO: 0 parents, 2 GP = GP only, family size 3 +. Probability = 0.03*0.60
    #MG: 2 parents, 1 GP = multigen, family size 4+. Probability = 0.04*0.60/2
    #MG2: 2 parents, 2 GP = multigen, family size 5+. Probability = 0.04*0.60/2
    #B: 2 parents, 0 GP = none, family size 3+. Probability = 0.60 - 0.04*0.60 - 0.03*0.60
    #GP only = 3%
    #multigen = 4%
    #single parent = 40%
  
  #Determine the HHids for households with kid based on size
  HHsummary$HasKids <- ifelse(HHsummary$HH >= min(HH_kids),1,0)
  HH_kids_1p <- sum(HHsummary$HasKids == 1) # Number of kid HHs
  HH_kids_2 <- HHsummary[HHsummary$HasKids==1 & HHsummary$size == 2,]$HH #HH id's for 2 person kid HH--must be SP/SGP
  HH_kids_3p <- HHsummary[HHsummary$HasKids==1 & HHsummary$size >= 3,]$HH #HH id for 3+ person kid HH
  HH_kids_4p <- HHsummary[HHsummary$HasKids==1 & HHsummary$size >= 4,]$HH #HH id for 4+ person kid HH
  HH_kids_5p <- HHsummary[HHsummary$HasKids==1 & HHsummary$size >= 5,]$HH #HH id for 5+ person kid HH
  
  #Calculate proportion of each of the 7 kid HH types
  propSP <- propSingleParent*(1-propGrandparent - propMultigen)
  propSGP <- propSingleParent*propGrandparent
  propSPMG <- propSPMG2 <- propSingleParent*propMultigen/2
  propGPO <- propGrandparent*(1-propSingleParent)
  propMG <- propMG2 <- propMultigen*(1-propSingleParent)/2
  propB <- (1-propSingleParent)*(1-propMultigen - propGrandparent)
  #These should all sum to 1!
  #propSP + propSGP + propSPMG + propGPO + propMG + propMG2  + propB + propSPMG2
  
  #2 person HH are automatically either SP or SGP
  SP <- sample(HH_kids_2, (1-propGrandparent)*length(HH_kids_2), replace = FALSE)
  SGP <- HH_kids_2[!HH_kids_2 %in% SP]
  
  #select the remaining SP and SGP households
  SPc <- sample(HH_kids_3p, max(1, propSP*HH_kids_1p - length(SP)), replace = FALSE)
  SP <- c(SP, SPc)
  SGPc <- sample(HH_kids_3p, max(1,propSGP*HH_kids_1p - length(SGP)), replace = FALSE)
  SGP <- c(SGP, SGPc)
  
  #Select SPMG HH and GPO HH (3+ HH)
  SPMG <- sample(HH_kids_3p[!HH_kids_3p %in% c(SP, SGP)], max(1,propSPMG*HH_kids_1p), replace = FALSE)
  GPO <- sample(HH_kids_3p[!HH_kids_3p %in% c(SP, SGP, SPMG)], max(1,propGPO*HH_kids_1p), replace = FALSE)
  
  #Select SPMG2 and MG HH (4+ HH)
  SPMG2 <- sample(HH_kids_4p[!HH_kids_4p %in% c(SP, SGP, SPMG, GPO)], max(1,propSPMG2*HH_kids_1p), replace = FALSE)
  MG <- sample(HH_kids_4p[!HH_kids_4p %in% c(SP, SGP, SPMG, GPO, SPMG2)], max(1,propMG*HH_kids_1p), replace = FALSE)
  
  #Select MG2 HH (5+ HH)
  MG2 <- sample(HH_kids_5p[!HH_kids_5p %in% c(SP, SGP, SPMG, GPO, SPMG2, MG)], max(1,propMG*HH_kids_1p), replace = FALSE)
  
  #Label the type of HH in HHsummary
  HHsummary$Type <- ifelse(HHsummary$HasKids == 1, "B", "NoKids") #initialize Type
  HHsummary[HHsummary$HH %in% SP, "Type"] <- "SP"
  HHsummary[HHsummary$HH %in% SGP,"Type"] <- "SGP"
  HHsummary[HHsummary$HH %in% SPMG,"Type"] <- "SPMG"
  HHsummary[HHsummary$HH %in% SPMG2,"Type"] <- "SPMG2"
  HHsummary[HHsummary$HH %in% GPO,"Type"] <- "GPO"
  HHsummary[HHsummary$HH %in% MG,"Type"] <- "MG"
  HHsummary[HHsummary$HH %in% MG2,"Type"] <- "MG2"
  
  B <- HHsummary[which(HHsummary$Type == "B"),]$HH
  
  #Single parent HH have 1 adult and non have 2 adults. Multigeneration have 2 parents and 1 grandparent. 
  #Grandparent only have no parents and 1 or 2 grandparents
  #The rest of the HH members are children
  Role <- rep(NA, N)
  Role[HH %in% HH_nokids] <- "NonParent"
  Role[HH %in% HH_kids] <- "Child" #initialize now, update below
  
  #update roles for the different HH types
  for (i in unique(SP)){
    Role[HH %in% i][1] <- "Parent"
  }
  for (i in unique(SGP)){
    Role[HH %in% i][1] <- "Grandparent"
  }
  for (i in unique(SPMG)){
    Role[HH %in% i][1] <- "Grandparent"
    Role[HH %in% i][2] <- "Parent"
  }
  for (i in unique(SPMG2)){
    Role[HH %in% i][1:2] <- "Grandparent"
    Role[HH %in% i][3] <- "Parent"
  }
  for (i in unique(GPO)){
    Role[HH %in% i][1:2] <- "Grandparent"
  }
  for (i in unique(MG)){
    Role[HH %in% i][1:2] <- "Parent"
    Role[HH %in% i][3] <- "Grandparent"
  }
  for (i in unique(MG2)){
    Role[HH %in% i][1:2] <- "Parent"
    Role[HH %in% i][3:4] <- "Grandparent"
  }
  for (i in unique(B)){
    Role[HH %in% i][1:2] <- "Parent"
  }

  Age <- rep(NA, N) #initialize
  
  #Determine the mean age of the children in the household so can based parent's and sibling ages accordingly
  HHsummary$AgeMeanKid <- ifelse(HHsummary$HasKids == 1, runif(HH_kids_1p, min = 0.1,  max = 17.9), 0)
  HHsummarysub <- data.frame(cbind("HH" = HHsummary$HH, "AgeMeanKid" = HHsummary$AgeMeanKid))
  
  #update summary table
  data <- left_join(data, HHsummarysub, by = "HH")
  

  #Draw from multimodel distribution, with children spaced on average 2 years apart
  Age[Role == "Child"] <- rnorm(sum(Role == "Child"), mean = 0, sd = 2) + data$AgeMeanKid[Role == "Child"]
  Age[Role == "Child" & Age < 0] <- 0.1
  Age[Role == "Child" & Age > 18] <- 17.9
  
  #mean age at first birth = 29 (data for Alameda), 95% CI: 17 - 42 (added 1 for mean kid vs oldest kid...)
  Age[Role == "Parent"] <- rnorm(sum(Role == "Parent"), mean = 30, sd = 6) + data$AgeMeanKid[Role == "Parent"]
  Age[Role == "Grandparent"] <- rnorm(sum(Role == "Grandparent"), mean = 30 + 30, sd = 8) + data$AgeMeanKid[Role == "Grandparent"]

  #Define the age of NonParents as uniform until 65
  Age[Role == "NonParent"] <- as.numeric(runif(sum(Role == "NonParent")) < (prop65p*N - sum(Age>65,na.rm = T))/sum(Role == "NonParent")) #1 = older than 65
  
  # Multimodal dist for nonparents: young and older adults
  coinflips <- rbinom(sum(Role == "NonParent" & Age == 0), 1, 0.5)
  youngAdults <- 18 + rexp(sum(Role == "NonParent" & Age == 0), rate = 0.15)
  oldAdults <- rnorm(sum(Role == "NonParent" & Age == 0), mean = 50, sd = 6)
  Age[Role == "NonParent" & Age == 0] <- coinflips*youngAdults + (1-coinflips)*oldAdults
  Age[Role == "NonParent" & Age == 1] <- rexp(sum(Role == "NonParent" & Age == 1), rate = 0.15) + 65

  # Vizualize distibutions
  # hist(Age[ Role == "Grandparent"])
  # hist(Age[ Role == "Parent"])
  # hist(Age[Role == "Child"])
  # hist(Age[ Role == "NonParent"])

  #check that proportions are correct
  # sum(Age <= 5)/N
  # sum(Age > 5 & Age < 18)/N
  # sum(Age < 18)/N
  # sum(Age > 65)/N
  # median(Age)
  # hist(Age) 
  
  #Define age categories
  AgeCat <- ifelse(Age<5, "Under5",
                   ifelse(Age>=5 & Age < 11, "5-10",
                          ifelse(Age >= 11 & Age < 14, "11-13",
                                 ifelse(Age >= 14 & Age < 18, "14-17",
                                        ifelse(Age >= 18 & Age < 65, "18-64",
                                               "65+")))))
  
  ## 5. Assign communities based on HH
  Community <- rep(NA, numHH)
  HHperCommunity <- numHH/numCommunity
  Community <- sample(1:numCommunity, numHH, replace = TRUE)
  HHsummary <- cbind(HHsummary, Community)
  HHsummarysub <- data.frame(cbind("HH" = HHsummary$HH, "Community" = HHsummary$Community))
  data <- left_join(data, HHsummarysub, by = "HH")
  Community <- data$Community
  
  ## 6. Assign schools, grades, classes based on age following some rules: 
  # - kids in same level should go to same school as sibling
  # - grades split by ages...
  
  School <- rep(NA, N)
  #Elementary schools take 5-10 y/o
  School[Age>=5 & Age < 11] <- sample(1:Nschools$Elementary, sum(Age>=5 & Age < 11), replace = TRUE)
  
  #Middle schools take 11-13 y/o
  School[Age >= 11 & Age < 14] <- sample(1:Nschools$Middle, sum(Age >= 11 & Age < 14), replace = TRUE)
    # To distinguish middle school ids, multiply by 1000
  School[Age >= 11 & Age < 14] <- 1000*School[Age >= 11 & Age < 14]

  #High schools take 14-17 y/o
  School[Age >= 14 & Age < 18] <- sample(1:Nschools$High, sum(Age >= 14 & Age < 18), replace = TRUE)
    # To distinguish high school ids, multiply by 100000
  School[Age >= 14 & Age < 18] <- 100000 * School[Age >= 14 & Age < 18]

  
  # Find households with multiple kids in same age group, choose one of their school ids
  ag <- aggregate(School, by = list(HH, AgeCat), 
                  function(l) {c(Count = length(l), 
                                 SchId = ifelse(sum(!is.na(l))==0, NA, min(l)))})
  # Keep track of individual ID, aggregate reorders rows
  id0 <- aggregate(ID, by = list(HH, AgeCat), function(l) {as.vector(l)})$x
  id0 <- unlist(id0)
  # Repeats school id for each person in HH in that age group
  School <- rep(ag$x[,2], times = ag$x[,1])
  School <- School[order(id0)]

  #Students go into grades by ages
  Grade <- rep(NA, N)
  Grade[!is.na(School)] <- floor(Age[!is.na(School)]) - 5
  
  SchoolGrade <- rep(NA,N) #unique indicator for grade
  SchoolGrade[!is.na(School)] <- paste0(School[!is.na(School)], ".", Grade[!is.na(School)])
  
  #Grades go into classes based on average class sizes
  Class <- rep(NA,N)
  SchoolGradeVect <- unique(SchoolGrade[!is.na(SchoolGrade)])
  for (i in 1:length(SchoolGradeVect)){
    numClasses <- ceiling(sum(SchoolGrade == SchoolGradeVect[i],na.rm =T)/ClassSize)
    Class[!is.na(SchoolGrade) & SchoolGrade == SchoolGradeVect[i]] <- 
      sample(1:max(1,numClasses), sum(SchoolGrade == SchoolGradeVect[i], na.rm = T), replace = TRUE)
  }

  SchoolGradeClass <- rep(NA,N) #unique indicator for class
  SchoolGradeClass[!is.na(School)] <- paste0(School[!is.na(School)], ".", Grade[!is.na(School)], ".", Class[!is.na(School)])
  
  ## 7. Assign workplaces based on age only -- add people who don't work (but are not unemployed)
  Employed <- Work <- rep(NA, N)
  Employed[Age >= 18 & Age < 65] <- as.numeric(runif(sum(Age >= 18 & Age < 65)) < 1-propUnemployed)
  Work[Age >= 18 & Age < 65 & Employed == 1] <- sample(1:NWork, sum(Age >= 18 & Age < 65 & Employed == 1), replace = TRUE)
  
  ## 8. Reassign adults to classes
  # Teachers go into classes
  SchoolGradeClassVect <- unique(SchoolGradeClass[!is.na(SchoolGradeClass)])
  eligibleteachers <- ID[Age >=18 & Age < 65 & Employed == 1]
  for(i in 1:length(SchoolGradeClassVect)){
    teacher <- sample(eligibleteachers, 1, replace = FALSE)
    Work[teacher] <- NA
    School[teacher] <- strsplit(SchoolGradeClassVect[i], "[.]")[[1]][1]
    Grade[teacher] <- strsplit(SchoolGradeClassVect[i], "[.]")[[1]][2]
    Class[teacher] <- strsplit(SchoolGradeClassVect[i], "[.]")[[1]][3]
  }
  
  # Admin go into schools
  # assuming no overlap just by big enough numbers...
  SchoolVect <- unique(School[!is.na(School)])
  eligibleadmin <- ID[Age >=18 & Age < 65 & Employed == 1]
  for(i in 1:length(SchoolVect)){
    admin <- sample(eligibleadmin, 20, replace = FALSE)
    Work[admin] <- NA
    School[admin] <- SchoolVect[i]
  }
  
  ## 9. Create dataframe of synthetic population and return
  synth_pop <- data.frame(cbind(ID, Age, AgeCat, Community, HH, School, Grade, Class, Work))
  return(synth_pop)
  
}

