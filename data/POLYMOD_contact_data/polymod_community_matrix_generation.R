###Objective of code: Generate and save POLYMOD contact matrices needed for each counterfactual (CF) scenario

############## Generate Community Contact Matrices for Each CF Situation #############
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

library(socialmixr)

##Download matrices according to counterfactual situation

#POLYMOD: Community - (school + work)
polymod_dat.CF6 <- contact_matrix(polymod, countries = "United Kingdom", 
                              age.limits = c(0, 5, 13, 18, 40, 65),
                              filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0),
                              symmetric = FALSE) 
polymod_dat.CF6 <- polymod_dat.CF6$matrix

#POLYMOD: Community - (school + work + public transport)
polymod_dat.CF3_5 <- contact_matrix(polymod, countries = "United Kingdom", 
                                  age.limits = c(0, 5, 13, 18, 40, 65),
                                  filter = list(cnt_home = 0, cnt_work = 0, cnt_school = 0, cnt_transport = 0),
                                  symmetric = FALSE) 
polymod_dat.CF3_5 <- polymod_dat.CF3_5$matrix

#POLYMOD: public transportation
polymod_dat.CF2_4 <- polymod_dat.CF6 - polymod_dat.CF3_5


#Save contact matrices as .rda files
save(polymod_dat.CF6, file= "polymod_dat.CF6.rda")
save(polymod_dat.CF3_5, file= "polymod_dat.CF3_5.rda")
save(polymod_dat.CF2_4, file= "polymod_dat.CF2_4.rda")
