# School closures reduced social mixing of children during COVID-19 with implications for transmission risk and school reopening policies
Analysis of school closure and reopening policies in COVID-19 outcomes in the Bay Area (California)
# Documentation

## Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled "School closures reduced social mixing of children during COVID-19 with implications for transmission risk and school reopening policies". The objectives of this study were to: 1) estimate social contact patterns among school-aged children during Bay Area (California) COVID-19 related school closures; 2) estimate the cumulative incidence of COVID-19 throughout the 2019-2020 spring semester under counterfactual scenarios had schools or workplaces remained open, or social distancing policies not been enacted; and 3) estimate the effect of various school reopening strategies in Bay Area schools for the 2020 fall semester.

## Downloading code and data

Code and data files are available from from this repository at: http://www.github.com/jrhead/COVIDandSchools. A description of each folder and file are below:

### data

The **data** folder contains social contact data obtained from both the Bay Area social contact survey, as well as the POLYMOD social contact surveys from the UK. The folder is sub-divided into two directories, with the following contents:

1. **POLYMOD_contact_data**: directory which contacts code and age-structured contact matrices during pre-pandemic times

    * **polymod_community_matrix_generation.R** contains the source code to download the POLYMOD age-structured contact matrices by location, using the socialmixr package.

    * **polymod_dat.RData** contains an age-structured contact matrix representing non-household contacts summed across all locations.

    * **polymod_dat.CF2_4.RData** contains an age-structured contact matrix representing non-household, non-work, non-school, and non-transportation contacts. It is combined with the work, school, and transportation matrices from the Bay Area synthetic population and the Bay Area survey to generate the community contact matrix for counterfactual scenarios 2 and 4 (workplaces open).

    * **polymod_dat.CF3_5.RData** contains an age-structured contact matrix representing transportation contacts. It is combined with the work and school, matrices from the Bay Area synthetic population and the non-transportation, non-household contacts from the Bay Area survey to generate the community contact matrix for counterfactual scenarios 3 and 5 (socializing permitted).

    * **polymod_dat.CF6.RData** contains an age-structured contact matrix representing non-household, non-work, and non-school contacts. It is combined with the work and school matrices from the Bay Area synthetic population to generate the community contact matrix for counterfactual scenario 6 (workplaces & schools open; socializing permitted)

2. **survey_contact_data**: directory which contains the age-structred contact matrices by location during the pandemic in California Bay Area.

    * **community-matrix_060820.RData**: contains an age-structured contact matrix representing the mean number of non-household contacts that occurred in the past 24 hours in the Bay Area at all locations.

    * **childcare_060820.RData**: contains an age-structured contact matrix representing the mean number of non-household contacts that occurred in the past 24 hours in the Bay Area at a child care setting.

    * **transit_060820.RData**: contains an age-structured contact matrix representing the mean number of non-household contacts that occurred in the past 24 hours in the Bay Area on public transport.

    * **work_060820.RData**: contains an age-structured contact matrix representing the mean number of non-household contacts that occurred in the past 24 hours in the Bay Area at the respondent's place of work.

### code

The **code** folder contains all the R scripts necessary to run all instances of the SEIR model. It is divided into two folders; a) **spring_closure** contains R scripts to estimate the cumulative incidence of COVID-19 throughout the 2019-2020 spring semester under counterfactual scenarios had schools or workplaces remained open, or social distancing policies not been enacted; and b) **fall_reopen** contains R scripts to estimate the effect of various school reopening strategies in Bay Area schools for the 2020 fall semester.

1. **spring_closure** directory: There are four files in this main directory. The first and main script,
**main_counterfactual_scenarios_spring.R**, is the only one that needs to be updated and executed. 

    * **main_counterfactual_scenarios_spring.R** The main R script which controls the synthetic population generation, manipulation of contact rates under various interventions, and running of the SEIR model.

    * **synth_pop.R** A funtion to generate a synthetic population of size N, using a variety of inputs describing population demographics (e.g. age distribution of population, average household size, proportion of single parent and multigenerational households, class sizes, employment rates, etc). The output is a dataframe of a synthetic population where N agents are assigned an age and membership in a household, community, and -- depending on age -- a workplace or school/grade/class.

    * **contact_matrix.R** A function to generate a N x N matrix of all pairwise contact rates between N agents in the synthetic population, stratified by relationship between each agent (e.g. household, workplace, school, grade, class, community)

    * **model_functions.R** A set of functions for determining the age-dependent outcome of infection for N agents, and for running the SEIR model.

    * **plots**: this directory contains three R scripts that generate Figures 2 and 3 in the manuscript and Figure S3 in the supplementary material. Each of these scripts load data contained in the **output** directory, but can be updated to generate figures for saved output generate under different model assumptions in the **main_counterfactual_scenarios_spring.R** script.

2. **fall_reopen** directory: There are 6 files in this main directory. The first two, 
**main_reopening_strategies.R** and **main_reopening_strateiges_hybridbygrade.R**, are the main script files and are the only ones that need to be updated and executed.

    * **main_reopening_strategies.R** The main R script which controls the synthetic population generation, manipulation of contact rates under various interventions, and running of the SEIR model. It differs from the script below because the hybrid strategies examined involve splitting classes into two, whereby children attend 2 days a week in half class sizes, and teachers attend 4 days a week to teach each half class. The other reopening strategies examined in this script include: strong cohort, masks, various testing regimes, combined interventions.
    
    * **main_reopening_strateiges_hybridbygrade.R** The main R script which controls the synthetic population generation, manipulation of contact rates under various interventions, and running of the SEIR model. It differs from the script above because the hybrid strategies examined involve splitting schools into two groups based on grades (i.e. grades K-2 vs grades 3-5), and teachers attend 2 days a week to teach their entire class. The other reopening strategies examined in this script include: weak cohorts, combined interventions.
    
    * **synth_pop_reopen.R** A funtion to generate a synthetic population of size N, using a variety of inputs describing population demographics (e.g. age distribution of population, average household size, proportion of single parent and multigenerational households, class sizes, employment rates, etc). The output is a dataframe of a synthetic population where N agents are assigned an age and membership in a household, community, and -- depending on age -- a workplace or school/grade/class. It differs from the synth_pop.R script in the spring_closure directory because now it assigns school children into a shift for a potential hybrid schedule, whereby shifts are half-classes.
    
    * **synth_pop_reopen_hybridbygrade.R** A funtion to generate a synthetic population of size N, using a variety of inputs describing population demographics (e.g. age distribution of population, average household size, proportion of single parent and multigenerational households, class sizes, employment rates, etc). The output is a dataframe of a synthetic population where N agents are assigned an age and membership in a household, community, and -- depending on age -- a workplace or school/grade/class. It differs from the synth_pop.R script in the spring_closure directory because it adds the assignment of  school children/teachers into a shift for a potential hybrid schedule, whereby shifts are based on grade groups.
    
    * **contact_matrix_reopen.R** A function to generate a N x N matrix of all pairwise contact rates between N agents in the synthetic population, stratified by relationship between each agent (e.g. household, workplace, school, grade, class, community). It differs from the contact_matrix.R script in the spring_closure directory because it also creates school, grade, and class matrices in effect under a hybrid schedule.
    
    * **model_functions_reopen.R** A set of functions for determining the age-dependent outcome of infection for N agents, and for running the SEIR model. It differs from the model_functions.R in the spring_closure directory because it contains a specific SEIR function that includes test and quarantine/isolation.
    
    * **plots_tables** This directory contains three R scripts:
          
      * **functions_for_summarizing_output.R** A set of functions for summarizing the mean and distribution of the absolute risk and the excess risk of certain outcomes by sub group across all independent realizations of the model.
            
      * **Figures5_6_reopening_strategies.R** Generates Figures 4 and 5 in the manuscript
            
      * **tables_of_excess_risk.R** Generates the supplemental table 5-8 in the manuscript

### output

The **output** directory contains .RData files of independent realizations of the model across different model specifications and assumptions. It contains three sub-directories:

1. **main_counter_factual_interventions** contains .RData files which are output from examining the 6 counterfactual interventions examining the effect of the spring semester closure.

    * **SpringSemester_Less10HalfSusceptible_CF1_6.RData** contains results assuming children under 10 years are half as susceptible to SARS-CoV-2 as children and adults 10 and above.
    
    * **SpringSemester_Less20HalfSusceptible_CF1_6.RData** contains results assuming children under 20 years are half as susceptible to SARS-CoV-2 as adults 20 and above.
        
    * **SpringSemester_EqualSusceptibility_CF1_6.RData** contains results assuming there is no age-dependent susceptibility to SARS-CoV-2.
    
2. **vary_key_params** contains .RData files which are output from examining the influence of $\alpha$ (ratio of the force of infection of asymptomatic vs symptomatic infectio), susceptibility ratios, and city demographics on the counterfactual incidence in the spring if schools had remained open. It contains two sub-directories:

    a. **alpha_susceptibility_ratio** contains nine .RData files where both $\alpha$ and the susceptibility ratios are varied across three combinations for each ($\alpha$ is 0.25, 0.50, or 1 (represented as A25, A50, A100 in the file names); and susceptibility ratio of children under 20 to adults above 20 is 0.25, 0.50, or 1 (represented as SR25, SR50, SR100 in the file names). These are produced by running the script **main_counterfactual_scenarios_spring.R**

    b. **proportion_children** contains two .RData files where the proportion of households with children is varied from 16.9% (Berkeley) to 24.4% (Hayward). These are produced by running the script **main_counterfactual_scenarios_spring.R**

3. **main_reopening_scenarios** contains eight .RData files produced by running both of the scripts **main_reopening_strategies.R** and **main_reopening_strategies_hybridbygrade.R**. They examine differences in outcomes association with school reopening under various interventions across four combinations of modelling scenarios. These four combinations are produced by crossing:

    * Community transmission high vs. community transmission moderate
    
    * Children under 20 are half as susceptible to SARS-CoV-2 as adults vs. all ages are equally susceptible.
    


## Running the scripts

### Spring semester closure

To examine the counterfactual incidence that could have occurred during the spring semester had schools remained open under various scenarios, options at the top of the R script, **main_counterfactual_scenarios_spring.R**, can be modified. These are shown here:

```{r eval = FALSE}
# MODEL PARAMETERS VARIED IN ANALYSES
# proportion of HH with children <18; 
propKidHH <- 0.251 

#ratio of asymptomatic to symptomatic transmission
alpha = 0.5 
#ratio of the susceptibility of children < (susceptAgeSplit) to SARS-CoV-2 vs. adults
susceptRatio = 0.5 

#age at which susceptibility changes according to the ratio above
susceptAgeSplit = 10 
```

Other parameters may be varied throughout, including other population level parameters.

To load needed functions into the main R script, first update the working directory to the COVIDandSchools directory.

```{r eval = FALSE}
#Load needed R scipts/functions
source(".\\code\\fall_reopen\\synth_pop_reopen.R")
source(".\\code\\fall_reopen\\contact_matrix_reopen.R")
source(".\\code\\fall_reopen\\model_functions_reopen.R")
```

This script was implemented in parallel on a computer with multiple cores. To change the code to fit with different computer specifications, see these lines of code:

```{r eval = FALSE}
##set up parallel processing
nCores <- 5
registerDoParallel(nCores)
```

To update the name of the simulations that will be saved in an .RData file, update the name in the final line in the script:

```{r eval = FALSE}
save(outcomes, file = "ModelledOutcomes_args.RData")
```

To visualize the results of the outcomes that you have just run, update the naming of the file in the load command of the following scripts within the *plots* sub-directory:

* Figure2_effect_of_spring_closure.R

* Figure3_effect_by_alpha_susceptibility_city.R

```{r eval = FALSE}
load(".\\output\\main_counterfactual_interventions\\SpringSemester_Less10HalfSusceptible_CF1_6.RData")
```


### Fall semester reopening strategies

To examine the incidence, hospitalizations, and deaths associated with reopening under different strategies in the fall semster under various community transmission scenarios and other modelling assumptions, options at the top of the R scripts, **main_counterfactual_scenarios_spring.R**, can be modified. The first 4 are the same as in the spring semester simulations, and the latter two are related to whether community transmission during the summer and into the fall semester will be considered "high" or "moderate". These are shown here:

```{r eval = FALSE}
# MODEL PARAMETERS VARIED IN ANALYSES
# proportion of HH with children <18; 
propKidHH <- 0.251 

#ratio of asymptomatic to symptomatic transmission
alpha = 0.5 
#ratio of the susceptibility of children < (susceptAgeSplit) to SARS-CoV-2 vs. adults
susceptRatio = 0.5 

#age at which susceptibility changes according to the ratio above
susceptAgeSplit = 10 

# COMMUNITY TRANSMISSION PARAMETERS VARIED IN ANALYSES
propMidEssentialTot = 0.75 #set to 75% of in-person work for high transmission scenario; 
                           #0.5 for moderate transmission scenario

CommContactIncr = 2 #indicates increases in socializing over summer; 
                    #set to 2 (twice as many community contacts as observed during survey) 
                    #for high transmission scenario, 1.28 for moderate

```

Other parameters may be varied throughout, including other population level parameters.

This script was implemented in parallel on a computer with multiple cores. To change the code to fit with different computer specifications, see these lines of code:

```{r eval = FALSE}
##set up parallel processing
nCores <- 5
registerDoParallel(nCores)
```

To update the name of the simulations that will be saved in an .RData file, update the name in the final line in the script:

```{r eval = FALSE}
save(outcomes, file = "ModelledReopeningStrategies.RData")
```

To visualize the results of the outcomes that you have just run, update the naming of the file in the load command of the following scripts within the *plots_tables* sub-directory:

* Figures4_5_reopening_strategies.R

* tables_of_excess_risk.R

```{r eval = FALSE}
load(".\\output\\main_reopening_scenarios\\ModelledReopeningStrategies.RData")
```
