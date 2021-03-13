

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)
library(magrittr)
library(here)
library(readxl)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="redtide_uncorrelated",
  modelDescr ="Mixed logit model on Red tide data, uncorrelated Lognormals in utility space",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 3
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Read data
database = read_excel("analysis\\apollo\\cleaned_data_wide.xlsx")

data_long = read_csv("analysis\\apollo\\cleaned_data.csv")

### reshape long to wide
individual_specific_vars <- c('id','origBlock','block','rotation','income','allNO','protNO','watercolor2018','watercolor2019','respirr2018','respirr2019','watercolor_1m','respirr_1m')
choice_variables <- c("cov","acc1","acc2","bid","vote","choice","sq","covacc1","covacc2","acc1acc2","cov12","acc175","acc1100","acc275","acc2100","cov12acc175","cov12acc1100","cov12acc275","cov12acc2100","cov12ec","acc175ec","acc1100ec","acc275ec","acc2100ec","cov12acc175ec","cov12acc1100ec","cov12acc275ec","cov12acc2100ec")
data_wide <- data_long %>% 
             pivot_wider(id_cols = all_of(individual_specific_vars), names_from = c('set','option'), values_from = all_of(choice_variables))
  
# database %<>% dplyr::select(id, set, idset, income, cov, acc1, acc2, bid,
#                         vote, watercolor2018, respirr2018, watercolor2019,
#                         respirr2019, watercolor_1m, respirr_1m)

names(database)[names(database) == "vote"] <- "choice"
database$ID<-database$ID + 1000
# ## keep only relevant columns
# database %<>% dplyr::select(id, set, idset, # choice set variables
#                         choice,sq,cov12,acc175,acc1100,acc275,acc2100,bid, # primary variables of interest
#                         watercolor2018,respirr2018,watercolor2019,respirr2019,watercolor_1m,respirr_1m, # beach observations
#                         income) # individual characteristics, any others?

## sort and generate unique id for each alternative
# database %<>% group_by(idset) %>% mutate(alt = seq(n()))
# database %<>% arrange(id,set,idset) 
# database %<>% mutate(bid = as.numeric(bid))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_1 = 0,
                asc_2 = 0,
                asc_3 = 0,
                mu_log_b_cov    =-3,
                sigma_log_b_cov_inter = 0,
                sigma_log_b_cov_intra = 0,
                mu_log_b_accF    =-3,
                sigma_log_b_accF_inter = 0,
                sigma_log_b_accF_intra = 0,
                mu_log_b_accS   =-3,
                sigma_log_b_accS_inter = 0,
                sigma_log_b_accS_intra = 0,
                mu_log_b_bid   =-3,
                sigma_log_b_bid_inter = 0,
                sigma_log_b_bid_intra = 0
                )

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_3")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_cov_inter","draws_accF_inter",
                     "draws_accS_inter","draws_bid_inter"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c("draws_cov_intra","draws_accF_intra",
                     "draws_accS_intra","draws_bid_intra")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_cov"]] = exp( mu_log_b_cov + sigma_log_b_cov_inter * draws_cov_inter
                               + sigma_log_b_cov_intra * draws_cov_intra )
  
  randcoeff[["b_accF"]] = exp( mu_log_b_accF + sigma_log_b_accF_inter * draws_accF_inter 
                                + sigma_log_b_accF_intra * draws_accF_intra )
  
  randcoeff[["b_accS"]] = exp( mu_log_b_accS + sigma_log_b_accS_inter * draws_accS_inter 
                               + sigma_log_b_accS_intra * draws_accS_intra )
  
  randcoeff[["b_bid"]] = exp( mu_log_b_bid + sigma_log_b_bid_inter * draws_bid_inter 
                              + sigma_log_b_bid_intra * draws_bid_intra )
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #
apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = b_cov * cov + b_accF * accF + b_accS * accS + b_bid * bid
  V[['alt2']] = b_cov * cov + b_accF * accF + b_accS * accS + b_bid * bid
  V[['alt3']] = b_cov * cov + b_accF * accF + b_accS * accS + b_bid * bid
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2,alt3=3),
    avail         = list(alt1=1, alt2=2,alt3=3),
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(hessianRoutine="maxLik"))





