### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="RT_InterIntraHetero",
  modelDescr ="Mixed logit model on Red tide data, uncorrelated in WTP space, inter and intra-individual heterogeneity",
  indivID   ="id",  
  mixing    = TRUE, 
  nCores    = 4
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
library(readxl)
database<-read.csv("D:\\RedTide\\R\\tables\\RT_setall.csv")
# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc1=0,
                asc2=0,
                asc3=0,
                mu_log_b_cov    =-3,
                sigma_log_b_cov_inter = 0,
                sigma_log_b_cov_intra = 0,
                mu_log_b_acc1    =-3,
                sigma_log_b_acc1_inter = 0,
                sigma_log_b_acc1_intra = 0,
                mu_log_b_acc2    =-3,
                sigma_log_b_acc2_inter = 0,
                sigma_log_b_acc2_intra = 0,
                mu_log_b_bid    =-3,
                sigma_log_b_bid_inter = 0,
                sigma_log_b_bid_intra = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc3")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c("draws_cov_intra","draws_acc1_intra","draws_acc2_intra","draws_bid_intra"),
  interNormDraws = c("draws_cov_inter","draws_acc1_inter","draws_acc2_inter","draws_bid_inter"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_cov"]] = -exp( mu_log_b_cov + sigma_log_b_cov_inter * draws_cov_inter +
                                 sigma_log_b_cov_intra * draws_cov_intra)
  
  randcoeff[["b_acc1"]] = -exp( mu_log_b_acc1 + sigma_log_b_acc1_inter * draws_acc1_inter +
                                  sigma_log_b_acc1_intra * draws_acc1_intra)
  
  randcoeff[["b_acc2"]] = -exp( mu_log_b_acc2 + sigma_log_b_acc2_inter * draws_acc2_inter  +
                                  sigma_log_b_acc2_intra * draws_acc2_intra)
  
  randcoeff[["b_bid"]] = -exp( mu_log_b_bid + sigma_log_b_bid_inter * draws_bid_inter  +
                                 sigma_log_b_bid_intra * draws_bid_intra)
  
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
  V[["alt1"]] = asc1 + b_cov * cov_1 + b_acc1 * acc1_1 + b_acc2 * acc2_1 + b_bid * bid_1
  V[["alt2"]] = asc2 + b_cov * cov_2 + b_acc1 * acc1_2 + b_acc2 * acc2_2 + b_bid * bid_2
  V[['alt3']] = asc3 + b_cov * cov_3 + b_acc1 * acc1_3 + b_acc2 * acc2_3 + b_bid * bid_3
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1,alt2=2,alt3=3),
    avail         = list(alt1=1, alt2=1,alt3=1),
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
                        apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)



