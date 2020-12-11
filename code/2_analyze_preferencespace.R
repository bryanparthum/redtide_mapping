## Written by: Bryan Parthum; bparthum@gmail.com ; December 2020

###################################################################################
##########################   PREFERENCE-SPACE MODELS    ###########################
###################################################################################

####################################################
########   Check for and load Packages   ###########
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("gmnl","mlogit","data.table",'here','readr','tidyverse','magrittr') ## you can add more packages here
lapply(packages, pkgTest)

####################################################
#####################   Load and generate dataframes
####################################################

## Set working directory
setwd(here())

## Read in files
data <- read_csv("store/cleaned_data.csv")

## drop protest. ask Klaus why does protNO==-999 in many cases? 
data %<>% dplyr::filter(protNO!=1)  

## keep only relevant columns
data %<>% dplyr::select(id, set, idset, # choice set variables
                        choice,sq,cov12,acc175,acc1100,acc275,acc2100,bid, # primary variables of interest
                        watercolor2018,respirr2018,watercolor2019,respirr2019,watercolor_1m,respirr_1m, # beach observations
                        income) # individual characteristics, any others?

## sort and generate unique id for each alternative
data %<>% group_by(idset) %>% mutate(alt = seq(n()))
data %<>% arrange(id,set,idset) 
data %<>% mutate(as.numeric(bid))

## Create mlogit data
d <-  mlogit.data(data,
                  # shape='long', 
                  drop.index = TRUE,
                  id.var='id', ## unique to individual_id
                  chid.var = 'idset', ## unique to individual_id and card_id
                  choice='choice',
                  shape='long',
                  alt.var='alt') ## the number alternative on each card
                  # opposite=c('bid')) ## bid is already opposite in Klaus' cleaning


####################################################
#############################################  BASIC
####################################################

## Klaus' basic MNL: clogit choice sq cov12 acc175 acc1100 acc275 acc2100 bid, group(idset), if protNO~=1 

start <- proc.time()
pref_clogit <-  mlogit(choice ~ sq + cov12 + acc175 + acc1100 + acc275 + acc2100 + bid,
                    data=d)

## WHY IS THIS SINGLUAR? 

summary(pref_clogit)
saveRDS(pref_clogit,file="estimates/pref_clogit.rds")
end <- proc.time() - start

####################################################
#######################  INTRODUCE RANDOM PARAMETERS
####################################################

start <- proc.time()
pref_rand_p <-  gmnl(choice ~ sq + cov12 + acc175 + acc1100 + acc275 + acc2100 + bid | 0 ,
                    data=d,
                    ranp=c(cov12='n',acc175='n',acc1100='n',acc275='n',acc2100='n'),
                    model='mixl',
                    panel=TRUE,
                    correlation=FALSE)

summary(pref_rand_p)
saveRDS(pref_rand_p,file="estimates/pref_rand_p.rds")
end <- proc.time() - start

####################################################
#################################  ASC HETEROGENEITY
####################################################

data %<>% mutate(sq_income = sq * income)

start <- proc.time()
het_income <-  gmnl(choice ~ sq + cov12 + acc175 + acc1100 + acc275 + acc2100 + bid + sq_income | 0 ,
                    data=d,
                    ranp=c(cov12='n',acc175='n',acc1100='n',acc275='n',acc2100='n'),
                    model='mixl',
                    panel=TRUE,
                    correlation=FALSE,
                    seed=42)

summary(het_income)
saveRDS(het_income,file="estimates/het_income.rds")
end <- proc.time() - start

####################################################
###############################################  WTP
####################################################

pref_basic_wtp <- wtp.gmnl(pref_basic, wrt = "bid")
saveRDS(pref_basic_wtp,file="estimates/pref_basic_wtp.rds")

het_income_wtp <- wtp.gmnl(het_income, wrt = "bid")
saveRDS(pref_basic_wtp,file="estimates/het_income_wtp.rds")

## END OF SCRIPT. Have a nice day! 