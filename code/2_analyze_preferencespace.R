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
data <- as.data.table(read_csv("store/cleaned_data.csv"))

## drop protest. ask Klaus why does protNO==-999 in many cases? 
data <- data[protNO!=1,]  

## keep only relevant columns
data %<>% dplyr::select(id, set, idset, # choice set variables
                        choice,sq,cov12,acc175,acc1100,acc275,acc2100,bid, # primary variables of interest
                        watercolor2018,respirr2018,watercolor2019,respirr2019,watercolor_1m,respirr_1m, # beach observations
                        income) # individual characteristics, any others?

## sort and generate unique id for each alternative
data %<>% arrange(id,set,idset) 
data %<>% mutate(alt = seq(n()))

## Create mlogit data
d <-  mlogit.data(data,
                  id.var='id',
                  chid.var = 'idset',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite=c('bid'))

####################################################
#############################################  BASIC
####################################################

## Klaus' basic MNL: clogit choice sq cov12 acc175 acc1100 acc275 acc2100 bid, group(idset), if protNO~=1 

pref_basic <-  gmnl(choice ~ bid + sq + cov12 + acc175 + acc1100 + acc275 + acc2100 + bid | 0 ,
                    data=d,
                    ranp=c(cov12='n',acc175='n',acc1100='n',acc275='n',acc2100='n'),
                    model='mixl',
                    panel=TRUE,
                    correlation=TRUE)

summary(pref_basic)
saveRDS(pref_basic,file="estimates/pref_basic.rds")

####################################################
#################################  ASC HETEROGENEITY
####################################################

data %<>% mutate(sq_income = sq * income)

het_income <-  gmnl(choice ~ bid + sq + cov12 + acc175 + acc1100 + acc275 + acc2100 + bid + sq_income | 0 ,
                    data=d,
                    ranp=c(cov12='n',acc175='n',acc1100='n',acc275='n',acc2100='n'),
                    model='mixl',
                    panel=TRUE,
                    correlation=TRUE,
                    seed=42)

summary(het_income)
saveRDS(het_income,file="estimates/het_income.rds")


####################################################
###############################################  WTP
####################################################

pref_basic_wtp <- wtp.gmnl(pref_basic, wrt = "bid")
saveRDS(pref_basic_wtp,file="estimates/pref_basic_wtp.rds")

het_income_wtp <- wtp.gmnl(het_income, wrt = "bid")
saveRDS(pref_basic_wtp,file="estimates/het_income_wtp.rds")

## END OF SCRIPT. Have a nice day! 