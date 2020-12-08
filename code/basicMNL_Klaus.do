/* Basic MNL ("clogit") */
/***************************************/
/***************************************/

set mem 5g 
set matsize 800
set logtype text
set more off

use f:\klaus\RTaerosol\stata\data\KlausCurrentShare,replace

/* keep only what I need */
/* don't worry about demographics right now, other than income */

keep origID block income covAQ1-voteQ4 allNO protNO

sort origID
egen id=group(origID) /*I checked, no duplicates */
drop origID

/* change to long format */
/**************************/
/* right now I have 1 row / person, with 9*4=36 columns related to the CE – 3 attributes & bid for each of the 2 actual options, plus the vote per choice set, times the 4 choice sets; I want to change this to 4 rows / person, one for each choice set. This should reduce the number of CE-related columns to 9; Before we reshape, make sure each of the variables that need to be stacked have a numerical ending, here 1-4 for the 4 sets.*/

reshape long covAQ acc1AQ acc2AQ covBQ acc1BQ acc2BQ bidAQ bidBQ voteQ, i(id) j(set)
/* STATA automatically added a new variable, "set" */

/* next, we want to stack the 3 options (A,B,SQ) within each set */
/*******************************************************************/
/* first, we need to add the SQ option, so let's generate SQ settings */
gen cov3=0
gen acc13=0
gen acc23=0
gen bid3=0

/* rename to make sure we have numerical endings for the remaining variables that need to be stacked */
rename covAQ cov1
rename covBQ cov2
rename acc1AQ acc11
rename acc1BQ acc12
rename acc2AQ acc21
rename acc2BQ acc22
rename bidAQ bid1
rename bidBQ bid2
rename voteQ vote

/* re-order variables for easier inspection */
order id-bid2 cov3 acc13 acc23 bid3

/* the next re-shape will be done for each id/set combination (= each row in the current data) , so we need to create a variable to reflect this */
sort id set
egen idset=group(id set)
order id set idset

/* now re-shape; and create a new variable "option" for each of the 3 choice options ("profiles") within each choice set */

reshape long cov acc1 acc2 bid, i(idset) j(option)
order id set idset option


/* generate all remaining variables needed for analysis */
/***********************************************************************/
/* first, the actual choice variable, "1" for what's chose, "0" otherwise */
gen choice=0
recode choice 0=1 if (option==1 & vote==1) | (option==2 & vote==2) | (option==3 & vote==3)

/* generate status quo dummy */
gen sq=0
recode sq 0=1 if option==3
label var sq "SQ indicator (3rd option)"


/* based on the parameters I gave "dcreate" in the script that generated the choice profiles (see next), our model should be able to identify point-effects (aka "nonlinear effects") for coverage("band")=12 (omitted base=6), accuracy1 ("ac12") =75,100 (omitted base=50), accuracy2 ("ac24") =75,100 (omitted base=50), price points (base=$10, but I later changed this to $5 manually), all point-wise interactions between band and accuracy, and a linear interaction between the two accuracy levels. Naturally, this implies that basic linear main effects are also identified, as well as all linear 2-way interactions – so lot's of options how to run the model */

/* to recall, from the dcreate script: 
dcreate ib6.band ib50.ac12 ib50.ac24 ib10.price 12.band#75.ac12 12.band#100.ac12 12.band#75.ac24 12.band#100.ac24 c.ac12#c.ac24, nalt(2) nset(20) bmat(betas) fixedalt(status_quo) asc(3) maxiter(1000) seed(`x') */

/* generate basic linear interactions */
gen covacc1 = cov*acc1
gen covacc2 = cov*acc2
gen acc1acc2 = acc1*acc2


/* basic dummies for coverage, accuracy, plus point-wise interactions;
we don’t really need price points*/
/***************************************/
gen cov12=1 if cov==12
recode cov12 .=0
gen acc175=1 if acc1==75
recode acc175 .=0
gen acc1100=1 if acc1==100
recode acc1100 .=0
gen acc275=1 if acc2==75
recode acc275 .=0
gen acc2100=1 if acc2==100
recode acc2100 .=0

gen cov12acc175=cov12*acc175
gen cov12acc1100=cov12*acc1100
gen cov12acc275=cov12*acc275
gen cov12acc2100=cov12*acc2100

label var cov12 "basic dummy for coverage=12"


/* effect codes – this is an alternative coding scheme that allows you to obtain coefficient estimates for the omitted baselines for each attribute – this can be handy at times, especially if the SQ option also has actual settings for each attribute. Here not so important, but let's create them anyway. See Holmes et al. Ch 5 p. 156*/
/**********************************************************************/
gen cov12ec=1 if cov==12 /* for the actual attribute level of interest */
recode cov12ec .=-1 if cov==6 /* for the usually omitted baseline */
recode cov12ec .=0 /* for everything else – here the SQ setting */

gen acc175ec=1 if acc1==75
recode acc175ec .=-1 if acc1==50
recode acc175ec .=0

gen acc1100ec=1 if acc1==100
recode acc1100ec .=-1 if acc1==50
recode acc1100ec .=0

gen acc275ec=1 if acc2==75
recode acc275ec .=-1 if acc2==50
recode acc275ec .=0

gen acc2100ec=1 if acc2==100
recode acc2100ec .=-1 if acc2==50
recode acc2100ec .=0

/* interactions */
gen cov12acc175ec=cov12ec*acc175ec
gen cov12acc1100ec=cov12ec*acc1100ec
gen cov12acc275ec=cov12ec*acc275ec
gen cov12acc2100ec=cov12ec*acc2100ec


label var cov12ec "effect code variable for coverage=12"

/* lastly, let's separate out the block from the rotation – we have 5 blocks (different sets of choice sets given to different sub-samples), and 4 different rotations across the 4 choice sets within each block. Currently this variable is entered as 1.1 – 5.4, we want to separate the first digit from the second*/

rename block origBlock
gen block = round(origBlock) /*convenient way to pick the first digit */
order id-origBlock block
gen rotation=(origBlock-block)*10 /*convenient way to pick the second digit */
order id-block rotation

/* re-order and save */
order id-vote choice
sort id set option

/* now everything is in the format STATA expects to run MNL-type models – let's save */
save f:\klaus\RTaerosol\stata\data\MNLdata,replace

/*

/*************************************************************/
/* Run MNL models */
/************************************************************/

/* see if we can run a simple linear clogit with main effects only */
clogit choice sq cov acc1 acc2 bid, group(idset) 
/* no problem */

/* add linear interactions */
clogit choice sq cov acc1 acc2 covacc1 covacc2 acc1acc2 bid, group(idset)
/* works but not so hot */

/* let's run the largest model identified by the design */
/********************************************************/
clogit choice sq cov12 acc175 acc1100 acc275 acc2100 cov12acc175 cov12acc1100 cov12acc275 cov12acc2100 acc1acc2 bid, group(idset)
/* OK, everything is identified, nothing got dropped, but only bid is significant */

/* check if sequencing of choice sets matters */
/******************************************/
/* need to interact this with an attribute, else drops out; let's choose bid, the most significant from before*/

tab set, gen(set)
gen bidset2=bid*set2
gen bidset3=bid*set3
gen bidset4=bid*set4


clogit choice sq cov12 acc175 acc1100 acc275 acc2100 cov12acc175 cov12acc1100 cov12acc275 cov12acc2100 acc1acc2 bid bidset2 bidset3 bidset4, group(idset)
/* none of the set interactions are significant, so we're good */
/* but just to be save, we should also run our best model using the first set only – by design of the CE this should still identify all effects – let's check*/

clogit choice sq cov12 acc175 acc1100 acc275 acc2100 cov12acc175 cov12acc1100 cov12acc275 cov12acc2100 acc1acc2 bid, group(idset), if set==1 /* works, but we lose some efficiency, of course */

/* repeat, dropping protest NO's */
/**********************************/
/* I already flagged protest responses with the "protNO" variable – we should exclude these cases if the model remains identified  */

clogit choice sq cov12 acc175 acc1100 acc275 acc2100 cov12acc175 cov12acc1100 cov12acc275 cov12acc2100 acc1acc2 bid, group(idset), if protNO~=1 

clogit choice sq cov12 acc175 acc1100 acc275 acc2100 cov12acc175 cov12acc1100 cov12acc275 cov12acc2100 acc1acc2 bid, group(idset), if protNO~=1 & set==1

/* still identified, SQ coeff changes dramatically as expected*/
/* given that we don't have identification problems, proceed without protest NOs */

/* interactions don't look promising, let's run model without them */
clogit choice sq cov12 acc175 acc1100 acc275 acc2100 bid, group(idset), if protNO~=1 
/* a lot better, several significant variables, and LLH value hardly changes, no-brainer */

/* re-run with effect codes – this should leave bid coefficient and overall model stats unchanged */
clogit choice sq cov12ec acc175ec acc1100ec acc275ec acc2100ec bid, group(idset), if protNO~=1 

/* WTP estimates */
/*****************************/
/*****************************/

/* WTP for a 6 mile band, 50/50 accuracy – lowest quality forecast*/
nlcom -(1/_b[bid])*(_b[cov12ec]*(-1)+_b[acc175ec]*(-1)+_b[acc1100ec]*(-1)+_b[acc275ec]*(-1)+_b[acc2100]*(-1) - _b[sq])

/* see if we get the same using basic dummies */
clogit choice sq cov12 acc175 acc1100 acc275 acc2100 bid, group(idset), if protNO~=1
nlcom -(1/_b[bid])*(- _b[sq]) /* exactly same, same CI, same efficiency */

/* let's stick with basic dummies – easier to interpret */
/*******************************************************/
/* again, 6 / 50 / 50 */
clogit choice sq cov12 acc175 acc1100 acc275 acc2100 bid, group(idset), if protNO~=1
nlcom -(1/_b[bid])*(- _b[sq])

/* 6 / 50 / 75 */
nlcom -(1/_b[bid])*(_b[acc275] - _b[sq])

/* 6 / 50 / 100 */
nlcom -(1/_b[bid])*(_b[acc2100] - _b[sq])

/* 6 / 75 / 50 */
nlcom -(1/_b[bid])*(_b[acc175] - _b[sq])

/* 6 / 75 / 75 */
nlcom -(1/_b[bid])*(_b[acc175]+ _b[acc275] - _b[sq])

/* next steps / extensions:

interact attributes with socio-demographics (here, perhaps, number of children, time spent outdoors, number of outdoor activities, etc.)

Let SQ constant be random – different respondents might value the existing RT info sources differently

Move on to fully mixed logit, latent class logit etc. – see below
*/


/* other free STATA packages to estimate choice models with >2 alternatives:

mixlogit – Mixed Logit (random coefficients)
lclogit – latent class logit (finite groups of different coefficients)
gmnl – generalized mixed logit
bayesmlogit – Bayesian mixed logit 

for a nice introduction to these commands see presentation slides "Mixed logit modelling in STATA" by Arne Risa Hole (2013)

*/
