** Written by: Bryan Parthum; bparthum@gmail.com ; April 2020

********************************************************************************
********************   D-EFFICIENT EXPERIMENT DESIGN   *************************
********************************************************************************

clear all
set more off

// ** SET WORKING DIRECTORY
// cd ""

// ** INSTALL PACKAGE TO CREATE D-EFFICIENT DESIGN
// ssc install dcreate

use N_dominated, clear
while N_dominated > 0 { 
qui{

clear all
set more off

local rand = runiformint(0,20000) 
set seed `rand' 
// di c(seed) 

*****************************************
********  CONSTRUCT FULL FACTORIAL MATRIX 
*****************************************

matrix levels = 3, /// *Cost: 			1) 10 	2) 50	3) 100
				2, /// *Distance: 		1) 6mi 	2) 12mi 
				3, /// *Accuracy 12:	1) 60% 	2) 80%  3) 100% 
				3  /*  *Accuracy 24:	1) 40% 	2) 60%  3) 80%  */
genfact, levels(levels)
// list, separator(3)

*****************************************
*********************************  RENAME
*****************************************

rename (x1 x2 x3 x4) ///
	   (cost ///
        dist ///
		ac12 ///
		ac24)

*****************************************
*********************************  RECODE
*****************************************

recode cost  (1=10) (2=50) (3=100)
recode dist  (1=6) 	(2=12)
recode ac12  (1=60) (2=80) (3=100)
recode ac24  (1=20) (2=40) (3=60)

*****************************************
**************************  IMPOSE PRIORS
*****************************************

mat status_quo = 0,0,0,0
mat betas = J(1,8,0)	/* Starting values for betas */	
// mat covar = ()	    /* Starting values for covariance matrix */

*****************************************
********************  GENERATE EXPERIMENT
*****************************************

dcreate i.cost ///
		i.dist ///
		i.ac12 ///
		i.ac24, ///
		nalt(2) ///
		nset(16) ///
		bmat(betas) ///
		seed(`rand') ///
		fixedalt(status_quo) ///
		asc(3)

*****************************************
************************  GENERATE BLOCKS
*****************************************

blockdes block, nblock(4) neval(40) seed(`rand')

*****************************************
*****************  SORT, ORDER, AND CLEAN
*****************************************

rename 	choice_set card_dcreate
sort 	block card_dcreate alt
gen 	alt_id = _n
egen 	card_id = group(block card_dcreate)
sort 	block alt alt_id
by 		block alt: gen card = _n
sort 	block card alt

gen 	title = "No Project"  if alt == 3
replace title = "Project 1" if alt == 1 
replace title = "Project 2" if alt == 2 

tostring dist, replace
replace  dist = "None" if dist == "0"
replace  dist = "6 miles" if dist == "1"
replace  dist = "12 miles" if dist == "2"

order block card alt ///
      title ///
	  cost ///
	  dist ///
	  ac12 ///
	  ac24 ///
	  alt_id card_id card_dcreate

save EXdesign_test, replace
// save design_matrix, replace
// export excel using design_matrix.xlsx, firstrow(variables) replace

*****************************************
****************  TEST STRICTLY DOMINATED
*****************************************

use EXdesign_test, clear

drop if alt == 3
// drop location title alt_id card_dcreate
keep cost dist ac12 ac24 card_id alt

reshape wide cost dist ac12 ac24, i(card_id) j(alt)

// gen dominated = (ac121        < ac241 |  ///
// 				 ac122        < ac242)  
				 
gen dominated = (ac121        < ac241 |  ///
				 ac122        < ac242 |  ///
				(ac121        < ac122 &  ///
				 ac241        < ac242 &  ///
				 dist1        > dist2 &  ///
				 cost1        > cost2) | ///
				(ac121        > ac122 &  ///
				 ac241        > ac242 &  ///
				 dist1        < dist2 &  ///
				 cost1        < cost2))   

egen N_dominated = sum(dominated)
keep N_dominated
duplicates drop 

tempfile dom
save `dom'
// save N_dominated, replace /* Just storing the number of dom stratagies in case you're curious how many there were in each iteration. */
append using N_dominated
save N_dominated, replace
}

sum N_dominated

qui use `dom', clear

}

*****************************************
***********************************  VIEW
*****************************************
use EXdesign_test, clear
list, separator(3) abbreviate(16)

*****************************************
*********************************  EXPORT
*****************************************

save EXdesign, replace
export excel using EXdesign.xlsx, firstrow(variables) replace

** END OF SCRIPT. Have a nice day!
