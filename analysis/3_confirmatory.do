********************************************************************************
********************************************************************************
** IDS confirmatory analyses ***************************************************
** updated for GitHub 06apr2020 ************************************************
********************************************************************************
********************************************************************************

********************************************************************************
** CONFIG **********************************************************************
********************************************************************************

** set your directory to the cloned GitHub repo
cd ~/git/infant-vocal

** stata working parameters
clear all
cls
set more off
set rmsg on

** get data & destring
insheet using ./data/IDS_Winsor.csv, names
destring praat_* mir_* tm_* npvi_*, replace force // this removes all #DIV/0!s etc

********************************************************************************/
** RESTRICT TO CONFIRMATORY SAMPLE *********************************************
********************************************************************************

** even numbered participants will be in the exploratory sample
** odd numbered participants will be in the confirmatory sample
** this nests sampling within fieldsites

** tag even participants and keep them
gen exploratory = mod(id_person,2)==0
keep if exploratory==0

********************************************************************************
** REGRESSION ANALYSES *********************************************************
********************************************************************************

** shorten variable names
rename *intensitytravel* *inttrav*
rename *voweltravel* *vowtrav*
rename *_first_quart *_1q
rename *_third_quart *_3q
rename *_median *_med
rename *_first_quar *_1q
rename *_third_quar *_3q
rename *_default *_df

** drop mean, min, max, stdev, range
drop *_mean *_min *_max *_std *_range

** log regression results
log using ./results/IDS_confirmatory, replace

** run all regressions
foreach v of varlist praat_* mir_* tm_* npvi_* {

	di "=============================================================================="
	di "=============================================================================="
	di "==== we are now testing `v' "
	di "=============================================================================="
	di "=============================================================================="
	
	** run main regression
	cap noi: meglm `v' infantdir song ids || id_site: || id_person:, vce(cluster id_site)
	** this is a fixed-effects model of FEATURE on target, utterance type, and their interaction
	** (the way this is coded means interaction == infant-directed song or everything else)
	** note: this regression is NOT bootstrapped, rather it uses the clustered sandwich estimator
	** for standard errors.
	
	** store output in matrix
	matrix `v'_reg0 = r(table)
	matrix `v'_reg1 = `v'_reg0'
	matselrc `v'_reg1 `v'_reg, row(1/4)
	
	** run linear combinations & store them in matrices
	lincom infantdir
	matrix `v'_LC1 = (r(estimate),r(se),r(z),r(p))
	matrix colnames `v'_LC1 = est se z p
	** this is a linear combination testing effect of infant-directedness vs adult-directedness,
	** regardless of utterance type
	lincom infantdir + ids
	matrix `v'_LC2 = (r(estimate),r(se),r(z),r(p))
	matrix colnames `v'_LC2 = est se z p
	** this is a linear combination testing effect of ID song vs AD song
	** (song cancels out, ID-ness + ID song = 0
	lincom ids + song
	matrix `v'_LC3 = (r(estimate),r(se),r(z),r(p))
	matrix colnames `v'_LC3 = est se z p
	** this is a linear combination testing effect of ID song vs ID speech
	** (ID cancels out, song + ID song = 0
	
	di ""
	di ""
	
	}
log close

** build lincom matrices into table
foreach v of varlist praat_* mir_* tm_* npvi_* {
	preserve
	clear
	svmat `v'_LC1, names(col)
	gen feature = "`v'"
	gen compare = "ID vs AD"
	cap sa ./results/temp/`v'_LC1, replace	
	clear
	svmat `v'_LC2, names(col)
	gen feature = "`v'"
	gen compare = "ID song vs AD song"
	sa ./results/temp/`v'_LC2, replace
	clear
	svmat `v'_LC3, names(col)
	gen feature = "`v'"
	gen compare = "ID song vs ID speech"
	sa ./results/temp/`v'_LC3, replace
	restore
	}
	
** make big table of all lincoms
preserve
clear
cd ./results/temp
fs *_LC1.dta *_LC2.dta *_LC3.dta
append using `r(files)'
order feature compare
cd ~/git/ids
gsort -compare feature
sa ./results/IDS-confirmatory-lincoms, replace
restore

** build regression matrices into table
foreach v of varlist praat_* mir_* tm_* npvi_* {
	preserve
	clear
	svmat `v'_reg, names(col)
	gen feature = "`v'"
	gen coef = ""
	replace coef = "target (infant == 1)" in 1
	replace coef = "utterance (song == 1)" in 2
	replace coef = "target x utterance" in 3
	replace coef = "constant" in 4
	sa ./results/temp/`v'_reg, replace
	restore
	}

** make big table of all regression matrices
preserve
clear
cd ./results/temp
fs *_reg.dta
append using `r(files)'
order feature coef
cd ~/git/ids
sort feature
sa ./results/IDS-confirmatory-regtables, replace
restore
