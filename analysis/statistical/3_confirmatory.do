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

********************************************************************************
** EXPORT FOR VIZ **************************************************************
********************************************************************************
	
insheet using ./data/IDS_Winsor.csv, names clear

** destring
destring praat* mir* tm* npvi*, replace force

** rename too long names
	foreach v of varlist *intensity* {
		loc n = subinstr("`v'","intensity","it",.)
		rename `v' `n'
		}
	foreach v of varlist *travel* {
		loc n = subinstr("`v'","travel","trv",.)
		rename `v' `n'
		}
	foreach v of varlist *vowel* {
		loc n = subinstr("`v'","vowel","vow",.)
		rename `v' `n'
		}
	foreach v of varlist *_quart {
		loc n = subinstr("`v'","first_quart","1q",.)
		rename `v' `n'
		}
	foreach v of varlist *_quart {
		loc n = subinstr("`v'","third_quart","3q",.)
		rename `v' `n'
		}

** normalize all vars
foreach v of varlist praat_* mir_* tm_* npvi_* {
	egen m`v' = mean(`v')
	egen sd`v' = sd(`v')
	replace `v' = (`v'-m`v')/sd`v'
	}
	
** restrict to confirmatory vars // these are from ./viz/tables/TableS4.csv
keep id-song mir_attack_med mir_inharmonicity mir_rolloff85 mir_roughness_iqr mir_roughness_med npvi_phrase npvi_total praat_f0_iqr praat_f0_med praat_f0trv_iqr praat_f0trv_med praat_f0_trv_rate_default praat_f0trv_rate_iqr praat_f0trv_rate_med praat_f1_med praat_it_iqr praat_it_med praat_ittrv_iqr praat_ittrv_med praat_ittrv_rate_defau praat_ittrv_rate_iqr praat_ittrv_rate_med praat_vowtrv_iqr praat_vowtrv_med praat_vowtrv_rate_default praat_vowtrv_rate_iqr praat_vowtrv_rate_med 

** reshape
foreach v of varlist praat_* mir_* npvi_* { //tm_* 
	rename `v' f_`v'
	}
reshape long f_, i(id) j(feat, str)
rename f_ z

** labels
la def infdir 1 "Infant-directed" 0 "Adult-directed"
la val infantdir infdir
la def song 1 "Song" 0 "Speech"
la val song song

** rename feats
replace feat = "Attack Curve Slope (Median)" if feat=="mir_attack_med"
replace feat = "Inharmonicity" if feat=="mir_inharmonicity"
replace feat = "Energy Roll-off (85th %ile)" if feat=="mir_rolloff85"
replace feat = "Roughness (IQR)" if feat=="mir_roughness_iqr"
replace feat = "Roughness (Median)" if feat=="mir_roughness_med"
replace feat = "nPVI (per phrase)" if feat=="npvi_phrase"
replace feat = "nPVI (per recording)" if feat=="npvi_total"
replace feat = "Pitch (IQR)" if feat=="praat_f0_iqr"
replace feat = "Pitch (Median)" if feat=="praat_f0_median"
replace feat = "Pitch Space (IQR)" if feat=="praat_f0trv_iqr"
replace feat = "Pitch Space (Median)" if feat=="praat_f0trv_median"
replace feat = "Pitch Rate (Whole)" if feat=="praat_f0_trv_rate_default"
replace feat = "Pitch Rate (IQR)" if feat=="praat_f0trv_rate_iqr"
replace feat = "Pitch Rate (Median)" if feat=="praat_f0trv_rate_median"
replace feat = "First Formant (Median)" if feat=="praat_f1_median"
replace feat = "Intensity (IQR)" if feat=="praat_it_iqr"
replace feat = "Intensity (Median)" if feat=="praat_it_median"
replace feat = "Intensity Space (IQR)" if feat=="praat_ittrv_iqr"
replace feat = "Intensity Space (Median)" if feat=="praat_ittrv_median"
replace feat = "Intensity Rate (Whole)" if feat=="praat_ittrv_rate_defau"
replace feat = "Intensity Rate (IQR)" if feat=="praat_ittrv_rate_iqr"
replace feat = "Intensity Rate (Median)" if feat=="praat_ittrv_rate_media"
replace feat = "Vowel Space (IQR)" if feat=="praat_vowtrv_iqr"
replace feat = "Vowel Space (Median)" if feat=="praat_vowtrv_median"
replace feat = "Vowel Rate (Whole)" if feat=="praat_vowtrv_rate_default"
replace feat = "Vowel Rate (IQR)" if feat=="praat_vowtrv_rate_iqr"
replace feat = "Vowel Rate (Median)" if feat=="praat_vowtrv_rate_median"

** export for viz
export delimited using ./viz/vizData/IDS_fig3.csv, replace
