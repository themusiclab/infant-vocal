********************************************************************************
********************************************************************************
** IDS analyses: convergent ****************************************************
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

** remove tempfiles
cd ./results/temp // fs deals weirdly with directories
fs *_bbreg.dta
foreach f in `r(files)' {
	rm `f'
	}
cd ~/git/infant-vocal

********************************************************************************
** GET DATA ********************************************************************
********************************************************************************

** get data
insheet using ./data/IDS_Winsor.csv, names

** merge in ID scores
mer m:m id using ./results/IDS_songCollapse

** keep only stim that have id scores
keep if _merge==3
destring praat* mir* tm* npvi*, replace force

********************************************************************************
** CLEANUP *********************************************************************
********************************************************************************

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

** normalize all values within person
egen indx_person = group(id_site id_person)
egen nperson = count(id), by(indx_person)
drop if nperson!=4
foreach v of varlist praat_* mir_* tm_* npvi_* {
	egen m`v' = mean(`v'), by(indx_person)  // get personwise mean for each feature
	egen var`v' = sd(`v')
	gen `v'_c = (`v'-m`v')/var`v' 			// center feature
	}
	
** export normalized scores for viz
preserve
keep id baby cat *_c
export delimited using ./viz/vizData/IDS_fig5.csv, replace
restore

********************************************************************************
** analyze *********************************************************************
********************************************************************************
	
** name features to analyze (from converge_vars.csv)
loc feats = "mir_attack_med_c mir_inharmonicity_c mir_rolloff85_c mir_roughness_iqr_c mir_roughness_med_c npvi_phrase_c praat_f0_iqr_c praat_f0_median_c praat_f0trv_iqr_c praat_f0trv_median_c praat_f0trv_rate_iqr_c praat_f0trv_rate_median_c praat_f1_median_c praat_it_iqr_c praat_it_median_c praat_ittrv_median_c praat_ittrv_rate_iqr_c praat_vowtrv_iqr_c praat_vowtrv_rate_iqr_c praat_vowtrv_rate_median_c tm_std_hz_c"

** all regressions
foreach v in `feats' { 

	di "=============================================================================="
	di "=============================================================================="
	di "==== we are now testing `v' "
	di "=============================================================================="
	di "=============================================================================="
	
	** run main regression
	reg baby `v'
	test `v'
	
	** store output in matrix
	matrix `v'_bbreg = (e(N),e(F),e(df_m),e(df_r),r(p),e(r2))
	matrix colnames `v'_bbreg = n f df1 df2 p r2
	
	/** plot // this moved to R viz
	tw sc baby `v' if infantdir==1&song==1, mc(eltblue%80) || ///
		sc baby `v' if infantdir==1&song==0, mc(midblue%80) || ///
		sc baby `v' if infantdir==0&song==1, mc(orange%70) || ///
		sc baby `v' if infantdir==0&song==0, mc(orange_red%80) || ///
		lowess baby `v', lw(medium) lc(purple%70) || ///
		lfitci baby `v', clc(red) clw(medthick) xti("`v'") yti("Infant-directedness score") ///
			leg(order(1 "ID song" 2 "ID speech" 3 "AD song" 4 "AD speech") col(4)) ///
			name("`v'", replace)
	*/
	di ""
	di ""
	
	}

** big regression // uses only bonferroni-corrected vars (hard-coded!!)
reg baby praat_f0_median_c ///
	praat_f0_iqr_c praat_ittrv_median_c tm_std_hz_c mir_roughness_iqr_c ///
	mir_inharmonicity_c mir_roughness_med_c praat_f0trv_iqr_c mir_attack_med_c ///
	mir_rolloff85_c praat_f1_median_c npvi_phrase_c 
pcorr baby praat_f0_median_c ///
	praat_f0_iqr_c praat_ittrv_median_c tm_std_hz_c mir_roughness_iqr_c ///
	mir_inharmonicity_c mir_roughness_med_c praat_f0trv_iqr_c mir_attack_med_c ///
	mir_rolloff85_c praat_f1_median_c npvi_phrase_c 

** build dta for each regression matrix
foreach v in `feats' { 
	preserve
	clear
	svmat `v'_bbreg, names(col)
	gen feature = "`v'"
	order feature
	sa ./results/temp/`v'_bbreg, replace
	restore
	}

** make big table of all regression matrices
preserve
clear
cd ./results/temp // fs deals weirdly with directories
fs *_bbreg.dta
append using `r(files)'
cd ../..
gsort -r2
sa ./results/IDS_acoustics_regs, replace
restore
