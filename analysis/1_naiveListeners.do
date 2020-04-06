********************************************************************************
********************************************************************************
** IDS analyses: listener experiment *******************************************
** 05oct2019- ******************************************************************
** Samuel Mehr *****************************************************************
********************************************************************************
********************************************************************************

clear all
set more off

** set your directory to the cloned repo
cd ~/git/ids

********************************************************************************
** clean data and extract covariates *******************************************
** [uncomment to rebuild processed dataset] ************************************
********************************************************************************

/* NOTE: this script does not collect covariates since the swap to centralized covariates!
** which means that a bunch of recent participants have no covariates analyzed */

** get preprocessed data
use ./data/TML-IDS-preprocess.dta, replace

** cleanup
*drop *indx tc*

** remove random duplicates within stimulus id
duplicates drop id stimulus, force

/* generate subject-level data */

** age
gen age_x = ""
replace age_x = resp if stim=="age"
destring age_x, replace
egen age = mode(age_x), by(id)

** gender
gen female_x = .
replace female_x = 1 if resp=="Female"&stim=="gender"
replace female_x = 0 if resp=="Male"&stim=="gender"
replace female_x = 99 if resp=="Other"&stim=="gender"
egen female = mode(female_x), by(id)

** country
gen country_x = resp if stim=="country"
egen country = mode(country_x), by(id)

** language
gen lang_x = resp if stim=="language"
egen lang = mode(lang_x), by(id)

** income
replace stim = "income" if strpos(stim,"current household income")>0

** make music vars
foreach stub in "musictrad" "musicskill" "parentsing" {
	gen `stub'_x = resp if stim=="`stub'"
	egen `stub' = mode(`stub'_x), by(id)
	drop `stub'_x
	}

** recode music vars
replace musictrad = "1" if strpos(musictrad,"never heard")>0
replace musictrad = "2" if strpos(musictrad,"little familiar")>0
replace musictrad = "3" if strpos(musictrad,"somewhat familiar")>0
replace musictrad = "4" if strpos(musictrad,"very familiar")>0
replace musictrad = "5" if strpos(musictrad,"extremely familiar")>0
replace musicskill = "1" if strpos(musicskill,"no skill")>0
replace musicskill = "2" if strpos(musicskill,"novice")>0
replace musicskill = "3" if strpos(musicskill,"some skill")>0
replace musicskill = "4" if strpos(musicskill,"lot of skill")>0
replace musicskill = "5" if strpos(musicskill,"expert")>0
replace parentsing = "1" if strpos(parentsing,"every 3 days or less")>0
replace parentsing = "2" if strpos(parentsing,"day or two")>0
replace parentsing = "3" if strpos(parentsing,"2-3 times")>0
replace parentsing = "4" if strpos(parentsing,"4-7 times")>0
replace parentsing = "5" if strpos(parentsing,"8 or more")>0
replace parentsing = "." if strpos(parentsing,"don't know")>0
destring music* parentsing, replace

** played before
gen playbefore_x = resp if stim=="donebefore"
egen playbefore = mode(playbefore_x), by(id)

** drop tempvars
drop *_x

/* deal with stim */
** get stim names & replace them in stim
replace stim = ustrregexs(1) if ustrregexm(stim,"([A-Z][A-Z][A-Z][0-9][0-9][A-D]).mp3")
** drop practice trials
drop if stim=="TOR47A"|stim=="WEL10C"
** get response image orders (desktops only)
gen keybaby = ustrregexs(1) if ustrregexm(string,`""image_order1":.+?"baby","key":"([f-j]).+?"adult","key":"([f-j]).*\]"')
gen keyadult = ustrregexs(2) if ustrregexm(string,`""image_order1":.+?"baby","key":"([f-j]).+?"adult","key":"([f-j]).*\]"')
foreach v of varlist keybaby keyadult {
	replace `v' = "70" if `v'=="f"
	replace `v' = "74" if `v'=="j"
	destring `v', replace
	}
** get response image orders (mobiles only; old version)
gen buttonbaby = ustrregexs(1) if ustrregexm(string,`""image_order":.+?"baby","button":([0-1]).+?"adult","button":([0-1]).*\]"')
gen buttonadult = ustrregexs(2) if ustrregexm(string,`""image_order":.+?"baby","button":([0-1]).+?"adult","button":([0-1]).*\]"')
** get response image orders (mobiles only; new version from code change on 25apr2019)
replace buttonbaby = ustrregexs(1) if ustrregexm(string,`""correct_response":"baby","button":([0-1]),""')
replace buttonadult = ustrregexs(2) if ustrregexm(string,`""correct_response":"adult","button":([0-1]),""')
destring buttonbaby buttonadult, replace

** populate responses
replace resp = "baby" if ismobile==0 & ustrregexm(stim,"[A-Z][A-Z][A-Z][0-9][0-9][A-D]") & key_press==keybaby
replace resp = "adult" if ismobile==0 & ustrregexm(stim,"[A-Z][A-Z][A-Z][0-9][0-9][A-D]") & key_press==keyadult
replace resp = "baby" if ismobile==1 & ustrregexm(stim,"[A-Z][A-Z][A-Z][0-9][0-9][A-D]") & button_press==buttonbaby
replace resp = "adult" if ismobile==1 & ustrregexm(stim,"[A-Z][A-Z][A-Z][0-9][0-9][A-D]") & button_press==buttonadult

** restrict to song ratings only
keep if ustrregexm(stim,"[A-Z][A-Z][A-Z][0-9][0-9][A-D]")==1

** generate metadata
egen nplays = count(resp), by(id) 	// number of plays per subject
gen type = "" 						// actual vocalization type
replace type = "baby" if ustrright(stim,1)=="A"|ustrright(stim,1)=="B"
replace type = "adult" if ustrright(stim,1)=="C"|ustrright(stim,1)=="D"
gen sung = ustrright(stim,1)=="A"|ustrright(stim,1)=="C"
gen infdir = ustrright(stim,1)=="A"|ustrright(stim,1)=="B"

** generate and relabel fieldsites // DO THIS
gen fieldsite = ustrleft(stim,3)	// fieldsite

/* cleanup! */

** drop testing data
drop if age==3

** drop empty responses
drop if resp==""

** make categories
gen cat = .
replace cat = 1 if sung==1&infdir==1
replace cat = 2 if sung==0&infdir==1
replace cat = 3 if sung==1&infdir==0
replace cat = 4 if sung==0&infdir==0
la def types 1 "id song" 2 "id speech" 3 "ad song" 4 "ad speech"
la val cat types

** score all items
gen score = resp==type

** response time cleaning
replace rt = . if score==0
su rt
loc rtN = r(N)
su rt, d
replace rt = . if rt<r(p1)
replace rt = . if rt>r(p99)
su rt
di r(N)/`rtN'
replace rt = rt/1000

** export collapsed version for convergent analyses
preserve
gen baby = response=="baby"
gen adult = response=="adult"
gen rt_correct = rt if score==1
collapse (mean) baby adult score rt rt_correct, by(stimulus cat sung infdir)
compress
rename stimulus id
gen pers = ustrleft(id,5)
save ./results/TML-IDS_songCollapse, replace
export delimited using ./viz/vizData/IDS_fig2.csv, replace
restore

** rename stim to fieldsite/participant only
replace stim = ustrleft(stim,5)

** cleanup
compress
sort id trial_indx
gen baby = resp=="baby"
order id *indx rt stim resp age-playbefore nplays-cat score baby
keep id *indx rt stim resp age-playbefore nplays-cat score baby

** save
sa ./results/TML-IDS_processed, replace

** export to csv for Max d' code
export delimited using ./results/TML-IDS_processed.csv, replace

** export to csv for OSF
preserve
keep id trial_indx indx rt stimulus response type age female country lang
order id trial_indx indx age female country lang stimulus type response rt
export delimited using ./results/IDS_naiveListeners.csv, replace
restore

** export to csv for Fig S2
preserve
collapse (mean) baby, by(stim fieldsite cat sung infdir)
egen fsm = mean(baby), by(fieldsite)
egen n = count(stimulus), by(fieldsite)
export delimited using ./viz/vizData/IDS_figS2.csv, replace
restore

********************************************************************************/
** analyses ********************************************************************
********************************************************************************

** demographics
preserve
collapse (firstnm) stim, by(id)
di _N
restore
preserve
collapse (firstnm) age female playbefore nplays, by(id)
ta female
su age, d
ta playbefore
ta nplays
restore
preserve
collapse (firstnm) stim country lang, by(id)
ta country
ta lang
restore
preserve
collapse (firstnm) id, by(country)
di _N
restore
preserve
collapse (firstnm) id, by(lang)
di _N
restore

** number of subjects per site
preserve
collapse (firstnm) id, by(stim fieldsite)
collapse (count) id, by(fieldsite)
su id, d
restore

** songwise summary stats
ta sung infdir

** scores (weighted by stim overall)
preserve
collapse (mean) score, by(stim)
ttest score==.5
restore

** overall & fieldsitewise stuff
preserve
collapse (mean) score baby, by(stim fieldsite cat sung infdir)
bysort fieldsite: ttest baby==.5 if cat==1
bysort fieldsite: ttest baby==.5 if cat==2
bysort fieldsite: ttest baby==.5 if cat==3
bysort fieldsite: ttest baby==.5 if cat==4
restore

** get stimwise sds // note that this is NOT dynamic
preserve
collapse (mean) score baby rt, by(stim)
su score baby rt
restore
loc score_sd = .1478473
loc baby_sd = .1424434
loc rt_sd = .4324774

** pairwise comparisons of IDness (baby only) (id vs ad grouped)
preserve
collapse (mean) score baby, by(stim infdir)
reshape wide score baby, i(stim) j(infdir)
ttest baby1==baby0
di (r(mu_1)-r(mu_2))/`baby_sd'
restore

** pairwise comparisons of IDness (baby only) (id song vs ad song; id song vs id speech)
preserve
collapse (mean) score baby, by(stim cat)
reshape wide score baby, i(stim) j(cat)
ttest baby1==baby3
di (r(mu_1)-r(mu_2))/`baby_sd'
ttest baby1==baby2
di (r(mu_1)-r(mu_2))/`baby_sd'
restore

** pairwise comparisons of accuracy (rt only) (id vs ad grouped)
preserve
collapse (mean) rt, by(fieldsite stim infdir)
reshape wide rt, i(fieldsite stim) j(infdir)
ttest rt1==rt0
di (r(mu_1)-r(mu_2))/`rt_sd'
restore

** pairwise comparisons of accuracy (rt only) (id song vs ad song; id song vs id speech)
preserve
collapse (mean) rt, by(stim cat)
reshape wide rt, i(stim) j(cat)
ttest rt1==rt3
di (r(mu_1)-r(mu_2))/`rt_sd'
ttest rt1==rt2
di (r(mu_1)-r(mu_2))/`rt_sd'
restore
