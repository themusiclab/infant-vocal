********************************************************************************
********************************************************************************
** IDS analyses: listener experiment *******************************************
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

** get data
insheet using ./data/IDS_naiveListeners.csv, names

********************************************************************************
** ANALYSES ********************************************************************
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
collapse (mean) baby, by(stim fieldsite cat sung infdir)
bysort fieldsite: ttest baby==.5 if cat=="id song"
bysort fieldsite: ttest baby==.5 if cat=="id speech"
bysort fieldsite: ttest baby==.5 if cat=="ad song"
bysort fieldsite: ttest baby==.5 if cat=="ad speech"
restore

** get stimwise sds // note that this is NOT dynamic
preserve
collapse (mean) baby rt, by(stim)
su baby rt
restore
loc baby_sd = .1424434
loc rt_sd = .4324774

** pairwise comparisons of IDness (baby only) (id vs ad grouped)
preserve
collapse (mean) baby, by(stim infdir)
reshape wide baby, i(stim) j(infdir)
ttest baby1==baby0
di (r(mu_1)-r(mu_2))/`baby_sd'
restore

** remove spaces from cat
replace cat = subinstr(cat," ","",.)

** pairwise comparisons of IDness (baby only) (id song vs ad song; id song vs id speech)
preserve
collapse (mean) baby, by(stim cat)
reshape wide baby, i(stim) j(cat, string)
ttest babyidsong==babyadsong
di (r(mu_1)-r(mu_2))/`baby_sd'
ttest babyidsong==babyidspeech
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
reshape wide rt, i(stim) j(cat, string)
ttest rtidsong==rtadsong
di (r(mu_1)-r(mu_2))/`rt_sd'
ttest rtidsong==rtidspeech
di (r(mu_1)-r(mu_2))/`rt_sd'
restore
