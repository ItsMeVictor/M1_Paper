
/***************************************************************************
*** 1. SET UP **************************************************************
****************************************************************************/

drop _all
eststo clear

	/// Download required external packages 
*capture ssc install ivreg2
*capture ssc install xtoverid
*capture ssc install xttest3
*capture ssc install estout

	/// Path & data 
use "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/data/final_data/final_merge.dta"

	/// Set data as panel data 
encode code_insee, generate(depcom)
sort depcom year
xtset depcom year

	/// Keep only cross-sections that are included in the balanced panel 
egen num_periods = total(1), by(depcom)
su num_periods, meanonly
scalar max_periods = r(max)
keep if num_periods == max_periods

	/// Add labels to our variables for future tables **
label variable depcom "Code commune"
label variable year "Year"
label variable code_insee "Code Insee"
label variable immi_prop "\% Immigrants"
label variable extdr_leg "\% Far-right votes"
label variable chomeurs_prop "\% Unemployed"
label variable dropouts "\% Dropouts"
label variable hlm_prop "\% Subsidized housing"
label variable ouvriers_prop "\% Workers"
label variable employes_prop "\% Employees"
label variable retraites_prop "\% Retirees"
label variable cadres_prop "\% Cadres"
label variable agriculteurs_prop "\% Farmers"
label variable pop_commune "Population" 
label variable schools "Number of schools"
label variable avg5_popdept_growth "5-year population growth (d)"
label variable pop_dept "Population (d)"
label variable crime_dept_prop "Crime rate (d)"
label variable avg5_naissances_dept "5-year birth rate (d)"
label variable logements_dept_prop "Dwellings construction rate (d)"

	/// Drop all variables that do not have a label (for simplicity, given how extensive the dataset is) **
ds
foreach var of varlist `r(varlist)' {
    if "`: variable label `var''" == "" {
        drop `var'
    }
}

/***************************************************************************
*** 2. MODEL SPECIFICATION *************************************************
****************************************************************************/
	
	/// Hsiao specification test

* Step 1
reg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept 

scalar pooled_RSS = e(rss)
scalar pooled_rdof = e(N) - e(df_m)

*scalar heterogeneous_RSS = 0
*scalar heterogeneous_rdof = 0

*forvalues i = 1/`=_N' {
*    quietly reg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept if depcom == `i'
*    scalar heterogeneous_RSS = heterogeneous_RSS + e(rss)
*    scalar unit_rdof = e(N) - e(df_m)
*    scalar heterogeneous_rdof = heterogeneous_rdof + unit_rdof
*}

	* Estimate separate OLS models for each cross-sectional unit and store RSS and r_dof
*bysort depcom: reg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept
*bysort depcom: egen unit_rss = total(e(rss))
*bysort depcom: egen unit_rdof = total(e(N) - e(df_m))

	* Calculate combined RSS and r_dof for all separate OLS models
*egen heterogeneous_RSS = total(unit_rss), by(depcom)
*egen heterogeneous_rdof = total(unit_rdof), by(depcom)

*scalar f_stat = ((heterogeneous_RSS - pooled_RSS) / (heterogeneous_rdof - pooled_rdof)) / (pooled_RSS / pooled_rdof)

* step 2

* step 3


***********************************************************************************************************************************************************************************************
	/// Stationarity
	
local variables "immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept"

foreach var of local variables {
    display "Performing ADF test for `var'"
    dfuller `var', lags(1)
}

***********************************************************************************************************************************************************************************************
	/// Robust Hausman test 

* Fixed effects estimation
xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept, fe

estimates store fe_model 

* Random effects estimation
xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept, re

estimates store re_model

* Robust Hausman
xtoverid

***********************************************************************************************************************************************************************************************
	/// F-test for fixed temporal effects 

* Unit-fixed effects estimation (same model as above)
xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept, fe

estimates store fe_model 

* Unit-fixed and time-fixed effects estimation
xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept i.year, fe

estimates store fe_model_with_time

* F-test
testparm i.year

***********************************************************************************************************************************************************************************************		/// Testing for heteroskedasticity 

* Model estimation (same model as above, both temporal and unit-fixed effects)
xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept i.year, fe

* Test itself
xttest3

***********************************************************************************************************************************************************************************************
	/// Testing for serial correlation

* xtserial does not allow for factor variables, so we create dummies for the time periods first
sort year
tabulate year, generate(year_dummy)

tset depcom year, delta(5)
xtserial immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept year_dummy1 year_dummy2 year_dummy3, output

***********************************************************************************************************************************************************************************************
	/// Dynamic modeling 

* Extend dataset for additional time-periods - Setup
drop _all

use "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/data/final_data/extend_merge.dta"

encode code_insee, generate(depcom)
sort depcom year
xtset depcom year

egen num_periods = total(1), by(depcom)
su num_periods, meanonly
scalar max_periods = r(max)
keep if num_periods == max_periods

* Add labels to our variables for future tables 
label variable depcom "Code commune"
label variable year "Year"
label variable code_insee "Code Insee"
label variable immi_prop "\% Immigrants"
label variable extdr_leg "\% Far-right votes"
label variable chomeurs_prop "\% Unemployed"
label variable dropouts "\% Dropouts"
label variable hlm_prop "\% Subsidized housing"
label variable ouvriers_prop "\% Workers"
label variable employes_prop "\% Employees"
label variable retraites_prop "\% Retirees"
label variable cadres_prop "\% Cadres"
label variable agriculteurs_prop "\% Farmers"
label variable pop_commune "Population" 
label variable schools "Number of schools"
label variable avg5_popdept_growth "5-year population growth (d)"
label variable pop_dept "Population (d)"
label variable crime_dept_prop "Crime rate (d)"
label variable avg5_naissances_dept "5-year birth rate (d)"
label variable logements_dept_prop "Dwellings construction rate (d)"

* Drop all variables that do not have a label (for simplicity, given how extensive the dataset is) **
ds
foreach var of varlist `r(varlist)' {
    if "`: variable label `var''" == "" {
        drop `var'
    }
}

est clear
	
* Static estimation

xtreg immi_prop extdr_leg cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept i.year, fe vce(cluster depcom)

eststo static1

* Lagged variables and dynamic estimation
tset depcom year, delta(5)

gen lag_extdrleg1 = L.extdr_leg
gen lag_extdrleg2 = L2.extdr_leg	

label variable lag_extdrleg1 "1st order lagged far-right votes"
label variable lag_extdrleg2 "2nd order lagged far-right votes"

xtreg immi_prop lag_extdrleg1 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop schools avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop  pop_commune pop_dept i.year, fe vce(cluster depcom)

eststo dynamic1

xtreg immi_prop lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop schools avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop  pop_commune pop_dept i.year, fe vce(cluster depcom)

eststo dynamic2

xtreg immi_prop extdr_leg lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept i.year, fe vce(cluster depcom)

eststo dynamic3

esttab static1 dynamic1 dynamic2 dynamic3 ///
	using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/static_dynamic.tex", replace ///
	label ///
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2_a N, labels("Adj. R-squared" "Number of observations")) ///
	title("Regression results – Static and dynamic modeling") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("Static" "Dynamic (1st)" "Dynamic (2nd)" "Dynamic (full)" ) ///
	drop(2007.year _cons pop_commune pop_dept) ///
	order(extdr_leg lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop schools avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop pop_commune pop_dept 2012.year 2017.year) /// 
	collabels(none) ///
	legend /// 
	addnote("Population variables dropped from the table. See table \ref{tab:staticextended} for extended version." /// 
			"Cluster-robust standard errors in parentheses." ///
			"(d) for variables aggregated at the département level.") 


* Extended table (for appendix)	
*esttab static1 dynamic1 dynamic2 dynamic3 ///
	using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/long_static_dynamic.tex" ///
	label ///
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(r2_a F N, labels("Adj. R-squared" "F-statistic" "Number of observations")) ///
	title("Regression results – Static and dynamic modeling (extended)") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("Static" "Dynamic (1st)" "Dynamic (2nd)" "Dynamic (full)" ) ///
	longtable /// 
	order(extdr_leg lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop schools avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop pop_commune pop_dept 2012.year 2017.year) /// 
	collabels(none) ///
	legend /// 
	addnote("Unit and temporal fixed effects models" /// 
			"Cluster-robust standard errors in parentheses." ///
			"(d) for variables aggregated at the département level." /// 
	***********************************************************************************************************************************************************************************************
		/// Instrumental variable estimation 

* Loading the data that includes our instruments – Setup 

drop _all

use "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/data/final_data/instrument_merge.dta"

encode code_insee, generate(depcom)
sort depcom year
tset depcom year, delta(5)

egen num_periods = total(1), by(depcom)
su num_periods, meanonly
scalar max_periods = r(max)
keep if num_periods == max_periods

gen lag_extdrleg1 = L.extdr_leg
gen lag_extdrleg2 = L2.extdr_leg	

* Add labels to our variables for future tables 
label variable depcom "Code commune"
label variable year "Year"
label variable code_insee "Code Insee"
label variable immi_prop "\% Immigrants"
label variable extdr_leg "\% Far-right votes"
label variable chomeurs_prop "\% Unemployed"
label variable dropouts "\% Dropouts"
label variable hlm_prop "\% Subsidized housing"
label variable ouvriers_prop "\% Workers"
label variable employes_prop "\% Employees"
label variable retraites_prop "\% Retirees"
label variable cadres_prop "\% Cadres"
label variable agriculteurs_prop "\% Farmers"
label variable pop_commune "Population" 
label variable schools "Number of schools"
label variable avg5_popdept_growth "5-year population growth (d)"
label variable pop_dept "Population (d)"
label variable crime_dept_prop "Crime rate (d)"
label variable avg5_naissances_dept "5-year birth rate (d)"
label variable logements_dept_prop "Dwellings construction rate (d)"
label variable trim_chomage_growth1 "Unemployment growth (s)"
label variable trim_chomage_growth2 "Unemployment growth (s-1)"
label variable trim_entreprise_prop_1 "Businesses' bankruptcy rate (s)" 
label variable trim_entreprise_prop_2 "Businesses' bankruptcy rate (s-1)" 
label variable extdr1986 "1986 Far-right electoral results"
label variable lag_extdrleg1 "1st order lagged far-right votes"
label variable lag_extdrleg2 "2nd order lagged far-right votes"

* Drop all variables that do not have a label (for simplicity, given how extensive the dataset is) **
ds
foreach var of varlist `r(varlist)' {
    if "`: variable label `var''" == "" {
        drop `var'
    }
}

est clear

xtset depcom year, delta(5)
tabulate year, gen(yd)

gen fn86_yd1 = extdr1986 * yd1
gen fn86_yd2 = extdr1986 * yd2
gen fn86_yd3 = extdr1986 * yd3
gen fn86_yd4 = extdr1986 * yd4
gen fn86_yd5 = extdr1986 * yd5

label variable fn86_yd4 "1986 FN votes \%, (2012)"
label variable fn86_yd5 "1986 FN votes \%, (2017)"
label variable yd4 "2012"
label variable yd5 "2017"

* Estimation
xtivreg2 immi_prop (extdr_leg = trim_chomage_growth2 trim_chomage_growth1) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, fe cluster(depcom) first savefirst savefprefix(st1)

***********************************************************************************************************************************************************************************************

save mydata_transformed.dta, replace
use mydata_transformed.dta

* IMPORTANT: Here, I apply the time-demeaned manipulation so I can then estimate my model using ivreg2, which does not accept fixed effects. 
* Xtivreg2 does, but cannot be followed by the robust weak instrument test weakivtest, so I apply the time-demeaning manipulation before estimating the model. 
* Naturally, there are no differences between the two approaches. 
xtset depcom year, delta(5)
xtdata, fe

eststo xi: ivreg2 immi_prop (extdr_leg = trim_chomage_growth2 trim_chomage_growth1) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

estadd scalar F_st1 = `e(F)': st1extdr_leg
estadd scalar R_st1 = `e(r2)': st1extdr_leg

* First stage table
esttab st* using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/stage1_instr1.tex", replace ///
	label /// 
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(F_st1 R_st1 N, labels("F-statistic" "R-squared" "Number of observations")) ///
	title("2SLS First-stage estimation results") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("\% Far-right votes") ///
	drop(pop_commune pop_dept) ///
	collabels(none) ///
	legend /// 
	addnote("Cluster-robust standard errors in parentheses." ///
	"Unit and temporal fixed effects." /// 
	"(d) for variables aggregated at the département level." ///
	"Population and time-dummies dropped from the table. See table \ref{tab:stage1extended} for extended version.")

* First stage - Extended table (for appendix)
esttab st* using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/stage1_instr1_long.tex", replace ///
	label /// 
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(F_st1 R_st1 N, labels("F-statistic" "R-squared" "Number of observations")) ///
	title("2SLS First-stage estimation results (extended)") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("\% Far-right votes") ///
	collabels(none) ///
	legend /// 
	longtable /// 
	addnote("Cluster-robust standard errors in parentheses." ///
			"Unit and temporal fixed effects." ///
			"(d) for variables aggregated at the département level." ///
			"\label{tab:stage1extended}")
			
* 2SLS Results
estadd scalar F_iv = `e(F)': 
estadd scalar n_clust = `e(N_clust)'
estadd scalar ar2 = `e(r2_a)'

esttab using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/iv_estimation.tex", replace ///
	label /// 
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(F_iv ar2 N n_clust, labels("F-statistic" "Adjusted R-squared" "Number of observations" "Number of clusters")) ///
	title("IV Regression Results") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("\% Immigrants") ///
	drop(pop_commune pop_dept) ///
	collabels(none) ///
	legend /// 
	addnote("Cluster-robust standard errors in parentheses." ///
	"Unit and temporal fixed effects." /// 
	"(d) for variables aggregated at the département level." ///
	"Population and time-dummies dropped from the table. See table \ref{tab:ivextended} for extended version.")

* 2SLS Results – Extended table (for appendix)
esttab using "/Users/victorkreitmann/Desktop/synced/life/academia-pro/school/4.sciences_po/work/semestre_2/serranito_advancedeconometrics/serranito_paper/serranito_paperitself/iv_estimationlong.tex", replace ///
	label /// 
	nonotes ///
	se(3) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(F_iv ar2 N n_clust, labels("F-statistic" "Adjusted R-squared" "Number of observations" "Number of clusters")) ///
	title("IV Regression Results (extended)") ///
	nonumbers ///
	booktabs ///
	nomtitles ///
	noobs ///
	mlabels("\% Immigrants") ///
	collabels(none) ///
	legend /// 
	longtable /// 
	addnote("Cluster-robust standard errors in parentheses." ///
			"Unit and temporal fixed effects." ///
			"(d) for variables aggregated at the département level." ///
			"\label{tab:ivextended}")
			
***********************************************************************************************************************************************************************************************
			/// IV Diagnostics
			
quietly ivreg2 immi_prop (extdr_leg = fn86_yd4 fn86_yd5) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest			

quietly ivreg2 immi_prop (extdr_leg = fn86_yd4 fn86_yd5 trim_chomage_growth1) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest			

quietly ivreg2 immi_prop (extdr_leg =  trim_chomage_growth1 trim_chomage_growth2) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest			

quietly ivreg2 immi_prop (extdr_leg =  trim_chomage_growth1 trim_entreprise_prop_1) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest			

quietly ivreg2 immi_prop (extdr_leg =  trim_entreprise_prop_1 trim_entreprise_prop_2) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest			

quietly ivreg2 immi_prop (extdr_leg = trim_entreprise_prop_1 fn86_yd4 fn86_yd5) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest

quietly ivreg2 immi_prop (extdr_leg = trim_entreprise_prop_2 fn86_yd4 fn86_yd5) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest

quietly ivreg2 immi_prop (extdr_leg = trim_chomage_growth2 trim_entreprise_prop_1) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest

quietly ivreg2 immi_prop (extdr_leg = trim_chomage_growth1 trim_entreprise_prop_2) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest

quietly ivreg2 immi_prop (extdr_leg = trim_chomage_growth2 trim_entreprise_prop_2) lag_extdrleg1 lag_extdrleg2 cadres_prop dropouts_prop employes_prop agriculteurs_prop retraites_prop hlm_prop chomeurs_prop ouvriers_prop avg5_naissances_dept avg5_popdept_growth crime_dept_prop logements_dept_prop schools pop_commune pop_dept yd4 yd5 if yd3 == 1 | yd4 == 1 | yd5 == 1, partial(yd4 yd5) cluster(depcom) first savefirst savefprefix(st1)

overid
weakivtest
