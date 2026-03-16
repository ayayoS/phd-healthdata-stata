*****************************************************************************************
* PhD title: Clinical outcomes following PCI and AMI in complex high risk populations
* Purpose: Multiple imputation for Flexible parametric modelling
* Date created: 13/09/2025
* Created by: Sharon Ayayo
*****************************************************************************************
*Change the working directory
cd "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Data"

use analysis_data, clear

*****************************************************
*Obtain the NA estimators for each outcome separately
*****************************************************
*Replace the times that were 0 to 0.0001 so that the na can be estimated - they resulted into missing initially in the 3rd study

local names stroke hf reinfarction bleeding death cdeath macce stroke_1yr hf_1yr reinf_1yr bleeding_1yr death_1yr cdeath_1yr macce_1yr

foreach name of local names{
	replace time_`name' = 0.0001 if time_`name'== 0
	
	stset time_`name', failure(`name'=1)

	sts gen na = na 

	rename (_d na) (`name'_indic na_`name')
}

save na_outcomes, replace

use na_outcomes, clear

recode ethnic_bin (1=0) (2=1)
lab define eth 0 "Whites" 1 "Non-whites"
label values ethnic_bin eth

// then  use _d = Di and na = H(Ti) in the imputation model


* set the data for mi - wide|mlong|flong|flongsep
mi set flong

* register variables to be imputed 
mi register imputed ethnic_bin hyperten hypercholes peripheral cerebrovasc renal smoke diabetic prevPCI prevCABG prevAMI prevAngina famhist_chd asthma_copd heartfailure heartrate systolic p2y12 asp warf statin glycoprotein beta_blocker acei pci cabg 

* register variables with complete data 

mi register regular age sex adm_year diag death_indic na_death stroke_indic na_stroke hf_indic na_hf reinfarction_indic na_reinfarction bleeding_indic na_bleeding cdeath_indic na_cdeath macce_indic na_macce death_1yr_indic na_death_1yr stroke_1yr_indic na_stroke_1yr hf_1yr_indic na_hf_1yr reinf_1yr_indic na_reinf_1yr bleeding_1yr_indic na_bleeding_1yr cdeath_1yr_indic na_cdeath_1yr macce_1yr_indic na_macce_1yr

*run the imputation model

mi stset, clear // Got this error: Your mi data are stset and some of the variables previously declared by stset are not in the dataset.  mi verifies that none of the stset variables are also registered as imputed or passive.  Type mi stset, clear to clear old no-longer-valid settings.


* Run 10 imputations
mi impute chained (pmm, knn(5)) heartrate systolic (logit) hyperten hypercholes peripheral cerebrovasc renal diabetic prevPCI prevCABG prevAMI prevAngina famhist_chd p2y12 asp warf statin glycoprotein beta_blocker acei asthma_copd pci cabg heartfailure ethnic_bin smoke = age sex adm_year diag death_indic na_death stroke_indic na_stroke hf_indic na_hf reinfarction_indic na_reinfarction bleeding_indic na_bleeding  cdeath_indic na_cdeath macce_indic na_macce death_1yr_indic na_death_1yr stroke_1yr_indic na_stroke_1yr hf_1yr_indic na_hf_1yr reinf_1yr_indic na_reinf_1yr bleeding_1yr_indic na_bleeding_1yr cdeath_1yr_indic na_cdeath_1yr macce_1yr_indic na_macce_1yr ///
, add(10) rseed(5116) burnin(10) force augment


save analysis_imputed, replace


***********************************
**Diagnostics
**********************************
use analysis_imputed, clear

mi extract 0

gen byte mis_systolic = missing(systolic)
gen byte mis_heartrate = missing(heartrate)

*use observed, clear
regress systolic age i.sex i.ethnic_bin i.hypercholes i.hyperten i.peripheral i.cerebrovasc i.renal i.diabetic i.prevAMI i.prevCABG i.prevPCI i.prevAngina i.famhist_chd i.asthma_copd i.heartfailure i.smoke i.p2y12 i.asp i.warf i.glycoprotein i.statin i.acei i.beta_blocker i.pci i.cabg i.diag heartrate adm_year

save observed, replace

use analysis_imputed, clear

forvalues i = 1/10{
	use analysis_imputed, clear

	mi extract `i'

	merge 1:1 nhs_no_pseudonymised using observed, keepusing(mis_systolic)
	drop _merge

	predict xb`i', xb

	twoway (scatter systolic xb`i' if mis_systolic==0, msymbol(oh)) ///
	(scatter systolic xb`i' if mis_systolic==1) ///
	(lowess systolic xb`i' if mis_systolic==0) ///
	(lowess systolic xb`i' if mis_systolic==1), ///
	ytitle(systolic blood pressure) ///
	title(Imputation `i') ///
	legend(label(1 "Observed") label(2 "Imputed") ///
	label(3 "Lowess (observed)") label(4 "Lowess (imputed)")) ///
	name(systolic`i')
}

graph combine systolic1 systolic2 systolic3 systolic4 systolic5 systolic6 systolic7 systolic8 systolic9 systolic10, name(systolic_all)

graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/systolic_lowess", as(png) name("systolic_all")

use observed, clear

regress heartrate age i.sex i.ethnic_bin i.hypercholes i.hyperten i.peripheral i.cerebrovasc i.renal i.diabetic i.prevAMI i.prevCABG i.prevPCI i.prevAngina i.famhist_chd i.asthma_copd i.heartfailure i.smoke i.p2y12 i.asp i.warf i.glycoprotein i.statin i.acei i.beta_blocker i.pci i.cabg i.diag systolic adm_year


use analysis_imputed, clear

forvalues i = 1/10{
	use analysis_imputed, clear
	
	mi extract `i'

	merge 1:1 nhs_no_pseudonymised using observed, keepusing(mis_heartrate)
	drop _merge

	predict xb`i', xb

	twoway (scatter heartrate xb`i' if mis_heartrate==0, msymbol(oh)) ///
	(scatter heartrate xb`i' if mis_heartrate==1) ///
	(lowess heartrate xb`i' if mis_heartrate==0) ///
	(lowess heartrate xb`i' if mis_heartrate==1), ///
	ytitle(heartrate) ///
	title(Imputation `i') ///
	legend(label(1 "Observed") label(2 "Imputed") ///
	label(3 "Lowess (observed)") label(4 "Lowess (imputed)")) ///
	name(heartrate`i')

}

graph combine heartrate1 heartrate2 heartrate3 heartrate4 heartrate5 heartrate6 heartrate7 heartrate8 heartrate9 heartrate10, name(heartrate_all)

*graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/heartrate diagnostics eps", as(eps) name(heartrate_all) // EPS doesn't open!

graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/heartrate lowess.jpg", as(jpg) name(heartrate_all) quality(90)

****Using the midiagplots command
search midiagplots // to install

help midiagplots

use analysis_imputed, clear

mi describe

midiagplots systolic ethnicity hyperten, plottype(kdensity, kernel(epan2)) m(1/5) combine


log using "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/MINAP/Log/imputation diagnostics",replace

midiagplots, plottype(kdensity, kernel(epan2)) m(1/10) combine sample(observed imputed)

log close

*The plot for heartrate was overwritten by for systolic
midiagplots heartrate, plottype(kdensity, kernel(epan2)) m(1/10) combine sample(observed imputed)

graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/heartrate density.jpg", as(jpg) name(Graph) quality(90)

*The editor wants eps, so run again to produce eps files - 10/06/2025
midiagplots systolic, plottype(kdensity, kernel(epan2)) m(1/10) combine sample(observed imputed)

graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/systolic density eps", as(eps) name(Graph)


*forgot to include the p-values for categorical

log using "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Log/imputation diagnostics",replace

midiagplots ethnic_bin prevAMI hyperten hypercholes peripheral cerebrovasc renal smoke diabetic prevPCI prevCABG prevAngina famhist_chd p2y12 asp warf statin glycoprotein acei beta_blocker asthma_copd pci cabg heartfailure, m(1/10) ksmirnov // only shows K-S for continuous variables

log close 





