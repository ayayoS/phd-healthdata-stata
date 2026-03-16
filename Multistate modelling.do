*****************************************************************************************
* PhD title: Clinical outcomes following PCI and AMI in complex high risk populations
* Purpose: Multistate modelling taking into account the order of bleeding and reinfarction
* Date created: 30/05/2025
* Created by: Sharon Ayayo
*****************************************************************************************

cap ado uninstall merlin

cap ado uninstall multistate

net install merlin, from("https://raw.githubusercontent.com/RedDoorAnalytics/merlin/main/")

net install multistate, from("https://raw.githubusercontent.com/RedDoorAnalytics/multistate/main/")


cd "C:\Users\m84006sa\The University of Manchester Dropbox\Sharon Ayayo\PhD Sharon\Multistate modelling\Data"

set seed 511618
use multistate_1imp,clear

gen pseudoID = _n


*For those with death and bleeding or reinfarction on the same day, add a small constant to death to make it occur last

replace time_death = time_death+0.0001 if time_bleeding == time_death & bleeding == 1 & death == 1 // 1432
replace time_death = time_death+0.0001 if time_reinfarction == time_death & reinfarction == 1 & death == 1 // 782
drop if time_bleeding == time_reinfarction & bleeding == 1 & reinfarction ==1 // 3125 patients

/*
States
1.	Admission (no events) 
2.	Bleeding 
3.	Reinfarction 
4.	Bleeding before reinfarction
5.	Reinfarction before bleeding
6.	Death
*/

*Create additional states 4 and 5 above and their corresponding times

*Bleeding before reinfarction status
gen bleed_reinf = 0
replace bleed_reinf = 1 if bleeding == 1 & reinfarction == 1 & (time_bleeding < time_reinfarction)

*Bleeding before reinfarction time
gen bleed_reinf_time = time_reinfarction if bleed_reinf == 1
replace bleed_reinf_time = followup_time if bleed_reinf == 0

*Reinfarction before bleeding status
gen reinf_bleed = 0
replace reinf_bleed = 1 if bleeding == 1 & reinfarction == 1 & (time_reinfarction < time_bleeding)

*Reinfarction before bleeding time
gen reinf_bleed_time = time_bleeding if reinf_bleed == 1
replace reinf_bleed_time = followup_time if reinf_bleed == 0


*Define transition matrix

matrix tmat = (., 1, 2, ., ., 3 \ ///
               ., ., ., 4, ., 5 \ ///
               ., ., ., ., 6, 7 \ ///
               ., ., ., ., ., 8 \ ///
               ., ., ., ., ., 9 \ ///
               ., ., ., ., ., .)

matrix rownames tmat = Admission Bleeding Reinfarction BleedReinf ReinfBleed Death
matrix colnames tmat = Admission Bleeding Reinfarction BleedReinf ReinfBleed Death

matrix list tmat

* Convert data to long format for multistate modelling
msset, id(pseudoID) states(bleeding reinfarction bleed_reinf reinf_bleed death) times(time_bleeding time_reinfarction bleed_reinf_time reinf_bleed_time time_death) transmat(tmat)


*declare the data to survival analysis data using clock reset approach

gen duration = _stop - _start

stset duration, failure(_status=1) scale(365.25)

*********************************************************************************
*model fiting for each transition 
*********************************************************************************

*Create dummy variables for categorical variables since merlin doesn't support i. prefix
xi i.sex i.ethnicity i.prevAMI i.hyperten i.hypercholes i.peripheral i.cerebrovasc i.renal i.smoking i.diabetic i.prevPCI i.prevCABG i.prevAngina i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.asthma_copd i.pci i.cabg i.heartfailure

* Fit transition-specific Weibull models

forvalues i = 1/9{
	merlin (_t age heartrate systolic _Isex_2 _Iethnicity_2 _Iethnicity_3 _Iethnicity_4 _Iethnicity_5 _IprevAMI_1 _Ihyperten_1 _Ihyperchol_1 _Iperiphera_1 _Icerebrova_1 _Irenal_1 _Ismoking_1 _Ismoking_2 _Ismoking_3  _Idiabetic_1 _IprevPCI_1 _IprevCABG_1 _IprevAngin_1 _Ifamhist_c_1 _Ip2y12_1 _Iasp_1 _Iwarf_1 _Istatin_1 _Iglycoprot_1 _Iasthma_co_1 _Ipci_1 _Icabg_1 _Iheartfail_1, family(weibull, failure(_d) ltruncated(_t0))) if _trans`i' == 1
	estimates store m`i'
}


*Transition Hazards at 5 timepoints (1-5 years)
*First create the time variable to make predictions at
gen tempvar = .
replace tempvar = _n if _n <= 5


****Predicted hazards rates for different covariate patterns for "average" patients

* Median ages: white males = 67.8, black males = 62.0 , white females = 76.7, black females = 71.1

*White, male, with diabetes
predictms, transmat(tmat) at1(age 67.8 _Isex_2 0 _Iethnicity_2 0 _Iethnicity_3 0 _Iethnicity_4 0 _Iethnicity_5 0 _Idiabetic_1 1) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

gen cov_pattern = "WMD67"

compress

save hazard_WMD67, replace

*White, male, without diabetes
predictms, transmat(tmat) at1(age 67.8 _Isex_2 0 _Iethnicity_2 0 _Iethnicity_3 0 _Iethnicity_4 0 _Iethnicity_5 0 _Idiabetic_1 0) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "WMDn67"

compress

save hazard_WMnD67, replace

*Black, male, with diabetes
predictms, transmat(tmat) at1(age 62 _Isex_2 0 _Iethnicity_2 1 _Idiabetic_1 1) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "BMD62"

compress

save hazard_BMD62, replace


*Black, male, without diabetes
predictms, transmat(tmat) at1(age 62 _Isex_2 0 _Iethnicity_2 1 _Idiabetic_1 0) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "BMnD62"

compress

save hazard_BMnD62, replace

*White, female,  with diabetes
predictms, transmat(tmat) at1(age 76.7 _Isex_2 1 _Iethnicity_2 0 _Iethnicity_3 0 _Iethnicity_4 0 _Iethnicity_5 0 _Idiabetic_1 1) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "WFD76"

compress

save hazard_WFD76, replace

*White, female, without diabetes
predictms, transmat(tmat) at1(age 76.7 _Isex_2 1 _Iethnicity_2 0 _Iethnicity_3 0 _Iethnicity_4 0 _Iethnicity_5 0 _Idiabetic_1 0) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "WFnD76"

compress

save hazard_WFnD76, replace

*Black, female, with diabetes
predictms, transmat(tmat) at1(age 71.1 _Isex_2 1 _Iethnicity_2 1 _Idiabetic_1 1) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "BFD71"

compress

save hazard_BFD71, replace


*Black, female, without diabetes
predictms, transmat(tmat) at1(age 71.1 _Isex_2 1 _Iethnicity_2 1 _Idiabetic_1 0) hazard models(m1 m2 m3 m4 m5 m6 m7 m8 m9) from(1 2 3 4 5) timevar(tempvar) ci reset

drop cov_pattern

gen cov_pattern = "BFnD71"

compress

save hazard_BFnD71, replace

* Some cleaning of the estimates

local datasets WMD WMnD BMD BMnD WFD WFnD BFD BFnD

foreach data of local datasets{
	foreach age in 50 60 70 80{
		
		use hazard_`data'`age', clear
	
		keep tempvar _hazard* cov_pattern

		drop if missing(tempvar)


		rename (_hazard_at1_1_2 _hazard_at1_1_3 _hazard_at1_1_6)  (adm_bleed_hazard adm_reinf_hazard adm_death_hazard) 

		rename (_hazard_at1_1_2_lci _hazard_at1_1_3_lci _hazard_at1_1_6_lci)  (adm_bleed_hazard_lci adm_reinf_hazard_lci adm_death_hazard_lci) 

		rename (_hazard_at1_1_2_uci _hazard_at1_1_3_uci _hazard_at1_1_6_uci)  (adm_bleed_hazard_uci adm_reinf_hazard_uci adm_death_hazard_uci) 


		rename (_hazard_at1_2_4 _hazard_at1_2_6)  (bleed_reinf_hazard bleed_death_hazard) 

		rename (_hazard_at1_2_4_lci _hazard_at1_2_6_lci)  (bleed_reinf_hazard_lci bleed_death_hazard_lci) 

		rename (_hazard_at1_2_4_uci _hazard_at1_2_6_uci)  (bleed_reinf_hazard_uci bleed_death_hazard_uci)  


		rename (_hazard_at1_3_5 _hazard_at1_3_6)  (reinf_bleed_hazard reinf_death_hazard) 

		rename (_hazard_at1_3_5_lci _hazard_at1_3_6_lci)  (reinf_bleed_hazard_lci reinf_death_hazard_lci) 

		rename (_hazard_at1_3_5_uci _hazard_at1_3_6_uci)  (reinf_bleed_hazard_uci reinf_death_hazard_uci)  


		rename (_hazard_at1_4_6)  (bleedreinf_death_hazard) 
		rename (_hazard_at1_4_6_lci)  (bleedreinf_death_hazard_lci) 
		rename (_hazard_at1_4_6_uci)  (bleedreinf_death_hazard_uci) 


		rename (_hazard_at1_5_6)  (reinfbleed_death_hazard) 
		rename (_hazard_at1_5_6_lci)  (reinfbleed_death_hazard_lci) 
		rename (_hazard_at1_5_6_uci)  (reinfbleed_death_hazard_uci) 

		local varlist adm_bleed_hazard adm_reinf_hazard adm_death_hazard adm_bleed_hazard_lci adm_reinf_hazard_lci adm_death_hazard_lci adm_bleed_hazard_uci adm_reinf_hazard_uci 	adm_death_hazard_uci bleed_reinf_hazard bleed_death_hazard bleed_reinf_hazard_lci bleed_death_hazard_lci bleed_reinf_hazard_uci bleed_death_hazard_uci reinf_bleed_hazard reinf_death_hazard reinf_bleed_hazard_lci reinf_death_hazard_lci reinf_bleed_hazard_uci reinf_death_hazard_uci bleedreinf_death_hazard bleedreinf_death_hazard_lci bleedreinf_death_hazard_uci reinfbleed_death_hazard reinfbleed_death_hazard_lci reinfbleed_death_hazard_uci

foreach var of local varlist{
	replace `var' = round(`var' * 100,0.1)
}
save hazard_`data'`age', replace
}
	}
	
* Append all datasets 

clear

local datasets WMD WMnD BMD BMnD WFD WFnD BFD BFnD

foreach data of local datasets{
	foreach age in 50 60 70 80{
		
		append using hazard_`data'`age'
	}
}

save hazard_all, replace

use hazard_all, clear

* Sort according to the age for the heatmap
gen age = .
foreach age in 50 60 70 80{
	replace age = `age' if strpos(cov_pattern, "`age'") > 0
}

gen cov_pattern_num = 1 if strpos(cov_pattern, "WMD") > 0
replace cov_pattern_num = 2 if strpos(cov_pattern, "WMnD") > 0
replace cov_pattern_num = 3 if strpos(cov_pattern, "BMD") > 0
replace cov_pattern_num = 4 if strpos(cov_pattern, "BMnD") > 0
replace cov_pattern_num = 5 if strpos(cov_pattern, "WFD") > 0
replace cov_pattern_num = 6 if strpos(cov_pattern, "WFnD") > 0
replace cov_pattern_num = 7 if strpos(cov_pattern, "BFD") > 0
replace cov_pattern_num = 8 if strpos(cov_pattern, "BFnD") > 0

sort tempvar age cov_pattern_num

keep if tempvar == 5

*Then export the data to excel for the heatmap
export excel using "C:\Users\m84006sa\The University of Manchester Dropbox\Sharon Ayayo\PhD Sharon\Multistate modelling\Output\Covariate patterns year 5.xls", firstrow(variables)





