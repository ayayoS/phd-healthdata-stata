***************************************************************************************************************************
* PhD title: Clinical outcomes following PCI and AMI in complex high risk populations
* Purpose: To perform the Fairlie decomposition on all the 10 imputed datasets
* Date created: 13/03/2023
* Created by: Sharon Ayayo
****************************************************************************************************************************
* Change working directory
cd "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/STATA/Data"


*import regions data
import excel "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/STATA/Data/hospitals_regions.xls", sheet("Sheet1") firstrow clear
replace region = trim(region) 
save regions,replace

*Load the analysis data
use analysis_10imputations, clear

*Remove private practices 

drop if strpos(hospital, "KES") > 0 | strpos(hospital, "KIM") > 0 | strpos(hospital, "HHW") > 0 | strpos(hospital, "LBH") > 0 | strpos(hospital, "LIS") > 0 | strpos(hospital, "PHB") > 0 

*some hospital codes are HH while others are HH, fix that
replace hospital = "HH" if hospital == "HH."

*merge analysis data with regions data
merge m:1 hospital using regions 

drop _merge

encode region, gen(regions)

save analysis_regions_10imp,replace

use analysis_regions_10imp,clear

*Fit the models
*****Fit the model

log using "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/STATA/Log/fairlie_10imps_main"

keep if year == 2006 | year == 2021

gen group = (year==2006)

label define group_ 0 "2021" 1 "2006"
label values group group_

* 2006 as reference

set seed 1234

xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity i.smoke) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity:i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) ref(1)


xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci i.smoke) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity:i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) ref(1)

* 2021 as reference

set seed 1234

xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity i.smoke) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity:i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) ref(0)

* pooled as reference

set seed 1234

xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity i.smoke) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity:i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) pooled

*------------------------------------------------------------------------------------------------------------------------------------------------------
*Ungrouped analysis

*2006 as reference

set seed 1234

xi:mi estimate, cmdok:fairlie dead age i.sex i.ethnicity i.smoke i.indication i.hyperchole i.hypertension i.pvd i.cva i.vhd i.renal i.diabetes i.previousmi i.previouscabg i.previouspci i.lvef i.access_site i.graft i.lmain i.multivessel i.inotropes i.iabp_ecmo_impella i.antiplatelet i.glyco i.ventilation i.cardio_shock i.cardiac_arrest i.device i.rotational i.regions, by(group) ro reps(1000) ref(1)


*2021 as reference - ungrouped
set seed 1234

xi:mi estimate, cmdok:fairlie dead age i.sex i.ethnicity i.smoke i.indication i.hyperchole i.hypertension i.pvd i.cva i.vhd i.renal i.diabetes i.previousmi i.previouscabg i.previouspci i.lvef i.access_site i.graft i.lmain i.multivessel i.inotropes i.iabp_ecmo_impella i.antiplatelet i.glyco i.ventilation i.cardio_shock i.cardiac_arrest i.device i.rotational i.regions, by(group) ro reps(1000) ref(0)

*pooled as reference - ungrouped

set seed 1234

xi:mi estimate, cmdok:fairlie dead age i.sex i.ethnicity i.smoke i.indication i.hyperchole i.hypertension i.pvd i.cva i.vhd i.renal i.diabetes i.previousmi i.previouscabg i.previouspci i.lvef i.access_site i.graft i.lmain i.multivessel i.inotropes i.iabp_ecmo_impella i.antiplatelet i.glyco i.ventilation i.cardio_shock i.cardiac_arrest i.device i.rotational i.regions, by(group) ro reps(1000) pooled



**************************************************************
****Contributors of mortality change pre and post 2012
**************************************************************
log using "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/STATA/Log/fairlie_10imps_main", append

use analysis_regions_10imp,clear

*2006 to 2012
preserve
keep if year == 2006 | year == 2012

*generate group variable
gen group = (year==2006)

label define group_ 0 "2012" 1 "2006"
label values group group_

*fit the model
set seed 1234

xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity i.smoke) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity:i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) ref(1) 

restore
********2012 to 2021**************
*use analysis_regions_1imp, clear

preserve
*2006 to 2012
keep if year == 2012 | year == 2021

*generate group variable
gen group = (year==2012)

label define group_ 0 "2021" 1 "2012"
label values group group_

*fit the model
set seed 1234

xi:mi estimate, cmdok:fairlie dead (demographics:age i.sex i.ethnicity i.smoke) (medical_history:i.hyperchole i.hypertension i.pvd i.cva i.renal i.diabetes i.previousmi i.previouscabg i.previouspci) (structural_cardiac:i.vhd i.lvef) (procedure:i.graft i.lmain i.multivessel) (pharmacology:i.antiplatelet i.glyco) (severity: i.inotropes i.iabp_ecmo_impella i.ventilation i.cardio_shock i.cardiac_arrest) (devices:i.device i.rotational) (indication: i.indication) (site:i.access_site) (region:i.regions), by(group) ro reps(1000) ref(1) 

restore

log close








