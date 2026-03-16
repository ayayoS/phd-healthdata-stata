*****************************************************************************************
* PhD title: Clinical outcomes following PCI and AMI in complex high risk populations
* Purpose: Fit parametric models and extrapolate to the next 20 years
* Date created: 26/11/2025
* Created by: Sharon Ayayo
*****************************************************************************************

cd "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Data"

use analysis_imputed, clear

gen year_from_start = adm_year - 2005

************************************************
* Primary outcome - All-cause mortality
************************************************

mi stset time_death, failure(death_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(3) failconvlininit

* Create permanent results file
postfile death double year double t_eval double hazard double lb double ub using "future_death.dta", replace

forvalues y = 2005/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post death (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose death

* Convert to percentage

use "future_death.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for all-cause mortality") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
 
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_death.png", replace width(1200) height(800)
 
************************************
* Secondary outcomes
************************************
*-------------------------------------
* 1. Cardiovascular death
*--------------------------------------

mi stset time_cdeath, failure(cdeath_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(3) failconvlininit

* Create permanent results file
postfile cdeath double year double t_eval double hazard double lb double ub using "future_cdeath.dta", replace

forvalues y = 2005/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post cdeath (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose cdeath

* Convert to percentage

use "future_cdeath.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for cardiac mortality") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_cdeath.png", replace width(1200) height(800)

*-------------------------------------
* 2. Bleeding 
*--------------------------------------

mi stset time_bleeding, failure(bleeding_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(3) failconvlininit

* Create permanent results file
postfile bleeding double year double t_eval double hazard double lb double ub using "future_bleeding.dta", replace

forvalues y = 2005/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post bleeding (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose bleeding

* Convert to percentage

use "future_bleeding.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for major bleeding") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_bleeding.png", replace width(1200) height(800)

*-------------------------------------
* 3. Stroke 
*--------------------------------------

mi stset time_stroke, failure(stroke_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(2) failconvlininit

* Create permanent results file
postfile stroke double year double t_eval double hazard double lb double ub using "future_stroke.dta", replace

forvalues y = 2019/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post stroke (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose stroke

* Convert to percentage

use "future_stroke.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for stroke") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_stroke.png", replace width(1200) height(800)

*-------------------------------------
* 4. Heart Failure
*--------------------------------------

mi stset time_hf, failure(hf_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(2) failconvlininit

* Create permanent results file
postfile hf double year double t_eval double hazard double lb double ub using "future_hf.dta", replace

forvalues y = 2019/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post hf (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose hf

* Convert to percentage

use "future_hf.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for heart failure") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
     
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_hf.png", replace width(1200) height(800)

*-------------------------------------
* 4. Reinfarction
*--------------------------------------

mi stset time_reinfarction, failure(reinfarction_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(2) failconvlininit

* Create permanent results file
postfile reinfarction double year double t_eval double hazard double lb double ub using "future_reinfarction.dta", replace

forvalues y = 2019/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') 

        matrix M = r(table)
        post reinfarction (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose reinfarction

* Convert to percentage

use "future_reinfarction.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v' * 100
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for reinfarction") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_reinfarction.png", replace width(1200) height(800)
 
*-------------------------------------
* 5. MACCE
*--------------------------------------

mi stset time_macce, failure(macce_indic) id(id)  

* Fit stpm2 model
mi estimate, post cmdok: ///
    stpm2 year_from_start age i.sex i.ethnic_bin i.hyperten i.hypercholes i.peripheral ///
         i.cerebrovasc i.renal i.smoke i.diabetic i.prevPCI i.prevCABG i.prevAMI i.prevAngina ///
         i.famhist_chd i.p2y12 i.asp i.warf i.statin i.glycoprotein i.acei i.beta_blocker ///
         i.diag i.asthma_copd i.pci i.cabg heartrate systolic i.heartfailure, ///
         scale(hazard) df(2) failconvlininit

* Create permanent results file
postfile macce double year double t_eval double hazard double lb double ub using "future_macce.dta", replace

forvalues y = 2019/2038{
    foreach t in 365 1826 {
        local yrsince = `y' - 2005   // compute numeric value first
	capture drop time_eval
	gen time_eval = `t'
        quietly mimrgns, predict(hazard timevar(time_eval)) ///
            at(year_from_start=`yrsince') asbalanced

        matrix M = r(table)
        post macce (`y') (`t') (M[1,1]) (M[5,1]) (M[6,1])
    }
}

postclose macce

* Convert to percentage

use "future_macce.dta", clear

local vars hazard lb ub

foreach v of local vars{
	replace `v' = `v'*1000
}

*  Plot with 95% CIs

twoway                                                     ///
 (rarea lb ub year if t_eval==365,               ///
        color(navy%15) lcolor(navy%15))                       ///
 (line  hazard year if t_eval==365,               ///
        lcolor(navy) lwidth(medthick))                     ///
 (rarea lb ub year if t_eval==1826,              ///
        color(maroon%15) lcolor(maroon%15))                     ///
 (line  hazard year if t_eval==1826,              ///
        lcolor(maroon) lwidth(medthick)),                  ///
 xtitle("AMI admission year")                                  ///
 ytitle("Hazard rate (%)")                  ///
 title("Predicted hazard rates for MACCE") ///
 legend(order(1 "95% CI" 2 "1-year hazard" 3 "95% CI" 4 "5-year hazard") pos(1) ring(0)) ///
 xline(2018, lcolor(black) lpattern(dash)) ///
 xlabel(2005 2018 2010(5)2040, angle(45)) ///
 ylabel(,format(%4.3f))
    
* Export the graph as PNG
graph export "/mnt/bmh01-rds/Kontopantelis_Cardio/Sharon/Fourth Study - MINAP/STATA/Output/future_macce.png", replace width(1200) height(800)


 
	 
































