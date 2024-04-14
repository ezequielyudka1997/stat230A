*****************************************************************************************
* Intergenerational Economic Mobility and the Racial Wealth Gap                         *
* by Jermaine Toney (Rutgers University) and Cassandra L. Robertson (New America)       *  
* AEA P&P Code Replication Files                                                        *  
* January 2021                                                                          * 
*****************************************************************************************

*****************************************************************************************
*two-stage least squares (2SLS/IV) for panel data                                       *
*parent-child estimates  (Table 1, Column (1))                                          *  
*****************************************************************************************
//working directory
clear 
clear matrix
*cd "change working directory"
use "aeapp_mobility_table1_and_summarystatistics.dta"

//parental wealth
global xlist isib_age sib_agesq isib_race 
global x2list ipar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref i.year sib_sex 
global ylist isib_famincome

xtivreg $ylist $xlist (isib_stocks=$x2list), first re ec2sls vce(cluster sib_id)
outreg2 using parent2SLS, replace excel dec(4) 

*****************************************************************************************
*first stage results                                                                    *
*parent-child estimates  (Table 2, Column (1))                                          *  
*****************************************************************************************
//first stage results
reg isib_stocks isib_age sib_agesq isib_race ipar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref sib_sex year2003 year2005 year2007 year2009 year2011 year2013 year2015 year2017 if e(sample)==1 
outreg2 using myparentfile, replace excel dec(4) 

******************************************************************************************
* household income at life event for adult children                                      * 
* parent-child estimates (summary statistics)                                            *
******************************************************************************************
***household income when adult children have their firstborn child
tabstat firstchild if sample==1 & firstchild!=0, stat(mean semean median min max n)

//black sample
tabstat firstchild if sample==1 & isib_race==1 & firstchild!=0, stat(mean semean median min max n)

//white sample
tabstat firstchild if sample==1 & isib_race==0 & firstchild!=0, stat(mean semean median min max n)

***household income when adult children complete college
tabstat firstcollege if sample==1 & firstcollege!=0, stat(mean semean median min max n)

//black sample
tabstat firstcollege if sample==1 & isib_race==1 & firstcollege!=0, stat(mean semean median min max n)

//white sample
tabstat firstcollege if sample==1 & isib_race==0 & firstcollege!=0, stat(mean semean median min max n)

***household income when adult children enter their first marriage 
tabstat firstmarriage if sample==1 & firstmarriage!=0, stat(mean semean median min max n)

//black sample
tabstat firstmarriage if sample==1 & isib_race==1 & firstmarriage!=0, stat(mean semean median min max n)

//white sample
tabstat firstmarriage if sample==1 & isib_race==0 & firstmarriage!=0, stat(mean semean median min max n)

***household income when adult children turn forty 
tabstat firstforty if sample==1 & firstforty!=0, stat(mean semean median min max n)

//black sample
tabstat firstforty if sample==1 &  isib_race==1 & firstforty!=0, stat(mean semean median min max n)

//white sample
tabstat firstforty if sample==1 & isib_race==0 & firstforty!=0, stat(mean semean median min max n)

***household income when adult children become first time homeowner 
tabstat firsthome if sample==1 & firsthome!=0, stat(mean semean median min max n)

//black sample
tabstat firsthome if sample==1 & isib_race==1 & firsthome!=0, stat(mean semean median min max n)

//white sample
tabstat firsthome if sample==1 & isib_race==0 & firsthome!=0, stat(mean semean median min max n)

***household income when adult children form their own household 
tabstat ownhousehold if sample==1 & ownhousehold!=0, stat(mean semean median min max n)

//black sample
tabstat ownhousehold if sample==1 & isib_race==1 & ownhousehold!=0, stat(mean semean median min max n)

//white sample
tabstat ownhousehold if sample==1 & isib_race==0 & ownhousehold!=0, stat(mean semean median min max n)

******************************************************************************************
* parental transfers and income prospects                                                * 
* parent-child estimates (summary statistics)                                            *
******************************************************************************************
***household income when adult children received post-secondary support from parents
///received money
tabstat schoolmoney if sample==1, stat(mean semean median min max n)

//black sample
tabstat schoolmoney if sample==1 & isib_race==1, stat(mean semean median min max n)

//white sample
tabstat schoolmoney if sample==1 & isib_race==0, stat(mean semean median min max n)

///did not receive money
tabstat noschoolmoney if sample==1 & noschoolmoney!=0 , stat(mean semean median min max n)

//black sample
tabstat noschoolmoney if sample==1 & isib_race==1 & noschoolmoney!=0 , stat(mean semean median min max n)

//white sample
tabstat noschoolmoney if sample==1 & isib_race==0 & noschoolmoney!=0, stat(mean semean median min max n)

***household income when adult children received house purchase support from parents 
///received support
tabstat housemoney if sample==1,  stat(mean semean median min max n)

//black sample
tabstat housemoney if sample==1 & isib_race==1,  stat(mean semean median min max n)

//white sample
tabstat housemoney if sample==1 & isib_race==0,  stat(mean semean median min max n)

///did not received money
tabstat nohousemoney if sample==1 & nohousemoney!=0, stat(mean semean median min max n)

//black sample
tabstat nohousemoney if sample==1 & isib_race==1 & nohousemoney!=0, stat(mean semean median min max n)

//white sample
tabstat nohousemoney if sample==1 & isib_race==0 & nohousemoney!=0, stat(mean semean median min max n)

***************************************************************************************** 
* oaxaca-blinder decomposition                                                          *
* parent-child estimates (Table 3, Panel A)                                             *  
*****************************************************************************************
keep if year==2017 

*IHS applied to household income ;
sort isib_race //non-Latino Black=1, non-Latino White=0
sum isib_famincome if isib_race==0  [aw=sib_famweight], detail
gen f90=r(p90)
gen f50=r(p50)
gen f10=r(p10)
sum isib_famincome if isib_race==1  [aw=sib_famweight], detail
gen m90=r(p90)
gen m50=r(p50)
gen m10=r(p10)
gen dif90=m90-f90
gen dif50=m50-f50
gen dif10=m10-f10
di "  m-f q10 =" dif10 "  m-f q50 =" dif50 "  m-f q90 ="  dif90

**perform the RIF decomposition
*** graph the densities to check bandwidth ;
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], gen(evalw1 densw1) width(0.10) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], gen(evalb1 densb1) width(0.10)  
*
set scheme s1color
label var evalb "IHS Income"
label var evalw "IHS Income"
graph twoway  (connected densb1 evalb1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )  /*
   */   (connected densw1  evalw1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  /*
   */   , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) /*
   */   xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  /* 
   */   legend(pos(7) col(2) lab(1 "Black")  lab(2 "White")    /*
   */   region(lstyle(none)) symxsize(8) keygap(1) textwidth(34) ) /*
   */   saving(ExtFamIncMobility,replace)
   
* compute RIF for the 10th, 50th and 90th quantiles for blacks and whites ;
forvalues qt = 10(40)90 {	
   gen rif_`qt'=.
}
pctile eval1=isib_famincome if isib_race==0  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], at(eval1) gen(evalw densw) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalw[`qt']+`qc'/densw[`qt'] if isib_famincome>=evalw[`qt'] & isib_race==0
 replace rif_`qt'=evalw[`qt']-(1-`qc')/densw[`qt'] if isib_famincome<evalw[`qt'] & isib_race==0
}
pctile eval2=isib_famincome if isib_race==1  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], at(eval2) gen(evalb densb) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalb[`qt']+`qc'/densb[`qt'] if isib_famincome>=evalb[`qt'] & isib_race==1
 replace rif_`qt'=evalb[`qt']-(1-`qc')/densb[`qt'] if isib_famincome<evalb[`qt'] & isib_race==1
}
sort isib_race
by isib_race: sum rif_10 rif_50 rif_90  [aw=sib_famweight]
** perform oaxaca decomposition;
gen bwrace=.
replace bwrace=0 if isib_race==0
replace bwrace=1 if isib_race==1

*mean
global xlist isib_educ ipar_inc ipar_wealth  isib_child isib_age sib_agesq  isib_married isib_employ sib_sex isib_occupation
global ylist isib_famincome 

oaxaca $ylist $xlist [aw=sib_famweight] if sample==1, by(bwrace) xb detail threefold(reverse) noisily
outreg2 using testOaxacaIHS, replace excel dec(4) 

*proportional
oaxaca rif_10 $xlist [aw=sib_famweight] if sample==1, by(bwrace) detail threefold(reverse) noisily
outreg2 using testOaxacaIHS10, replace excel dec(4) 

oaxaca rif_50 $xlist [aw=sib_famweight] if sample==1, by(bwrace)  detail threefold(reverse) noisily
outreg2 using testOaxacaIHS50, replace excel dec(4) 

oaxaca rif_90 $xlist [aw=sib_famweight] if sample==1, by(bwrace)  detail threefold(reverse) noisily  
outreg2 using testOaxacaIHS90, replace excel dec(4) 

drop f50 f10 m90 m50 m10 f90 f50 f10 m90 m50 m10 dif90 dif50 dif10 densw1 evalw1 densb1 evalb1 rif_90 rif_10 rif_50 eval1 densw evalw eval2 densb evalb 

*****************************************************************************************
*two-stage least squares (2SLS/IV) for panel data                                       *
* paternal grandparent - grandchild estimates (Table 1, Column (2))                     *  
*****************************************************************************************
//working directory
clear 
clear matrix
*cd "change working directory"
use "aeapp_mobility_column2_table1.dta"

//paternal grandparental wealth
global xlist isib_age sib_agesq  isib_race 
global x2list igpar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref i.year  sib_sex 
global ylist isib_famincome 

xtivreg $ylist $xlist (isib_stocks=$x2list), first re ec2sls vce(cluster sib_id) 
outreg2 using paternal2SLS, replace excel dec(4)


*****************************************************************************************
*first stage results                                                                    *
* paternal grandparent - grandchild estimates (Table 2, Column (2))                     *  
*****************************************************************************************

//first stage results
reg isib_stocks isib_age sib_agesq isib_race  igpar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref sib_sex year2003 year2005 year2007 year2009 year2011 year2013 year2015 year2017 if e(sample)==1 
outreg2 using mypaternalfile, replace excel dec(4)


*****************************************************************************************  
* oaxaca-blinder decomposition                                                          * 
* paternal grandparent - grandchild estimates                                           * 
*****************************************************************************************
keep if year==2017 

*IHS applied to household income ;
sort isib_race //non-Latino Black=1, non-Latino White=0
sum isib_famincome if isib_race==0  [aw=sib_famweight], detail
gen f90=r(p90)
gen f50=r(p50)
gen f10=r(p10)
sum isib_famincome if isib_race==1  [aw=sib_famweight], detail
gen m90=r(p90)
gen m50=r(p50)
gen m10=r(p10)
gen dif90=m90-f90
gen dif50=m50-f50
gen dif10=m10-f10
di "  m-f q10 =" dif10 "  m-f q50 =" dif50 "  m-f q90 ="  dif90

**perform the RIF decomposition
*** graph the densities to check bandwidth ;
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], gen(evalw1 densw1) width(0.10) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], gen(evalb1 densb1) width(0.10)  
*
set scheme s1color
label var evalb "IHS Income"
label var evalw "IHS Income"
graph twoway  (connected densb1 evalb1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )  /*
   */   (connected densw1  evalw1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  /*
   */   , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) /*
   */   xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  /* 
   */   legend(pos(7) col(2) lab(1 "Black")  lab(2 "White")    /*
   */   region(lstyle(none)) symxsize(8) keygap(1) textwidth(34) ) /*
   */   saving(ExtFamIncMobility,replace)
   
* compute RIF for the 10th, 50th and 90th quantiles for blacks and whites ;
forvalues qt = 10(40)90 {	
   gen rif_`qt'=.
}
pctile eval1=isib_famincome if isib_race==0  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], at(eval1) gen(evalw densw) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalw[`qt']+`qc'/densw[`qt'] if isib_famincome>=evalw[`qt'] & isib_race==0
 replace rif_`qt'=evalw[`qt']-(1-`qc')/densw[`qt'] if isib_famincome<evalw[`qt'] & isib_race==0
}
pctile eval2=isib_famincome if isib_race==1  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], at(eval2) gen(evalb densb) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalb[`qt']+`qc'/densb[`qt'] if isib_famincome>=evalb[`qt'] & isib_race==1
 replace rif_`qt'=evalb[`qt']-(1-`qc')/densb[`qt'] if isib_famincome<evalb[`qt'] & isib_race==1
}
sort isib_race
by isib_race: sum rif_10 rif_50 rif_90  [aw=sib_famweight]
** perform oaxaca decomposition;
gen bwrace=.
replace bwrace=0 if isib_race==0
replace bwrace=1 if isib_race==1

*mean
global xlist isib_educ igpar_inc igpar_wealth isib_child isib_age sib_agesq  isib_married  isib_employ sib_sex isib_occupation 
global ylist isib_famincome

oaxaca $ylist $xlist [aw=sib_famweight], by(bwrace) xb detail threefold(reverse) noisily
outreg2 using testOaxacaPaternalIHS, replace excel dec(4) 

*proportional
oaxaca rif_10 $xlist [aw=sib_famweight], by(bwrace) detail threefold(reverse) noisily
outreg2 using testOaxacaPaternalIHS10, replace excel dec(4) 

oaxaca rif_50 $xlist [aw=sib_famweight], by(bwrace)  detail threefold(reverse) noisily
outreg2 using testOaxacaPaternalIHS50, replace excel dec(4) 

oaxaca rif_90 $xlist [aw=sib_famweight], by(bwrace)  detail threefold(reverse)  noisily  
outreg2 using testOaxacaPaternalIHS90, replace excel dec(4) 

drop f50 f10 m90 m50 m10 f90 f50 f10 m90 m50 m10 dif90 dif50 dif10 densw1 evalw1 densb1 evalb1 rif_90 rif_10 rif_50 eval1 densw evalw eval2 densb evalb 

*****************************************************************************************
*two-stage least squares (2SLS/IV) for panel data                                       *
* maternal grandparent - grandchild estimates (Table 1, Column (3))                     *  
*****************************************************************************************
//working directory
clear 
clear matrix
*cd "change working directory"
use "aeapp_mobility_column3_table1.dta"

//maternal grandparental wealth
global xlist isib_age sib_agesq  isib_race   
global x2list igpar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref i.year sib_sex 
global ylist isib_famincome 

xtivreg $ylist $xlist (isib_stocks=$x2list), first re ec2sls vce(cluster sib_id)
outreg2 using maternal2SLS, replace excel dec(4) //


*****************************************************************************************
*first stage results                                                                    *
* maternal grandparent - grandchild estimates (Table 2, Column (3))                     *  
*****************************************************************************************

//first stage results
reg isib_stocks isib_age sib_agesq isib_race  igpar_wealth isib_educ isib_child isib_married isib_employ isib_occupation sib_pension isib_2home isib_computeruse isib_riskpref sib_sex year2003 year2005 year2007 year2009 year2011 year2013 year2015 year2017 if e(sample)==1 
outreg2 using mymaternalfile, replace excel dec(4)


*****************************************************************************************  
* oaxaca-blinder decomposition                                                          * 
* maternal grandparent - grandchild estimates (Table 3, Panel B)                        * 
*****************************************************************************************
keep if year==2017

*IHS applied to household income ;
sort isib_race //non-Latino Black=1, non-Latino White=0
sum isib_famincome if isib_race==0  [aw=sib_famweight], detail
gen f90=r(p90)
gen f50=r(p50)
gen f10=r(p10)
sum isib_famincome if isib_race==1  [aw=sib_famweight], detail
gen m90=r(p90)
gen m50=r(p50)
gen m10=r(p10)
gen dif90=m90-f90
gen dif50=m50-f50
gen dif10=m10-f10
di "  m-f q10 =" dif10 "  m-f q50 =" dif50 "  m-f q90 ="  dif90

**perform the RIF decomposition
*** graph the densities to check bandwidth ;
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], gen(evalw1 densw1) width(0.10) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], gen(evalb1 densb1) width(0.10)  
*
set scheme s1color
label var evalb "IHS Income"
label var evalw "IHS Income"
graph twoway  (connected densb1 evalb1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )  /*
   */   (connected densw1  evalw1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  /*
   */   , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) /*
   */   xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  /* 
   */   legend(pos(7) col(2) lab(1 "Black")  lab(2 "White")    /*
   */   region(lstyle(none)) symxsize(8) keygap(1) textwidth(34) ) /*
   */   saving(ExtFamIncMobility,replace)
   
* compute RIF for the 10th, 50th and 90th quantiles for blacks and whites ;
forvalues qt = 10(40)90 {	
   gen rif_`qt'=.
}
pctile eval1=isib_famincome if isib_race==0  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==0  [aw=sib_famweight], at(eval1) gen(evalw densw) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalw[`qt']+`qc'/densw[`qt'] if isib_famincome>=evalw[`qt'] & isib_race==0
 replace rif_`qt'=evalw[`qt']-(1-`qc')/densw[`qt'] if isib_famincome<evalw[`qt'] & isib_race==0
}
pctile eval2=isib_famincome if isib_race==1  [aw=sib_famweight], nq(100) 
kdensity isib_famincome if isib_race==1  [aw=sib_famweight], at(eval2) gen(evalb densb) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalb[`qt']+`qc'/densb[`qt'] if isib_famincome>=evalb[`qt'] & isib_race==1
 replace rif_`qt'=evalb[`qt']-(1-`qc')/densb[`qt'] if isib_famincome<evalb[`qt'] & isib_race==1
}
sort isib_race
by isib_race: sum rif_10 rif_50 rif_90  [aw=sib_famweight]
** perform oaxaca decomposition;
gen bwrace=.
replace bwrace=0 if isib_race==0
replace bwrace=1 if isib_race==1

*mean
global xlist isib_educ igpar_inc igpar_wealth isib_child isib_age sib_agesq  isib_married  isib_employ sib_sex isib_occupation 
global ylist isib_famincome

oaxaca $ylist $xlist [aw=sib_famweight], by(bwrace) xb detail threefold(reverse) noisily
outreg2 using testOaxacaMaternalIHS, replace excel dec(4) 

*proportional
oaxaca rif_10 $xlist [aw=sib_famweight], by(bwrace) detail threefold(reverse) noisily
outreg2 using testOaxacaMaternalIHS10, replace excel dec(4) 

oaxaca rif_50 $xlist [aw=sib_famweight], by(bwrace)  detail threefold(reverse) noisily
outreg2 using testOaxacaMaternalIHS50, replace excel dec(4) 

oaxaca rif_90 $xlist [aw=sib_famweight], by(bwrace)  detail threefold(reverse)  noisily  
outreg2 using testOaxacaMaternalIHS90, replace excel dec(4) 

drop f50 f10 m90 m50 m10 f90 f50 f10 m90 m50 m10 dif90 dif50 dif10 densw1 evalw1 densb1 evalb1 rif_90 rif_10 rif_50 eval1 densw evalw eval2 densb evalb 


