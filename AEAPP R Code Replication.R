################################################################################
#two-stage least squares (2SLS/IV) for panel data
#parent-child estimates  (Table 1, Column (1))
################################################################################
if (!require("haven")) install.packages("haven")
if (!require("fixest")) install.packages("fixest")
if (!require("stargazer")) install.packages("stargazer")

library(haven)
library(fixest)
library(stargazer)

setwd("/Users/zequi/Desktop/Berkeley/230A/stat230A")

data <- read_dta("aeapp_mobility_table1_and_summarystatistics.dta")

xlist <- c("isib_age", "sib_agesq", "isib_race")
x2list <- c("ipar_wealth", "isib_educ", "isib_child", "isib_married", "isib_employ", 
            "isib_occupation", "sib_pension", "isib_2home", "isib_computeruse", 
            "isib_riskpref", "year", "sib_sex")
ylist <- "isib_famincome"

model <- feols(as.formula(paste(ylist, "~", paste(xlist, collapse = "+"), 
                                "|", paste(x2list, collapse = "+"))), 
               data = data, cluster = ~sib_id)
#Question 1: Should we worry about this:
#NOTE: 61,576 observations removed because of NA values (LHS: 12,593, RHS: 12,593, Fixed-effects: 61,576).

table_output <- etable(model, tex = FALSE, digits = 4)
table_lines <- capture.output(print(table_output))
writeLines(table_lines, "parent2SLS_results.txt")

################################################################################
#first stage results
#parent-child estimates  (Table 2, Column (1)) 
################################################################################  
# Load necessary libraries
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("stargazer")) install.packages("stargazer")

library(openxlsx)
library(stargazer)

data_used <- subset(data, sample == 1)

model <- lm(isib_stocks ~ isib_age + sib_agesq + isib_race + ipar_wealth + isib_educ + 
              isib_child + isib_married + isib_employ + isib_occupation + sib_pension + 
              isib_2home + isib_computeruse + isib_riskpref + sib_sex + year2003 + year2005 + 
              year2007 + year2009 + year2011 + year2013 + year2015 + year2017, 
            data = data_used)

# Output results to the console using stargazer
stargazer(model, type = "text", out = "myparentfile.txt", title = "First Stage Results", 
          decimal.mark = ".", digits = 4)

# For exporting the results to Excel
model_summary <- summary(model)
#Quesiton 3: verify results
write.xlsx(model_summary$coefficients, file = "myparentfile.xlsx", sheetName = "Regression Results", rowNames = TRUE)


################################################################################
# household income at life event for adult children
# parent-child estimates (summary statistics)
################################################################################
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Household income when adult children have their firstborn child
## Entire sample
overall_stats <- data %>%
  filter(sample == 1, firstchild != 0) %>%
  summarise(
    Mean = mean(firstchild, na.rm = TRUE),
    SEMean = sd(firstchild, na.rm = TRUE) / sqrt(n()),
    Median = median(firstchild, na.rm = TRUE),
    Min = min(firstchild, na.rm = TRUE),
    Max = max(firstchild, na.rm = TRUE),
    N = n()
  )

## Black sample
black_sample_stats <- data %>%
  filter(sample == 1, isib_race == 1, firstchild != 0) %>%
  summarise(
    Mean = mean(firstchild, na.rm = TRUE),
    SEMean = sd(firstchild, na.rm = TRUE) / sqrt(n()),
    Median = median(firstchild, na.rm = TRUE),
    Min = min(firstchild, na.rm = TRUE),
    Max = max(firstchild, na.rm = TRUE),
    N = n()
  )

## White sample
white_sample_stats <- data %>%
  filter(sample == 1, isib_race == 0, firstchild != 0) %>%
  summarise(
    Mean = mean(firstchild, na.rm = TRUE),
    SEMean = sd(firstchild, na.rm = TRUE) / sqrt(n()),
    Median = median(firstchild, na.rm = TRUE),
    Min = min(firstchild, na.rm = TRUE),
    Max = max(firstchild, na.rm = TRUE),
    N = n()
  )

print("Overall Sample Stats:")
print(overall_stats)
print("Black Sample Stats:")
print(black_sample_stats)
print("White Sample Stats:")
print(white_sample_stats)

#household income when adult children complete college
## Entire sample
overall_college_stats <- data %>%
  filter(sample == 1, firstcollege != 0) %>%
  summarise(
    Mean = mean(firstcollege, na.rm = TRUE),
    SEMean = sd(firstcollege, na.rm = TRUE) / sqrt(n()),
    Median = median(firstcollege, na.rm = TRUE),
    Min = min(firstcollege, na.rm = TRUE),
    Max = max(firstcollege, na.rm = TRUE),
    N = n()
  )

## Black sample
black_college_stats <- data %>%
  filter(sample == 1, isib_race == 1, firstcollege != 0) %>%
  summarise(
    Mean = mean(firstcollege, na.rm = TRUE),
    SEMean = sd(firstcollege, na.rm = TRUE) / sqrt(n()),
    Median = median(firstcollege, na.rm = TRUE),
    Min = min(firstcollege, na.rm = TRUE),
    Max = max(firstcollege, na.rm = TRUE),
    N = n()
  )

## White sample
white_college_stats <- data %>%
  filter(sample == 1, isib_race == 0, firstcollege != 0) %>%
  summarise(
    Mean = mean(firstcollege, na.rm = TRUE),
    SEMean = sd(firstcollege, na.rm = TRUE) / sqrt(n()),
    Median = median(firstcollege, na.rm = TRUE),
    Min = min(firstcollege, na.rm = TRUE),
    Max = max(firstcollege, na.rm = TRUE),
    N = n()
  )

print("Overall Sample Stats for College Completion:")
print(overall_college_stats)
print("Black Sample Stats for College Completion:")
print(black_college_stats)
print("White Sample Stats for College Completion:")
print(white_college_stats)


#household income when adult children enter their first marriage 
## Entire sample
overall_marriage_stats <- data %>%
  filter(sample == 1, firstmarriage != 0) %>%
  summarise(
    Mean = mean(firstmarriage, na.rm = TRUE),
    SEMean = sd(firstmarriage, na.rm = TRUE) / sqrt(n()),
    Median = median(firstmarriage, na.rm = TRUE),
    Min = min(firstmarriage, na.rm = TRUE),
    Max = max(firstmarriage, na.rm = TRUE),
    N = n()
  )

## Black sample
black_marriage_stats <- data %>%
  filter(sample == 1, isib_race == 1, firstmarriage != 0) %>%
  summarise(
    Mean = mean(firstmarriage, na.rm = TRUE),
    SEMean = sd(firstmarriage, na.rm = TRUE) / sqrt(n()),
    Median = median(firstmarriage, na.rm = TRUE),
    Min = min(firstmarriage, na.rm = TRUE),
    Max = max(firstmarriage, na.rm = TRUE),
    N = n()
  )

## White sample
white_marriage_stats <- data %>%
  filter(sample == 1, isib_race == 0, firstmarriage != 0) %>%
  summarise(
    Mean = mean(firstmarriage, na.rm = TRUE),
    SEMean = sd(firstmarriage, na.rm = TRUE) / sqrt(n()),
    Median = median(firstmarriage, na.rm = TRUE),
    Min = min(firstmarriage, na.rm = TRUE),
    Max = max(firstmarriage, na.rm = TRUE),
    N = n()
  )

print("Overall Sample Stats for First Marriage:")
print(overall_marriage_stats)
print("Black Sample Stats for First Marriage:")
print(black_marriage_stats)
print("White Sample Stats for First Marriage:")
print(white_marriage_stats)

#household income when adult children turn forty 
## Entire sample
entire_sample_forty_stats <- data %>%
  filter(sample == 1, firstforty != 0) %>%
  summarise(
    Mean = mean(firstforty, na.rm = TRUE),
    SEMean = sd(firstforty, na.rm = TRUE) / sqrt(n()),
    Median = median(firstforty, na.rm = TRUE),
    Min = min(firstforty, na.rm = TRUE),
    Max = max(firstforty, na.rm = TRUE),
    N = n()
  )

## Black sample
black_sample_forty_stats <- data %>%
  filter(sample == 1, isib_race == 1, firstforty != 0) %>%
  summarise(
    Mean = mean(firstforty, na.rm = TRUE),
    SEMean = sd(firstforty, na.rm = TRUE) / sqrt(n()),
    Median = median(firstforty, na.rm = TRUE),
    Min = min(firstforty, na.rm = TRUE),
    Max = max(firstforty, na.rm = TRUE),
    N = n()
  )

## White sample
white_sample_forty_stats <- data %>%
  filter(sample == 1, isib_race == 0, firstforty != 0) %>%
  summarise(
    Mean = mean(firstforty, na.rm = TRUE),
    SEMean = sd(firstforty, na.rm = TRUE) / sqrt(n()),
    Median = median(firstforty, na.rm = TRUE),
    Min = min(firstforty, na.rm = TRUE),
    Max = max(firstforty, na.rm = TRUE),
    N = n()
  )

print(entire_sample_forty_stats)
print(black_sample_forty_stats)
print(white_sample_forty_stats)

#household income when adult children become first time homeowner
## Entire sample
entire_sample_homeowner_stats <- data %>%
  filter(sample == 1, firsthome != 0) %>%
  summarise(
    Mean = mean(firsthome, na.rm = TRUE),
    SEMean = sd(firsthome, na.rm = TRUE) / sqrt(n()),
    Median = median(firsthome, na.rm = TRUE),
    Min = min(firsthome, na.rm = TRUE),
    Max = max(firsthome, na.rm = TRUE),
    N = n()
  )
print(entire_sample_homeowner_stats)

## Black sample
black_sample_homeowner_stats <- data %>%
  filter(sample == 1, isib_race == 1, firsthome != 0) %>%
  summarise(
    Mean = mean(firsthome, na.rm = TRUE),
    SEMean = sd(firsthome, na.rm = TRUE) / sqrt(n()),
    Median = median(firsthome, na.rm = TRUE),
    Min = min(firsthome, na.rm = TRUE),
    Max = max(firsthome, na.rm = TRUE),
    N = n()
  )
print(black_sample_homeowner_stats)

## White sample
white_sample_homeowner_stats <- data %>%
  filter(sample == 1, isib_race == 0, firsthome != 0) %>%
  summarise(
    Mean = mean(firsthome, na.rm = TRUE),
    SEMean = sd(firsthome, na.rm = TRUE) / sqrt(n()),
    Median = median(firsthome, na.rm = TRUE),
    Min = min(firsthome, na.rm = TRUE),
    Max = max(firsthome, na.rm = TRUE),
    N = n()
  )
print(white_sample_homeowner_stats)

#household income when adult children form their own household 
## Entire sample
entire_sample_ownhousehold_stats <- data %>%
  filter(sample == 1, ownhousehold != 0) %>%
  summarise(
    Mean = mean(ownhousehold, na.rm = TRUE),
    SEMean = sd(ownhousehold, na.rm = TRUE) / sqrt(n()),
    Median = median(ownhousehold, na.rm = TRUE),
    Min = min(ownhousehold, na.rm = TRUE),
    Max = max(ownhousehold, na.rm = TRUE),
    N = n()
  )
print(entire_sample_ownhousehold_stats)

## Black sample
black_sample_ownhousehold_stats <- data %>%
  filter(sample == 1, isib_race == 1, ownhousehold != 0) %>%
  summarise(
    Mean = mean(ownhousehold, na.rm = TRUE),
    SEMean = sd(ownhousehold, na.rm = TRUE) / sqrt(n()),
    Median = median(ownhousehold, na.rm = TRUE),
    Min = min(ownhousehold, na.rm = TRUE),
    Max = max(ownhousehold, na.rm = TRUE),
    N = n()
  )
print(black_sample_ownhousehold_stats)

## White sample
white_sample_ownhousehold_stats <- data %>%
  filter(sample == 1, isib_race == 0, ownhousehold != 0) %>%
  summarise(
    Mean = mean(ownhousehold, na.rm = TRUE),
    SEMean = sd(ownhousehold, na.rm = TRUE) / sqrt(n()),
    Median = median(ownhousehold, na.rm = TRUE),
    Min = min(ownhousehold, na.rm = TRUE),
    Max = max(ownhousehold, na.rm = TRUE),
    N = n()
  )
print(white_sample_ownhousehold_stats)

################################################################################
# parental transfers and income prospects
# parent-child estimates (summary statistics)
################################################################################

#household income when adult children received post-secondary support from parents
## Entire sample
entire_sample_schoolmoney_stats <- data %>%
  filter(sample == 1) %>%
  summarise(
    Mean = mean(schoolmoney, na.rm = TRUE),
    SEMean = sd(schoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(schoolmoney, na.rm = TRUE),
    Min = min(schoolmoney, na.rm = TRUE),
    Max = max(schoolmoney, na.rm = TRUE),
    N = n()
  )
print(entire_sample_schoolmoney_stats)

## Black sample
black_sample_schoolmoney_stats <- data %>%
  filter(sample == 1, isib_race == 1) %>%
  summarise(
    Mean = mean(schoolmoney, na.rm = TRUE),
    SEMean = sd(schoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(schoolmoney, na.rm = TRUE),
    Min = min(schoolmoney, na.rm = TRUE),
    Max = max(schoolmoney, na.rm = TRUE),
    N = n()
  )
print(black_sample_schoolmoney_stats)

## White sample
white_sample_schoolmoney_stats <- data %>%
  filter(sample == 1, isib_race == 0) %>%
  summarise(
    Mean = mean(schoolmoney, na.rm = TRUE),
    SEMean = sd(schoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(schoolmoney, na.rm = TRUE),
    Min = min(schoolmoney, na.rm = TRUE),
    Max = max(schoolmoney, na.rm = TRUE),
    N = n()
  )
print(white_sample_schoolmoney_stats)

## Entire sample
entire_sample_noschoolmoney_stats <- data %>%
  filter(sample == 1, noschoolmoney != 0) %>%
  summarise(
    Mean = mean(noschoolmoney, na.rm = TRUE),
    SEMean = sd(noschoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(noschoolmoney, na.rm = TRUE),
    Min = min(noschoolmoney, na.rm = TRUE),
    Max = max(noschoolmoney, na.rm = TRUE),
    N = n()
  )
print(entire_sample_noschoolmoney_stats)

## Black sample
black_sample_noschoolmoney_stats <- data %>%
  filter(sample == 1, isib_race == 1, noschoolmoney != 0) %>%
  summarise(
    Mean = mean(noschoolmoney, na.rm = TRUE),
    SEMean = sd(noschoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(noschoolmoney, na.rm = TRUE),
    Min = min(noschoolmoney, na.rm = TRUE),
    Max = max(noschoolmoney, na.rm = TRUE),
    N = n()
  )
print(black_sample_noschoolmoney_stats)

## White sample
white_sample_noschoolmoney_stats <- data %>%
  filter(sample == 1, isib_race == 0, noschoolmoney != 0) %>%
  summarise(
    Mean = mean(noschoolmoney, na.rm = TRUE),
    SEMean = sd(noschoolmoney, na.rm = TRUE) / sqrt(n()),
    Median = median(noschoolmoney, na.rm = TRUE),
    Min = min(noschoolmoney, na.rm = TRUE),
    Max = max(noschoolmoney, na.rm = TRUE),
    N = n()
  )
print(white_sample_noschoolmoney_stats)

#household income when adult children received house purchase support from parents 
## Entire sample
entire_sample_housemoney_stats <- data %>%
  filter(sample == 1) %>%
  summarise(
    Mean = mean(housemoney, na.rm = TRUE),
    SEMean = sd(housemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(housemoney, na.rm = TRUE),
    Min = min(housemoney, na.rm = TRUE),
    Max = max(housemoney, na.rm = TRUE),
    N = n()
  )
print(entire_sample_housemoney_stats)

## Black sample
black_sample_housemoney_stats <- data %>%
  filter(sample == 1, isib_race == 1) %>%
  summarise(
    Mean = mean(housemoney, na.rm = TRUE),
    SEMean = sd(housemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(housemoney, na.rm = TRUE),
    Min = min(housemoney, na.rm = TRUE),
    Max = max(housemoney, na.rm = TRUE),
    N = n()
  )
print(black_sample_housemoney_stats)

## White sample
white_sample_housemoney_stats <- data %>%
  filter(sample == 1, isib_race == 0) %>%
  summarise(
    Mean = mean(housemoney, na.rm = TRUE),
    SEMean = sd(housemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(housemoney, na.rm = TRUE),
    Min = min(housemoney, na.rm = TRUE),
    Max = max(housemoney, na.rm = TRUE),
    N = n()
  )
print(white_sample_housemoney_stats)

## Entire sample
entire_sample_nohousemoney_stats <- data %>%
  filter(sample == 1, nohousemoney != 0) %>%
  summarise(
    Mean = mean(nohousemoney, na.rm = TRUE),
    SEMean = sd(nohousemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(nohousemoney, na.rm = TRUE),
    Min = min(nohousemoney, na.rm = TRUE),
    Max = max(nohousemoney, na.rm = TRUE),
    N = n()
  )
print(entire_sample_nohousemoney_stats)

## Black sample
black_sample_nohousemoney_stats <- data %>%
  filter(sample == 1, isib_race == 1, nohousemoney != 0) %>%
  summarise(
    Mean = mean(nohousemoney, na.rm = TRUE),
    SEMean = sd(nohousemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(nohousemoney, na.rm = TRUE),
    Min = min(nohousemoney, na.rm = TRUE),
    Max = max(nohousemoney, na.rm = TRUE),
    N = n()
  )
print(black_sample_nohousemoney_stats)

## White sample
white_sample_nohousemoney_stats <- data %>%
  filter(sample == 1, isib_race == 0, nohousemoney != 0) %>%
  summarise(
    Mean = mean(nohousemoney, na.rm = TRUE),
    SEMean = sd(nohousemoney, na.rm = TRUE) / sqrt(n()),
    Median = median(nohousemoney, na.rm = TRUE),
    Min = min(nohousemoney, na.rm = TRUE),
    Max = max(nohousemoney, na.rm = TRUE),
    N = n()
  )
print(white_sample_nohousemoney_stats)

################################################################################
# oaxaca-blinder decomposition
# parent-child estimates (Table 3, Panel A)
################################################################################
#IHS applied to household income ;
if (!require("Hmisc")) install.packages("Hmisc")

library(Hmisc)

data <- data %>%
  filter(year == 2017)

# Calculating weighted quantiles for non-Latino White
white_stats <- data %>%
  filter(isib_race == 0) %>%
  summarise(
    f90 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.90, na.rm = TRUE),
    f50 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.50, na.rm = TRUE),
    f10 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.10, na.rm = TRUE)
  )

# Calculating weighted quantiles for non-Latino Black
black_stats <- data %>%
  filter(isib_race == 1) %>%
  summarise(
    m90 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.90, na.rm = TRUE),
    m50 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.50, na.rm = TRUE),
    m10 = wtd.quantile(isib_famincome, weights = sib_famweight, probs = 0.10, na.rm = TRUE)
  )

# Calculate differences
dif_stats <- data.frame(
  dif90 = black_stats$m90 - white_stats$f90,
  dif50 = black_stats$m50 - white_stats$f50,
  dif10 = black_stats$m10 - white_stats$f10
)

# Display differences
print(paste("m-f q10 =", dif_stats$dif10, "m-f q50 =", dif_stats$dif50, "m-f q90 =", dif_stats$dif90))

#perform the RIF decomposition
## graph the densities to check bandwidth ;

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
##Next (line 155)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

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



