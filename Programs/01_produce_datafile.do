capture log close _all
clear all
version 17
set linesize 250
set more off
set varabbrev off
set type double
pause on

* define path to data
global root "C:\Users\Jennah\iCloudDrive\Desktop\Wagner\AEM\Replication"

*global inp  "${root}/raw pums80 slim.dta"
global prog "$root/Programs"
global out  "${root}/Output"

global data  "raw pums80 slim.dta"

* define log file
log using "${prog}/01_produce_datafile.log", replace text

*---------------------------------------------------------------------------------------------------
*  Course: 	Advanced Empirical Methods
*  Date:  	10/13/2021
*  Author: 	Jennah Gosciak
*  Assgnmt: Replication descriptives
*---------------------------------------------------------------------------------------------------

*---------------------------------------------------------------------------------------------------
* LOAD DATA + SIMPLE DESCRIPTIVES
*---------------------------------------------------------------------------------------------------
use "${root}/${data}", clear
count 

drop agemarr us80a_momloc age sex hispan school race
ren us80a_* *

* generate birthfrac variable based on birthqtr
gen birthfrac = 1-(birthqtr/4)
gen age_qtr = age + birthfrac
assert age_qtr < 1 if age == 0

*---------------------------------------------------------------------------------------------------
* Initial constructs
*---------------------------------------------------------------------------------------------------
* Race and ethnicity
* indicator for Black
tablist race, sort(v) 
gen 	r_black = 	(race == 3) if !mi(race) 
tablist r_black 	 race, sort(v) ab(30) clean

*gen 	hisp = (inlist(hispan, 1, 2, 3, 4) | race == 2) if !mi(hispan)
gen 	hisp = (race == 2) if !mi(race)
tablist hisp hispan race, sort(v) ab(30)

gen 	r_oth = (inrange(race, 4, 13)) if !mi(race)
tablist r_oth		 race, sort(v) ab(30) clean

* chcek race variables
tablist r_black hisp r_oth race, sort(v) ab(30) clean

* labor variables
* worked for pay (= 1 if worked for pay in year prior to census)
assert !mi(wkswork1)
assert inrange(wkswork1, 0, 52)
* recode us80a_wkswork1 (0=0 "Didnt work") (1/52=1 "Worked"), gen(wrkdlastyr)
gen 	wkpay_lyr = (wkswork1 > 0)
tablist wkpay_lyr 	 wkswork1, sort(v) ab(30) clean

* weeks worked (already in data)
* wkswork1
assert 		wkswork1 == 0 if wkpay_lyr == 0

* hours per week 
* uhrswork
assert	 	uhrswork == 0 if wkpay_lyr == 0

* labor income (labor earnings in year prior to census, in 1995 dollars)
* incwage
clonevar 	incwage_orig = incwage
replace	 	incwage = 0 if wkpay_lyr == 0
replace 	incwage = 2.099173554 * incwage_orig

* family income (family income in year prior to census, in 1995 dollars)
* ftotinc
clonevar 	ftotinc_orig = ftotinc
replace 	ftotinc = 2.099173554 * ftotinc_orig

* non-wife income (family income minus wife's labor income, in 1995 dollars)
gen nonwinc = ftotinc - incwage

* replace 0 and negative values with 1
clonevar ftotinc_temp = ftotinc
replace  ftotinc_temp = 1 if ftotinc_temp <= 0

clonevar nonwinc_temp = nonwinc
replace  nonwinc_temp = 1 if nonwinc_temp <= 0

gen l_ftotinc = ln(ftotinc_temp) // log transform variables
gen l_nonwinc = ln(nonwinc_temp)

local outcome_vars wkpay_lyr wkswork1 uhrswork incwage ftotinc l_ftotinc l_nonwinc 

*---------------------------------------------------------------------------------------------------
* RESHAPE DATA
*---------------------------------------------------------------------------------------------------
* Filter for children
preserve
	keep serial momloc sex age birthqtr qsex qage qbirthmo age_qtr

	keep if momloc != 0 // filter to respondent who has a mother in sample

	gen birthyr = 1980 - age

	* reverse order, oldest (= first child) should be first
	*bysort serial momloc (age_qtr): gen sort_order = _n
	*bysort serial momloc (age_qtr): gen child_order = (_N - _n) + 1
	
	bysort serial momloc (birthyr birthqtr): gen child_order = _n
	sum child_order
	drop birthyr

	*tablist child_order sort_order, sort(v) ab(30) clean

	reshape wide sex age age_qtr birthqtr qsex qage qbirthmo, i(serial momloc) j(child_order)

	* restricted to women for whom the reported values of age and 
	* sex of their two oldest children
	* were not allocated by the US Census Bureau

	* drop all the flags equal to 1???
	* angrist and evans don't use observations with flagged or allocated values
	/*
	keep if (qsex1 == 0 & /// 
			 qsex2 == 0 & ///
			 qage1 == 0 & /// 
			 qage2 == 0 & ///
			 qbirthmo1 == 0 & qbirthmo2 == 0) //& qage1 == 0 & & qage2 == 0 &)
	*/

	keep if qsex1 == 0
	keep if qsex2 == 0
	keep if qage1 == 0
	keep if qage2 == 0
	keep if qbirthmo1 == 0
	keep if qbirthmo2 == 0
	* if I don't use qbirthmo, I get too high of an estimate?

	ren momloc pernum // rename for merging later
	isid serial pernum

	* file unique on serial and momloc
	tempfile 	momloc_data
	save 		"`momloc_data'", replace
restore 

* Filter for father
preserve 
	keep if sploc != 0 & sex == 1
	keep serial sploc sex age birthqtr qsex qage qbirthmo age_qtr `outcome_vars'

	ren * *_father
	ren (sploc_father serial_father) (pernum serial)
	isid serial pernum

	tempfile 	sploc_data
	save 		"`sploc_data'", replace
restore 

count 

* merge back onto the original data fileusing serial and pernum
merge 1:1 serial pernum using "`momloc_data'", nogen 	keep(3) // merge to children
merge 1:1 serial pernum using "`sploc_data'", 			keep(1 3) // merge to fathers

ren _merge _father_merge
count

save "${out}/merged_data", replace

*---------------------------------------------------------------------------------------------------
* PRODUCE CONSTRUCTS
*---------------------------------------------------------------------------------------------------
use "${out}/merged_data", clear
isid serial pernum // check uniquenesss

* sex-mix estimation strategy is implemented using information
* on labor-supply, sex of mothers' first two children, and an
* indicator of multiple births

* generate indicator for mother
tablist 	sex, sort(v) ab(30) clean
assert  	sex == 2

* generate indicators for number of children
* create match variable
clonevar 	chborn_orig = chborn
replace  	chborn = chborn - 1

label drop us80a_chbornlbl
tablist chborn chborn_orig, sort(v) ab(30) clean

* women aged 21-25 and aged 36-50
gen age_21_35 = inrange(age, 21, 35)
gen age_36_50 = inrange(age, 36, 50)

tablist age_21_35 age_36_50 age, sort(v) ab(30) clean

* we deleted any mother for whom the number of children in the household did not match
* the number of children ever born
assert chborn_orig != 0 // not in universe
*assert chborn_orig != 1 // has 0 children
replace chborn = 9 if inrange(chborn, 9, 12) // need to fix, coding does not line up
 
count 	if nchild != chborn
drop 	if nchild != chborn

* all women with two or more children
* compare accuracy of construct produced with chborn
egen 	count_child = rownonmiss(sex1 	sex2 	sex3 	sex4 	sex5 	sex6 	sex7 	sex8 	sex9 	sex10 ///
								 sex11 	sex12 	sex13	sex14 	sex15 	sex16 	sex17)
*gen 	cnum_mte2_test = count_child >= 2
* these actually don't match in all cases
*tablist cnum_mte2 cnum_mte2_test, sort(v) ab(30) clean // compare accuracy of variable

* indicators for number of children
gen cnum_2 = (count_child == 2) 	if !mi(count_child)
gen cnum_mt2 = 	count_child > 2	if !mi(count_child)
gen cnum_mte2 = count_child >= 2	if !mi(count_child)

tablist count_child chborn cnum_2 cnum_mt2 cnum_mte2, sort(v) ab(30) clean

label variable cnum_2 		"Mother had two children"
label variable cnum_mt2 	"Mother had more than two children"
label variable cnum_mte2 	"Mother had more than or equal to two children"

* the sample is limited to mothers aged 21-35
* whose oldest child was less than 18 years of age at the time of the census
* except for women who's second child is less than 1 year old
egen age_oldest = 	rowmax(age? age??)
assert age_oldest == age1 // check this is true

egen age_youngest = rowmin(age? age??)
label variable age_oldest 	"Age of mother's oldest child"
label variable age_youngest "Age of mother's youngest child"

egen age_oldest_qtr = rowmax(age_qtr? age_qtr??)
label variable age_oldest 	"Age of mother's oldest child (qtrs)"

* table 2 variables
* already in data:
* 1. chborn (with no modifications)
* 2. cnum_mt2

* 3. boy first
gen f_boy = (sex1 == 1)
label variable f_boy "First child is a boy"

* 4. boy second
gen s_boy = (sex2 == 1)
label variable s_boy "Second child is a boy"

* 5. two boys
gen twoboys = (sex1 == 1 & sex2 == 1)
label variable twoboys "First two children are boys"

* 6. two girls
gen twogirls = (sex1 == 2 & sex2 == 2)
label variable twogirls "First two children are girls"

* 7. same sex 
gen samesex = (sex1 == sex2)
label variable samesex "First two children are the same sex"

* 8. twins
* siblings having the same age and quarter of birth
* birthqtr increases accuracy
gen twins = (age2 == age3) & (birthqtr2 == birthqtr3)
label variable twins "Second and third children are twins"

* age at first birth
gen age_fbirth = (age_qtr - age_oldest_qtr)
label variable age_fbirth "Age at first birth"

gen age_fbirth_father = (age_qtr_father - age_oldest_qtr)
label variable age_fbirth_father "Age at first birth (father"

*---------------------------------------------------------------------------------------------------
* FILTER FOR SAMPLE 1 (all mothers), SAMPLE 2 (married mothers), and SAMPLE 3 (married fathers)
*---------------------------------------------------------------------------------------------------
keep if age_21_35 == 1 & age_oldest < 18 & cnum_mte2 == 1 & age2 >= 1 & !mi(age2)
*keep if inrange(age_qtr, 21, 35.75) & age_oldest < 18 & cnum_mte2 == 1 & age_qtr2 >= 1

* the first includes all women with two kids or more children
* age of first child must be greater than or equal to 1
* age of oldest less than 18
count
tempfile 	sample1 
save 		"`sample1'", replace
save 		"${out}/sample1", replace

preserve 
	* the second includes only married women
	* coules who were married at the time of the Census,
	* married only once
	* and married at the time of their first birth
	* length married = age - age married
	* length married >= age of first child
	*gen  	len_married = (age - agemarr)
	*gen 	marr_fbirth = (len_married >= age_oldest)
	*gen 	marr_fbirth = (age - age_oldest) >= agemarr

	gen marrfrac = 1 - (marrqtr/4)
	gen agemarr_qtr = agemarr + marrfrac

	* gte
	gen  	marr_fbirth = (age_qtr - age_oldest_qtr) >= agemarr_qtr
	
	label variable marr_fbirth "Married at first birth"
	tablist marr_fbirth 	age 		age_oldest, sort(v) ab(30) clean
	tablist marr_fbirth 	age_qtr 	age_oldest_qtr, sort(v) ab(30) clean

	tablist marst, sort(v) ab(30) clean
	keep if marst == 1 & marrno == 1 & marr_fbirth == 1 & _father_merge == 3
	assert cnum_mte2 == 1
	count

	tempfile 	sample2
	save 		"`sample2'", replace
	save 		"${out}/sample2", replace

	* create father sample
	drop `outcome_vars' sex age birthqtr qsex qage qbirthmo age_qtr age_fbirth
	ren (*_father) 	(*)
	keep serial `outcome_vars' sex age birthqtr qsex qage qbirthmo age_qtr age_fbirth
	assert sex == 1

	tempfile 	sample3
	save 		"`sample3'", replace
	save 		"${out}/sample3", replace
restore

*---------------------------------------------------------------------------------------------------
* TABLE 2
*---------------------------------------------------------------------------------------------------
tempname memhold 
tempfile tbl2

postfile `memhold' str30(varname sample mean) double(N order) ///
	using `tbl2', replace

forvalues s = 1/3 {
	preserve 
		use "${out}/sample`s'", clear

		if `s' == 1 local s_lab "all mothers"
		else if `s' == 2 local s_lab "married mothers"
		else if `s' == 3 local s_lab "married fathers"

		local sort_order = 0
		foreach var in 	chborn 		cnum_mt2 	f_boy 		s_boy 	twoboys twogirls samesex twins age age_fbirth wkpay_lyr ///
						uhrswork 	wkswork1 	incwage	 	ftotinc nonwinc {
			local sort_order = `sort_order' + 1 // increment

			cap confirm variable `var'
			if _rc != 0 {
				post `memhold' ("`var'") ("`s_lab'") ("") (.) (`sort_order')
				continue
			}

			sum `var'

			local n = cond(`r(mean)' >= 1000, 6, 5)
			local d = cond(`r(mean)' >= 1000, 0, 3)
			local mean : 	di %`n'.`d'fc `r(mean)'
			local sd : 		di %`n'.`d'fc `r(sd)'
			local mean_form "`mean' `=char(13)'(`sd')"
			post `memhold' ("`var'") ("`s_lab'") ("`mean_form'") (r(N)) (`sort_order')
		}
	restore 
}

postclose `memhold'

preserve
	use "`tbl2'", clear

	keep varname sample mean order
	replace sample = subinstr(sample, " ", "_", .)
	reshape wide mean, i(varname order) j(sample) string

	sort order
	drop order

	label variable meanall_mothers	 	"All mothers"
	label variable meanmarried_mothers 	"Married mothers"
	label variable meanmarried_fathers 	"Married fathers"
	order varname meanall_mothers meanmarried_mothers meanmarried_fathers

	export excel using "${out}/table_results.xlsx", sheet("table 2") sheetreplace firstrow(varlabels) keepcellfmt
restore 

*---------------------------------------------------------------------------------------------------
* TABLE 6 - OLS estimates
*---------------------------------------------------------------------------------------------------
local allcov 	f_boy s_boy samesex twoboys twogirls 	age age_fbirth r_black hisp r_oth
local covars1 				samesex
local covars2 	f_boy s_boy samesex 					age age_fbirth r_black hisp r_oth
local covars3 	f_boy 				twoboys twogirls 	age age_fbirth r_black hisp r_oth

tempname memhold 
tempfile tbl6

postfile `memhold' str30(varname sample) double(model order) str30(est) ///
	using `tbl6', replace

forvalues s = 1/2 {
	preserve 
		use "${out}/sample`s'", clear

		local s_lab = cond(`s' == 1, "all mothers", "married mothers")

		forvalues i = 1/3 {

			reg cnum_mt2 `covars`i'', r
			local r2 : di %4.3f e(r2)
			
			mat def results = r(table)
			
			local v_count = 1
			foreach v in `allcov' {
				local col = colnumb(results, "`v'")
				local est : di %5.4f `= results[1,`col']'
				local se : di %5.4f `= results[2,`col']'
				local pval = results[4,`col']

				if `pval' < 0.01 		local ast ***
				else if `pval' < 0.05 	local ast **
				else if `pval' < 0.1 	local ast *

				local est_form
				if !mi("`est'") & strtrim("`est'") != "." local est_form "`est'`ast' `=char(13)'(`se')"
				post `memhold' ("`v'") ("`s_lab'") (`i') (`v_count') ("`est_form'")

				local v_count = 1 + `v_count'
			}

			post `memhold' ("r2") ("`s_lab'") (`i') (`v_count') ("`r2'")
		}
	restore 
}

postclose `memhold'

preserve
	use "`tbl6'", clear

	list
	replace sample = subinstr(sample, " ", "_", .)
	reshape wide est, i(varname order sample) j(model)
	reshape wide est?, i(varname order) j(sample) string

	sort order
	drop order

	label variable est1all_mothers "All mothers, `=char(13)' OLS No covariates"
	label variable est2all_mothers "All mothers, `=char(13)' OLS"
	label variable est3all_mothers "All mothers, `=char(13)' OLS"

	label variable est1married_mothers "Married mothers, `=char(13)' OLS No covariates"
	label variable est2married_mothers "Married mothers, `=char(13)' OLS"
	label variable est3married_mothers "Married mothers, `=char(13)' OLS"
	
	export excel using "${out}/table_results.xlsx", sheet("table 6") sheetreplace firstrow(varlabels) keepcellfmt
restore

* Other covariates are: age, age at first birth, black, hispanic, and other race
* Boy 2nd
* (1)
use "${out}/sample1", clear
reg cnum_mt2 samesex, r 
outreg2 using "${out}/table_2", keep(samesex) excel ctitle(OLS - no covar) replace

reg cnum_mt2 f_boy s_boy samesex 						age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy samesex) excel ctitle(OLS) append

reg cnum_mt2 f_boy 					twoboys twogirls 	age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy twoboys twogirls) excel ctitle(OLS) append

use "${out}/sample2", clear
reg cnum_mt2 samesex, r 
outreg2 using "${out}/table_2", keep(samesex) excel ctitle(OLS - no covar) append

reg cnum_mt2 f_boy s_boy samesex 						age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy samesex) excel ctitle(OLS) append

reg cnum_mt2 f_boy 					twoboys twogirls 	age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy twoboys twogirls) excel ctitle(OLS) append

*---------------------------------------------------------------------------------------------------
* TABLE 7
*---------------------------------------------------------------------------------------------------
local depvars wkpay_lyr wkswork1 incwage l_ftotinc l_nonwinc
local allcov age age_fbirth f_boy s_boy r_black hisp r_oth
local instr2 samesex
local instr3 "twogirls twoboys"
tempname memhold 
tempfile tbl7

postfile `memhold' str30(varname sample) double(model order) str30(est) ///
	using `tbl7', replace

forvalues s = 1/2 {
	preserve 
		use "${out}/sample`s'", clear

		local s_lab = cond(`s' == 1, "all mothers", "married mothers")

		forvalues i = 1/3 {
			local v_order = 1
			foreach depvar in `depvars' {

				if inlist(`i', 2, 3) 	ivregress 2sls 	`depvar' 			`allcov' (cnum_mt2 = `instr`i''), 	r
				else  					reg  			`depvar' cnum_mt2 	`allcov',							r

				local est : di %7.4f `=_b[cnum_mt2]'
				local se : 	di %7.4f `=_se[cnum_mt2]'	

				local est_form "`est' `=char(13)' (`se')"
				post `memhold' ("`depvar'") ("`s_lab'") (`i') (`v_order') ("`est_form'")
				local v_order = `v_order' + 1
			}
		}
	restore 
}

postclose `memhold'

preserve
	use "`tbl7'", clear

	list
	replace sample = subinstr(sample, " ", "_", .)
	reshape wide est,  i(varname sample order) j(model)
	reshape wide est?, i(varname order) j(sample) string

	sort order 
	drop order 

	label variable est1all_mothers "All mothers, `=char(13)' OLS"
	label variable est2all_mothers "All mothers, `=char(13)' IV Same sex"
	label variable est3all_mothers "All mothers, `=char(13)' IV Two boys, two girls"

	label variable est1married_mothers "Married mothers, `=char(13)' OLS"
	label variable est2married_mothers "Married mothers, `=char(13)' IV Same sex"
	label variable est3married_mothers "Married mothers, `=char(13)' IV Two boys, two girls"
	
	export excel using "${out}/table_results.xlsx", sheet("table 7") sheetreplace firstrow(varlabels) keepcellfmt
restore



/*
NOTES
***************************************************************
* LINKING CHILDREN TO MOTHERS
* Households with one family

* linking mother and children
* if a woman was reported having given birth was designated as the householder
* or the spouse of another householder, we attached all people in the household
* labeled as child in the primary relationship code
tablist us80a_relate, sort(v)

gen hh_female = (inlist(us80a_relate, 101, 201) & sex == 2)
tablist hh_female us80a_relate if sex == 2, sort(v) ab(30)

gen child_ind = (us80a_relate == 301)
tablist child_ind us80a_relate, sort(v) ab(30) clean

* count children only if mother has ever had children
replace child_ind = . if !(hh_female == 1 & chborn > 0 & chborn < 99 & us80a_nfams == 1)

* in households with multiple families, we used detailed
* realtionsihp codes as well as subfamily numbers to pair 
* children with mothers
* if the woman who reported having given birth was a child of
* the householder, we attached all grandchildren of teh householder
* sharing the same subfamily codes to the woman who reported
* having the child, identifying her as the mother


* delete any mother for whom the number of children does
* in the household does not match the number of children ever born

* note: the sample is restricted to women for whom the reported
* values of age and sex of their two oldest children
* were not allocated by the US Bureau of the Census

* generate age in quarters 
gen 	ageqtr = 4*age
replace ageqtr = ageqtr - birthqtr

gen 	ageqtr_mar = 4*agemarr
replace ageqtr_mar = ageqtr_mar - marrqtr

tablist ageqtr age birthqtr, sort(v) ab(30)

*/

log close _all

