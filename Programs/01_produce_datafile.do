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

local data  "raw pums80 slim.dta"

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
use "${root}/`data'", clear
count 

* create varlist
preserve 
	desc *, clear replace
	list *
	export excel using "${out}/varlist.xlsx", replace firstrow(variables) keepcellfmt
restore 

* load data, put in mom exclusion criteria, then exclude them?
* identified mom's that we're interested in
* generated children of the mom's that we're interested in
* then split them

* drop nkids != total household kids
* 

* do age restrictions and age of second child, two or more kids
* do the drop if own children is different than number of children 

drop agemarr us80a_momloc age sex hispan school race
*drop us80a_agemarr us80a_momloc us80a_age us80a_sex us80a_hispan us80a_school us80a_race
ren us80a_* *

gen age_qtr = (age*4) + birthqtr
tablist age_qtr age birthqtr, sort(v) ab(30) clean

* Initial constructs
* Black indicator for race
tablist race, sort(v) 
gen 	r_black = 	(race == 3 & hispan == 0) if !mi(race) 
tablist r_black 	 race, sort(v) ab(30) clean

gen 	hisp = (inlist(hispan, 1, 2, 3, 4) | race == 2) if !mi(hispan)
tablist hisp hispan, sort(v) ab(30)

gen 	r_oth = (inrange(race, 4, 13) & hispan == 0) if !mi(race)
tablist r_oth		 race, sort(v) ab(30) clean

tablist r_black hisp r_oth race, sort(v) ab(30) clean

*---------------------------------------------------------------------------------------------------
* RESHAPE DATA
*---------------------------------------------------------------------------------------------------
*ren (us80a_birthqtr 	us80a_marrqtr 	us80a_serial 	us80a_pernum 	us80a_chborn 	us80a_nchild) ///
*	(birthqtr 			marrqtr 		serial 			pernum 			chborn 			nchild)

preserve
	keep serial momloc sex age birthqtr qsex qage qbirthmo age_qtr

	keep if momloc > 0 // filter to respondent who has a mother in sample

	* reverse order, oldest (= first child) should be first
	bysort serial momloc (age birthqtr): gen child_order = (_N - _n) + 1
	sum child_order

	reshape wide sex age age_qtr birthqtr qsex qage qbirthmo, i(serial momloc) j(child_order)

	* restricted to women for whom the reported values of age and 
	* sex of their two oldest children
	* were not allocated by the US Census Bureau

	* drop all the flags equal to 1???
	* angrist and evans don't use observations with flagged or allocated values
	keep if (qsex1 == 0 & /// 
			 qsex2 == 0 & ///
			 qage1 == 0 & /// 
			 qage2 == 0 & ///
			 qbirthmo1 == 0 & qbirthmo2 == 0) //& qage1 == 0 & & qage2 == 0 &)
	* if I don't use qbirthmo, I get too high of an estimate?

	ren momloc pernum // rename for merging later
	isid serial pernum

	* file unique on serial and momloc
	tempfile 	momloc_data
	save 		"`momloc_data'", replace
restore 

* merge back onto the original data fileusing serial and pernum
merge 1:1 serial pernum using "`momloc_data'"

keep if _merge == 3
drop 	_merge

*---------------------------------------------------------------------------------------------------
* PRODUCE CONSTRUCTS
*---------------------------------------------------------------------------------------------------
isid serial pernum // check uniquenesss

* sex-mix estimation strategy is implemented using information
* on labor-supply, sex of mothers' first two children, and an
* indicator of multiple births

* generate indicator for mother
tablist 	sex, sort(v) ab(30) clean
levelsof 	sex 

* generate indicators for number of children
* create match variable
clonevar 	chborn_orig = chborn
replace  	chborn = chborn - 1

tablist chborn chborn_orig, sort(v) ab(30) clean
gen cnum_2 = (chborn == 2) 	if !mi(chborn)
gen cnum_mt2 = 	chborn > 2	if !mi(chborn)
gen cnum_mte2 = chborn >= 2	if !mi(chborn)

tablist chborn cnum_2 cnum_mt2 cnum_mte2, sort(v) ab(30) clean

label variable cnum_2 		"Mother had two children"
label variable cnum_mt2 	"Mother had more than two children"
label variable cnum_mte2 	"Mother had more than or equal to two children"

* women aged 21-25 and aged 36-50
gen age_21_35 = inrange(age, 21, 35)
gen age_36_50 = inrange(age, 36, 50)

tablist age_21_35 age_36_50 age, sort(v) ab(30) clean

* we attached people in a household labeled as "child" in the primary relationship
* code to a female householder or the spouse of a male householder
gen ischild = momloc > 0 & !mi(momloc)

* we deleted any mother for whom the number of children in the household did not match
* the number of children ever born
assert chborn_orig != 0 // not in universe

replace chborn = 9 if inrange(chborn, 9, 12) // need to fix, coding does not line up
 
count 	if nchild != chborn
drop 	if nchild != chborn

* this is not true
*bysort serial: assert _N == 1

* all women with two or more children
* compare accuracy of construct produced with chborn
egen 	count_child = rownonmiss(sex? sex??)
gen 	cnum_mte2_test = count_child >= 2
tablist cnum_mte2 cnum_mte2_test, sort(v) ab(30) clean // compare accuracy of variable

* the sample is limited to mothers aged 21-35
* whose oldest child was less than 18 years of age at the time of the census
* except for women who's second child is less than 1 year old
egen age_oldest = 	rowmax(age? age??)
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
gen age_fbirth = (age_qtr - age_oldest_qtr)/4
label variable age_fbirth "Age at first birth"

* labor variables
* worked for pay (= 1 if worked for pay in year prior to census)
assert !mi(wkswork1)
assert inrange(wkswork1, 0, 52)
* recode us80a_wkswork1 (0=0 "Didnt work") (1/52=1 "Worked"), gen(wrkdlastyr)
gen 	wkpay_lyr = (wkswork1 > 0)
tablist wkpay_lyr 	 wkswork1, sort(v) ab(30) clean

* weeks worked (already in data)

* labor income (labor earnings in year prior to census, in 1995 dollars)
* incwage

* family income (family income in year prior to census, in 1995 dollars)
* ftotinc

* non-wife income (family income minus wife's labor income, in 1995 dollars)
gen nonwinc = ftotinc - incwage

*---------------------------------------------------------------------------------------------------
* FILTER FOR SAMPLE 1 AND SAMPLE 2
*---------------------------------------------------------------------------------------------------
keep if age_21_35 == 1 & age_oldest < 18 & cnum_mte2 == 1 & age2 >= 1

* the first includes all women with two kids or more children
* age of first child must be greater than or equal to 1
* age of oldest less than 18
count
tempfile 	sample1 
save 		"`sample1'", replace

preserve 
	* the second includes only married women
	* coules who were married at the time of the Census,
	* married only once
	* and married at the time of their first birth
	* length married = age - age married
	* length married >= age of first child
	gen  	len_married = (age - agemarr)
	*gen 	marr_fbirth = (len_married > age_oldest)
	gen agemarr_qtr = (agemarr*4) + marrqtr

	* gte
	gen  	marr_fbirth = (age_qtr - age_oldest_qtr) >= agemarr_qtr
	label variable marr_fbirth "Married at first birth"
	*tablist marr_fbirth 	age 	age_oldest, sort(v) ab(30) clean
	tablist marr_fbirth 	age_qtr 	age_oldest_qtr, sort(v) ab(30) clean

	tablist marst, sort(v) ab(30) clean
	keep if inlist(marst, 1, 2) & marrno == 1 & marr_fbirth == 1
	assert at leat mroe than 2 kids
	count

	tempfile 	sample2
	save 		"`sample2'", replace
restore 

*---------------------------------------------------------------------------------------------------
* DESCRIPTIVES OF CONSTRUCTS
*---------------------------------------------------------------------------------------------------
tempname memhold 
tempfile descriptives

postfile `memhold' str30(varname sample) str180(varlab) double(N min p5 p25 median mean p75 p95 max) ///
	using `descriptives', replace

forvalues i = 1/2 { // loop for sample version (1 = all, 2 = married)
	preserve 
		use "`sample`i''", clear // load data file for sample

		local addvar 
		if `i' == 2 {
			local addvar marr_fbirth
		}

		* constructs to perform descriptives on
		qui ds chborn cnum_mt2 f_boy s_boy twoboys twogirls samesex twins age_fbirth age_oldest age1 age2 `addvar'
		foreach v in `r(varlist)' { // iterate through the relevant variable list
			local lab : variable label `v'
			* check if numeric
			cap confirm numeric variable `v'
			if _rc == 0 {
				qui sum `v', d 
				post `memhold' 	("`v'") 	("sample`i'") 	("`lab'") 	(r(N)) 		///
								(r(min)) 	(r(p5)) 		(r(p25)) 	(r(p50)) 	///
								(r(mean)) 	(r(p75))		(r(p95)) 	(r(max))
			}
			else {
				di "`v' is string, output suppressed"
				post `memhold' ("`v'") ("analysis") ("`lab'") (.) (.) (.) (.) (.) (.) (.) (.) (.)
			}
		}
	restore 
}
postclose `memhold'

preserve
	use "`descriptives'", clear

	list varname sample N min p5 p25 median mean p75 p95 max
	export excel using "${out}/descriptives.xlsx", replace firstrow(variables) keepcellfmt
restore 

*---------------------------------------------------------------------------------------------------
* TABLE 2
*---------------------------------------------------------------------------------------------------
tempname memhold 
tempfile tbl2

postfile `memhold' str30(varname sample) double(mean N) ///
	using `tbl2', replace

forvalues s = 1/2 {
	preserve 
		use "`sample`s''", clear

		local s_lab = cond(`s' == 1, "all mothers", "married mothers")

		foreach var in 	chborn 		cnum_mt2 	f_boy 	s_boy 	twoboys twogirls samesex twins age age_fbirth wkpay_lyr ///
						wkswork1 	incwage	 	ftotinc nonwinc {

			sum `var'
			post `memhold' ("`var'") ("`s_lab'") (r(mean)) (r(N))
		}
	restore 
}

postclose `memhold'

preserve
	use "`tbl2'", clear

	export excel using "${out}/table_results.xlsx", sheet("table 2") sheetreplace firstrow(variables) keepcellfmt
restore 

* recoding educaition variables

* all married: 393758
* married: 253024
* husbands: married, present; married, absent
* married sample: 

*---------------------------------------------------------------------------------------------------
* TABLE 6 - OLS estimates
*---------------------------------------------------------------------------------------------------
* Other covariates are: age, age at first birth, black, hispanic, and other race
* Boy 2nd
* (1)
use "`sample1'", clear
reg cnum_mt2 samesex, r 
outreg2 using "${out}/table_2", keep(samesex) excel ctitle(OLS - no covar) replace

reg cnum_mt2 f_boy s_boy samesex 						age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy samesex) excel ctitle(OLS) append

reg cnum_mt2 f_boy 					twoboys twogirls 	age age_fbirth r_black hisp r_oth, r
outreg2 using "${out}/table_2", keep(f_boy s_boy twoboys twogirls) excel ctitle(OLS) append

use "`sample2'", clear

*---------------------------------------------------------------------------------------------------
* TABLE 7
*---------------------------------------------------------------------------------------------------

import sas "C:\Users\Jennah\iCloudDrive\Desktop\Wagner\AEM\Replication\AngEv98\AngEv98\m_d_806.sas7bdat", clear


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

