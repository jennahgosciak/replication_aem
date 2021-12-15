	/*
gen age_fbirth = (age_qtr - age_oldest_qtr)
label variable age_fbirth "Age at first birth"

gen age_fbirth_father = (age_qtr_father - age_oldest_qtr)
label variable age_fbirth_father "Age at first birth (father"
*/

	* length married = age - age married
	* length married >= age of first child
	*gen  	len_married = (age - agemarr)
	*gen 	marr_fbirth = (len_married >= age_oldest)
	*gen 	marr_fbirth = (age - age_oldest) >= agemarr

	*gen marrfrac = 1 - (marrqtr/4)
	*gen agemarr_qtr = agemarr + marrfrac

	* gte
	*gen  	marr_fbirth = (age_qtr - age_oldest_qtr) >= agemarr_qtr
	


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
