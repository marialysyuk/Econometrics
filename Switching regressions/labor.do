clear all
set more off
use "C:\Users\Dangerous\Desktop\IND_2010_2015_HHconstr_16_24.dta", clear
**Wage
gen wage=j13_2
replace wage=. if wage>99999990
replace wage=ln(wage)
drop if wage>11.15625 
**Sex
gen sex=.
replace sex=1 if h5==1
replace sex=0 if h5==2
**Age
replace age=. if age>99999990
gen age2=age^2
**Tenure
gen tenure=j73_3y+j73_3m/12
replace tenure=. if tenure>100
**Marital status
gen marstatus=.
replace marstatus=1 if marst==2 | marst==3 | marst==6
replace marstatus=0 if marst==1 | marst==4 | marst==5 | marst==7
**Education
replace educ=. if educ>99999990
**Occupation
replace occup08=. if occup08>99999990
**Workweek
gen workweek=j6_2
replace workweek=. if workweek>99999990
drop if work>130
**Size of the company
gen size=j13
replace size=. if size>99999990
**Parent education
gen peduc=j217a
replace peduc=. if peduc>99999990
**Public & Private
gen industry=j4_1
replace industry=. if indust>9999990
gen state=j23
replace state=. if state>9999990
replace state=1 if state==1
replace state=0 if state==2
gen public=0
replace public=1 if state==1 & industry>8 & industry<13 
**Child
gen kids=j72_171
replace kids=. if kids>9999990
replace kids=1 if kids==1
replace kids=0 if kids==2
**Shortweek
gen shortweek=0
replace shortweek=1 if workweek<=36
drop if age<15 | age>72
 
**keep year wage sex age2  tenure marstatus educ occup08 workweek size  peduc industry state public age
**Summary statistics
bysort sex: sum(wage sex tenure marstatus educ peduc workweek size shortweek kids)
*bysort public: outreg2 using x.doc, replace sum(log) eqkeep(N mean sd)


