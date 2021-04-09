clear all
set more off
cd ~/Desktop
use "IND_2010_2015_HHconstr_16_24.dta", clear

global file labor_table_dummy
scalar Print = 0
/*change this scalar to 1 if you want to see the output*/


*xtset idind id_w
foreach x of varlist year-tpensnr{
qui replace `x'= . if `x'>99999990
}

* Transform the variables

**Wage
qui gen wage=j13_2
qui replace wage=ln(wage)
qui drop if wage>11.15625 
**Sex
qui gen sex=.
qui replace sex=2-h5
**Age
qui gen age2=age^2
**Tenure
qui gen tenure=j73_3y+j73_3m/12
qui replace tenure=. if tenure>100
qui gen tenure2=tenure^2
**Marital status
qui gen marstatus=.
qui replace marstatus=1 if marst==2 | marst==3
qui replace marstatus=0 if marst==1 | marst==4 | marst==5 | marst==6
**Workweek
qui gen workweek=j6_2
qui drop if work>130
**Size of the company
qui gen size=j13
qui gen lnsize=log(j13)
** Education
qui gen ed0=.
qui gen ed1=.
qui gen ed2=.
qui gen ed3=.
global ed ed0-ed3
foreach x of varlist $ed{
qui replace `x'=0 if !missing(educ)
}
qui replace ed0=1 if educ<=13
qui replace ed1=1 if educ==14
qui replace ed2=1 if (educ>=15)&(educ<=19)
qui replace ed3=1 if educ>=20
global ed ed1-ed3

**Public & Private
qui gen industry=j4_1
qui gen state=j23
qui replace state=2-state
qui gen public=0
qui replace public=1 if state==1 & industry>8 & industry<13 
**Shortweek
qui gen shortweek=0
qui replace shortweek=1 if workweek<=36
**Dropping criteria
qui drop if age<15 | age>72
qui drop if industry==8
**Second job
qui gen sjob = 2-j32
**Subord
qui gen sub0=j6-1
qui gen sub1=.
qui replace sub1=1 if j6==1 & j6_0<=5
qui replace sub1=0 if !(j6==1 & j6_0<=5)
qui gen sub2=.
qui replace sub2=1 if j6==1 & (j6_0>5 & j6_0<=13)
qui replace sub2=0 if !(j6==1 & (j6_0>5 & j6_0<=13))
qui gen sub3=.
qui replace sub3=1 if j6==1 & (j6_0>13 & j6_0<=40)
qui replace sub3=0 if !(j6==1 & (j6_0>13 & j6_0<=40))
qui gen sub4=.
qui replace sub4=1 if j6==1 & (j6_0>40 & j6_0<=200)
qui replace sub4=0 if !(j6==1 & (j6_0>40 & j6_0<=200))
qui drop if j6==1 &j6_0>200
global sub sub1-sub4
**Child
qui gen kids=j72_171
qui replace kids=1 if kids==1
qui replace kids=0 if kids==2


qui drop j1-tid

qui gen publicsex = public*sex
 
**qui keep year wage sex age2 marstatus workweek size peduc industry state public age 
**Summary statistics
*bysort public: sum(wage sex tenure marstatus educ workweek size shortweek kids)
*bysort public: outreg2 using x.doc, replace sum(log) eqkeep(N mean sd)


*Prorensity scores

**trimming by propencity score
xi: qui probit public sex age marstatus workweek lnsize sjob $ed $sub I.year, cluster(region)
qui predict p_psc
** Bit, where we check optimal trimming conditions (from Imbens and Wooldridge, 2009)
**the two displayed numbers should be almost equal
scalar thr = 0.08
/*display (1/(thr*(1-thr)))/2
capture qui drop ieiie
gen ieiie = 1/(p_psc*(1-p_psc))
replace ieiie = . if ieiie>=1/(thr*(1-thr))
sum ieiie*/
gen S_Sample=1
qui replace S_Sample=0 if p_psc<thr|p_psc>(1-thr)

**RE-generating propencity score
qui capture drop p_psc
xi: qui probit public sex age age2 marstatus workweek lnsize sjob $ed $sub I.year if S_Sample, cluster(region)
qui predict p_psc 
qui gen weights=0
qui replace weights=1/p_psc  if S_Sample&public
qui replace weights=1/(1-p_psc) if S_Sample&!public

xi: cgmreg wage public sex I.year if S_Sample [aw=weights], cluster(region industry)
if Print outreg2 using $file, tex addtext (cov.1, -, cov.2, -) append
xi: reg wage public sex publicsex I.year if S_Sample [aw=weights], cluster(industry)
if Print outreg2 using $file, tex addtext (cov.1, -, cov.2, -) append

xi: reg wage public sex age $ed I.year if S_Sample [aw=weights], cluster(industry)
if Print outreg2 using $file, tex addtext (cov.1, +, cov.2, -) append
xi: reg wage public sex publicsex age $ed I.year if S_Sample [aw=weights], cluster(industry)
if Print outreg2 using $file, tex addtext (cov.1, +, cov.2, -) append

xi: reg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], cluster(industry)
if Print outreg2 using $file, tex addtext (cov.1, +, cov.2, +) append
xi: reg wage public sex publicsex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], cluster(industry)
if Print outreg2 using $file, tex addtext (cov.1, +, cov.2, +) append
/*We used only one-way clustering instead of two-way somewhere,
 because two-way refused to produce standard errors for "public"*/

local str sex age marstatus workweek lnsize sjob $ed $sub 
xi: movestay (wage=`str' I.year), select(public = `str' I.year kids shortweek)
if Print outreg2 using labor_switching, tex append
forvalues i = 1/2{
	forvalues j = 1/2{
		mspredict yc`i'_`j', yc`i'_`j'
		qui replace yc`i'_`j'=exp(yc`i'_`j')
		}
	}
qui gen mill = .
qui replace mill = (yc1_1-yc2_1)/((yc1_1+yc2_1)/2) if !missing(yc1_1)&!missing(yc2_1)
qui replace mill = (yc1_2-yc2_2)/((yc1_2+yc2_2)/2) if !missing(yc1_2)&!missing(yc2_2)
sum mill

** Quantile regression stuff
** Table
xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(.10)
if Print outreg2 using labor_quantile, tex addtext (quantile, 10%) append
xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(.25)
if Print outreg2 using labor_quantile, tex addtext (quantile, 25%) append
xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(.50)
if Print outreg2 using labor_quantile, tex addtext (quantile, 50%) append
xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(.75)
if Print outreg2 using labor_quantile, tex addtext (quantile, 75%) append
xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(.90)
if Print outreg2 using labor_quantile, tex addtext (quantile, 90%) append
** Graph
mat B=(0)
mat SE=(0)
mat q=(0)
local S=0.05  //change this paremeter to adjust presision/performance time
local end=1-`S'
forvalues i = `S'(`S')`end'{
	xi: qreg wage public sex age marstatus workweek lnsize sjob $ed $sub I.year if S_Sample [aw=weights], quantile(`i')
	mat	me=e(b)
	mat me=me[1,1]
	mat B=B\me
	mat V=e(V)
	mat V=(V[1,1])^(0.5)
	mat SE=SE\V
	scalar ii=`i'
	mat q=q\ii
	}
mat Aggregate=q,B,SE
qui capture mat drop q B SE
scalar R = rowsof(Aggregate)
mat Aggregate = Aggregate[2..R,1...]
if Print putexcel A1=matrix(Aggregate) using "labor_quantile.xls", sheet(`S', replace) colwise modify
mat list Aggregate
