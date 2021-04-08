*Transofrm variables

rename age Age
rename area Area
rename balance Balance
rename broker Broker
rename del Delinquency
rename fico FICO
rename gender Gender
rename hard Hard
rename income Income
rename ltv LTV
rename rate Rate
rename refinance Refinance
rename white White

*Summary statistics

summarize Age-White

*validity check 1
twoway (scatter mean_rate FICO)
graph2tex, epsfile(FICO)
twoway (scatter mean_rate FICO>630&FICO<650)
graph2tex, epsfile(FICO1)
twoway (scatter mean_rate FICO>650&FICO<670)
graph2tex, epsfile(FICO2) 

*validity check 2
bysort FICO: egen mean_balance = mean(Balance)
bysort FICO: egen mean_broker = mean(Broker)
bysort FICO: egen mean_gender = mean(Gender)
bysort FICO: egen mean_hard = mean(Hard)
bysort FICO: egen mean_income = mean(Income)
bysort FICO: egen mean_LTV = mean(LTV)
bysort FICO: egen mean_rate = mean(Rate)
bysort FICO: egen mean_refinance = mean(Refinance)
bysort FICO: egen mean_white = mean(White)
bysort FICO: egen mean_age = mean(Age)
bysort FICO: egen mean_delinquency = mean(Delinquency)
bysort FICO: egen mean_area = mean(Area)
 
twoway (scatter mean_balance FICO)
graph2tex, epsfile(Balance)
twoway (scatter mean_broker FICO)
graph2tex, epsfile(Broker)
twoway (scatter mean_gender FICO)
graph2tex, epsfile(Gender)
twoway (scatter mean_hard FICO)
graph2tex, epsfile(Hard)
twoway (scatter mean_income FICO)
graph2tex, epsfile(Income)
twoway (scatter mean_LTV FICO)
graph2tex, epsfile(LTV)
twoway (scatter mean_refinance FICO)
graph2tex, epsfile(Refinance)
twoway (scatter mean_white FICO)
graph2tex, epsfile(White)
twoway (scatter mean_age FICO)
graph2tex, epsfile(Age)
twoway (scatter mean_area FICO)
graph2tex, epsfile(Area)

cumul FICO, gen (FICO_emp)
twoway (line FICO FICO_emp , lcolor(blue)), ytitle(" ") xtitle(" FICO") ytitle(" FICO Density") title("Empirical distribution of FICO")
graph2tex, epsfile(FICO_emp)

*validity check 3
twoway (scatter mean_income FICO) if mean_income>300

*validity check 4
cumul FICO, gen (FICO_emp)
twoway (line FICO FICO_emp , lcolor(blue)), ytitle(" ") xtitle(" FICO") ytitle(" FICO Density") title("Empirical distribution of FICO")
graph2tex, epsfile(FICO_emp)

*specification check  
histogram Rate
graph2tex, epsfile(hyst_Rate)
histogram Income
graph2tex, epsfile(hyst_Income)
histogram Age
graph2tex, epsfile(hyst_Age)
histogram Area
graph2tex, epsfile(hyst_Area)
histogram Balance
graph2tex, epsfile(hyst_Balance)
histogram LTV
graph2tex, epsfile(hyst_LTV)

sktest LTV

*Parametric regressions
gen lnLTV=ln(LTV)
drop if mean_income> 1000
drop if mean_LTV>78

generate w = 0 
replace w = 1 if FICO>=620
replace w = . if missing(FICO)

gen FICO1= FICO-620
gen FICO2= (FICO-620)^2
gen inter1= w*( FICO-620)
gen inter2= w*( FICO-620)^2

*regressions without controls:
reg Rate FICO w, vce (cluster Area)
outreg2 using param1.tex, replace ctitle (RD1)
predict residuals1, residuals
gen int1=(1-w)*residuals1
logit Delinquency FICO Rate int1 residuals1, vce(cluster Area)
outreg2 using param1.tex, append ctitle (Logit1)

reg Rate w FICO1 inter1 , vce (cluster Area)
outreg2 using param1.tex, append ctitle (RD2)
predict residuals2, residuals
gen int2=(1-w)*residuals2
logit Delinquency FICO Rate int2 residuals2, vce(cluster Area)
outreg2 using param1.tex, append ctitle (Logit2)

reg Rate w FICO1 inter1 inter2 , vce (cluster Area)
outreg2 using param1.tex, append ctitle (RD3)
predict residuals3, residuals
gen int3=(1-w)*residuals3
logit Delinquency FICO Rate int3 residuals3, vce(cluster Area)
outreg2 using param1.tex, append ctitle (Logit3)

*regressions with controls
reg Rate FICO w Age Balance Broker Gender Hard Income LTV Refinance White , vce (cluster Area)
outreg2 using param2.tex, replace ctitle (RD1)
reg Rate FICO w Age Balance Gender  Income LTV Refinance White , vce (cluster Area)
outreg2 using param2.tex, append ctitle (RR2)
predict residuals4, residuals
gen int4=(1-w)*residuals4
logit Delinquency Rate FICO w Age Balance Gender  Income LTV Refinance White int4, vce (cluster Area)
outreg2 using param2.tex, append ctitle (Logit2)

