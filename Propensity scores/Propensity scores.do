 **1
*Provide summary statistics for treatment and control groups separately

sort own
summarize Llnsales-FsalesGR if own==0
summarize Llnsales-FsalesGR if own==1

*Provide normalized differences

egen SD_Llnsales = sd(Llnsales), by (own)
egen SD_Lexport  = sd(Lexport), by (own)
egen SD_Lavwage  = sd(Lavwage), by (own)
egen SD_Linnovation   = sd(Linnovation ), by (own)
egen SD_Llnkapital    = sd(Llnkapital  ), by (own)
egen SD_LsalesGR   = sd(LsalesGR), by (own)
egen SD_FsalesGR   = sd(FsalesGR ), by (own)
egen mean_Llnsales = mean (Llnsales), by(own)
egen mean_Lexport  = mean (Lexport ), by(own)
egen mean_Lavwage  = mean (Lavwage ), by(own)
egen mean_Linnovation   = mean (Linnovation ), by(own)
egen mean_Llnkapital  = mean ( Llnkapital  ), by(own)
egen mean_LsalesGR  = mean (LsalesGR ), by(own)
egen mean_FsalesGR = mean ( FsalesGR ), by(own)
gen nd_Llnsales = (mean_Llnsales[5142] - mean_Llnsales[1])/sqrt((SD_Llnsales[5142]^2)+(SD_Llnsales[1]^2))
gen nd_Lexport = (mean_Lexport[5142] - mean_Lexport[1])/sqrt((SD_Lexport[5142]^2)+(SD_Lexport[1]^2))
gen nd_Lavwage = (mean_Lavwage[5142] - mean_Lavwage[1])/sqrt((SD_Lavwage[5142]^2)+(SD_Lavwage[1]^2))
gen nd_Linnovation  = (mean_Linnovation [5142] - mean_Linnovation [1])/sqrt((SD_Linnovation [5142]^2)+(SD_Linnovation [1]^2))
gen nd_Llnkapital = (mean_Llnkapital[5142] - mean_Llnkapital[1])/sqrt((SD_Llnkapital[5142]^2)+(SD_Llnkapital[1]^2))
gen nd_LsalesGR = (mean_LsalesGR[5142] - mean_LsalesGR[1])/sqrt((SD_LsalesGR[5142]^2)+(SD_LsalesGR[1]^2))

*Graphical analysis
cumul Llnsales if own==1, gen (cum_Llnsales_1 )
cumul Llnsales if own==0, gen (cum_Llnsales_0 )
twoway (line cum_Llnsales_1 Llnsales if own==1, lcolor(blue)) ( line cum_Llnsales_0 Llnsales if own==0, lcolor(maroon)  legend(order( 1 "Treated" 2 "Control" ))), ytitle(" ") xtitle(" Llnsales") title("Treated versus control Llnsales")
graph2tex, epsfile(Llnsales_whole)

cumul Llnsales if own==1& subsample==1, gen (cum_Llnsales_11 )
cumul Llnsales if own==0& subsample==1, gen (cum_Llnsales_00 )
twoway (line cum_Llnsales_11 Llnsales if own==1&subsample==1, lcolor(blue)) ( line cum_Llnsales_00 Llnsales if own==0&subsample==1, lcolor(maroon)  legend(order( 1 "Treated" 2 "Control" ))), ytitle(" ") xtitle(" Llnsales") title("Treated versus control Llnsales")
graph2tex, epsfile(Llnsales_subsample)

**2
global ylist FsalesGR
global xlist Llnsales-LsalesGR

scalar Threshold=0.01


gen  Llnsaleswage= Llnsales* Lavwage
gen  LavwagesalesGR =  Lavwage* LsalesGR
gen  Llnsalessq=Llnsales^2
*gen Llnsaleswagesq=Llnsaleswage^2
global xcontrols Llnsaleswage-Llnsalessq

gen Llnsales_own=Llnsales*own
gen Lexport_own=Lexport*own
gen Lavwage_own=Lavwage*own
gen Linnovation_own=Linnovation*own
gen Llnkapital_own=Llnkapital*own
gen LsalesGR_own=LsalesGR*own
global xown Llnsales_own-LsalesGR_own

*Estimation of the propensity score

pscore own $xlist $xcontrols, pscore (Bpsc_own) blockid(new) det
*pscore own $xlist $xcontrols, logit pscore (Bpsc_own) blockid(new) det

*Graphs away
hist Bpsc_own if own==1
graph2tex, epsfile(Probit_own1)
hist Bpsc_own if own==0
graph2tex, epsfile(Probit_own0)

*hist Bpsc_own if own==1
*graph2tex, epsfile(Logit_own1)
*hist Bpsc_own if own==0
*graph2tex, epsfile(Logit_own0)

**3
*a
reg $ylist own, robust
outreg2 using Basic.tex, replace ctitle(Difference in means)
*b
reg $ylist $xlist own, robust
outreg2 using Basic.tex, append ctitle(OLS1)

reg $ylist $xlist $xown own, robust
outreg2 using Basic.tex, append ctitle(OLS2)

*c1 "Matching techique"

nnmatch $ylist own $xlist, m(2) robust(2) biasadj(bias)

*c2 "Propensity score weighting"

*generate weights
gen _wt = 1/Bpsc_own if own
gen _wc = 1/(1-Bpsc_own) if !own
quiet mean $ylist [iw = _wt] 
matrix mt = e(b)
quiet mean $ylist [iw = _wc] 
matrix mc = e(b)
matrix mdif = mt - mc
matrix list mdif

* c3 "Weighting and regression"
gen _lam = own/Bpsc_own + !own/(1-Bpsc_own) 
reg $ylist own $xlist [iw = _lam]
outreg2 using Basic.tex, append ctitle(Weightening and regression)

*c4:"Blocking and regression":
// blocking & regression
gen _tr = .

//within each block that was defined by pscore
//we are looking for treatment effect.
levelsof new, local(bl)
foreach b of local bl {
	qui reg $ylist own $xlist if new == `b' 
	matrix B = e(b)
	svmat B, names("coef")
	replace _tr = coef1[1] if new== `b'
	drop coef*
}
mean _tr 

**4
*Subsample creation

gen subsample=1
replace subsample=0 if Bpsc_own<Threshold
replace subsample=0 if Bpsc_own>(1-Threshold)

*Summary statistics

sort own
summarize Llnsales-FsalesGR if (own==0)&subsample
summarize Llnsales-FsalesGR if (own==1)&subsample

*Normalized differences
egen SD_LlnsalesSS = sd(Llnsales) if subsample, by (own)
egen SD_LexportSS  = sd(Lexport) if subsample, by (own)
egen SD_LavwageSS  = sd(Lavwage) if subsample, by (own)
egen SD_LinnovationSS   = sd(Linnovation ) if subsample, by (own)
egen SD_LlnkapitalSS    = sd(Llnkapital  ) if subsample, by (own)
egen SD_LsalesGRSS   = sd(LsalesGR) if subsample, by (own)
egen mean_LlnsalesSS = mean (Llnsales) if subsample, by(own)
egen mean_LexportSS  = mean (Lexport ) if subsample, by(own)
egen mean_LavwageSS  = mean (Lavwage ) if subsample, by(own)
egen mean_LinnovationSS   = mean (Linnovation ) if subsample, by(own)
egen mean_LlnkapitalSS  = mean ( Llnkapital  ) if subsample, by(own)
egen mean_LsalesGRSS  = mean (LsalesGR ) if subsample, by(own)

gen nd_LlnsalesSS = (mean_LlnsalesSS[5142] - mean_LlnsalesSS[1])/sqrt((SD_LlnsalesSS[5142]^2)+(SD_LlnsalesSS[1]^2))
gen nd_LexportSS = (mean_LexportSS[5142] - mean_LexportSS[1])/sqrt((SD_LexportSS[5142]^2)+(SD_LexportSS[1]^2))
gen nd_LavwageSS = (mean_LavwageSS[5142] - mean_LavwageSS[1])/sqrt((SD_LavwageSS[5142]^2)+(SD_LavwageSS[1]^2))
gen nd_LinnovationSS  = (mean_LinnovationSS[5142] - mean_LinnovationSS[1])/sqrt((SD_LinnovationSS[5142]^2)+(SD_LinnovationSS[1]^2))
gen nd_LlnkapitalSS = (mean_LlnkapitalSS[5142] - mean_LlnkapitalSS[1])/sqrt((SD_LlnkapitalSS[5142]^2)+(SD_LlnkapitalSS[1]^2))
gen nd_LsalesGRSS = (mean_LsalesGRSS[5142] - mean_LsalesGRSS[1])/sqrt((SD_LsalesGRSS[5142]^2)+(SD_LsalesGRSS[1]^2))


*Remake propensity scores

pscore own $xlist $xcontrols if subsample, pscore (Bpsc_own_sub) blockid(new_sub) det
pscore own $xlist $xcontrols if subsample, logit pscore (Bpsc_own_sub) blockid(new_sub) det

*Graphs away
hist Bpsc_own if own==1&subsample
graph2tex, epsfile(Probit_own1_sub)
hist Bpsc_own if own==0&subsample
graph2tex, epsfile(Probit_own0_sub)

hist Bpsc_own if own==1 &subsample
graph2tex, epsfile(Logit_own1_sub)
hist Bpsc_own if own==0 &subsample
graph2tex, epsfile(Logit_own0_sub)

**3
*a
reg $ylist own if subsample, robust
outreg2 using Basic_sub.tex, replace ctitle(Difference in means)
*b
reg $ylist $xlist own if subsample, robust
outreg2 using Basic_sub.tex, append ctitle(OLS1)

reg $ylist $xlist $xown own if subsample, robust
outreg2 using Basic_sub.tex, append ctitle(OLS3)

*c1 "Matching techique"

nnmatch $ylist own $xlist if subsample, m(4) robust(2) biasadj(bias)

*c2 "Propensity score weighting"

*generate weights
gen _wt = 1/Bpsc_own if own & subsample
gen _wc = 1/(1-Bpsc_own) if !own & subsample
quiet mean $ylist [iw = _wt] 
matrix mt = e(b)
quiet mean $ylist [iw = _wc] 
matrix mc = e(b)
matrix mdif = mt - mc
matrix list mdif

* c3 "Weighting and regression"
gen _lam = own/Bpsc_own + !own/(1-Bpsc_own) 
reg $ylist own $xlist [iw = _lam] if subsample


*c4:"Blocking and regression":
// blocking & regression
gen _tr = .

//within each block that was defined by pscore
//we are looking for treatment effect.
levelsof new, local(bl)
foreach b of local bl {
	qui reg $ylist own $xlist if new == `b'& subsample
	matrix B = e(b)
	svmat B, names("coef")
	replace _tr = coef1[1] if new== `b'
	drop coef*
}
mean _tr 

