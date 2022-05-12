set more off
clear
capture log close

log using "C:\Users\gulle\Downloads\Thesis Stata\SeniorThesisFinal.log", replace

cd "C:\Users\gulle\Downloads\Thesis Stata"
use "C:\Users\gulle\Downloads\Thesis Stata\usa_00008.dta"

*IPUMS Data Cleaning*
replace hhwt      = hhwt      / 100
replace perwt     = perwt     / 100

format serial    %8.0f
format cbserial  %13.0f
format hhwt      %10.2f
format cluster   %13.0f
format strata    %12.0f
format perwt     %10.2f

label var year      `"Census year"'
label var sample    `"IPUMS sample identifier"'
label var serial    `"Household serial number"'
label var cbserial  `"Original Census Bureau household serial number"'
label var hhwt      `"Household weight"'
label var cluster   `"Household cluster for variance estimation"'
label var met2013   `"Metropolitan area (2013 OMB delineations)"'
label var city   	`"City"'
label var cityerr   `"Coverage Error in City"'
label var citypop   `"City Population"'
label var strata    `"Household strata for variance estimation"'
label var gq        `"Group quarters status"'
label var ownershp  `"Ownership Status of Dwelling (Tenure)"'
label var ownershpd `"Ownership Status of Dwelling (Tenure) (Detailed)"'
label var mortgage   `"Mortgage Status"'
label var proptx99   `"Annual Property Taxes"'
label var rent      `"Monthly contract rent"'
label var valueh   `"House Value"'
label var coupletype   `"Householder Couple Type"'
label var bedrooms  `"Number of bedrooms"'
label var pernum    `"Person number in sample unit"'
label var perwt     `"Person weight"'
label var nchild    `"Number of own children in the household"'
label var nchlt5   `"Number of own children in household under 5"'
label var age   	`"Age"'
label var hispan   `"Hispanic Origin"'
label var marst     `"Marital status"'
label var race      `"Race [general version]"'
label var raced     `"Race [detailed version]"'
label var educ      `"Educational attainment [general version]"'
label var educd     `"Educational attainment [detailed version]"'
label var incwage   `"Wage and salary income"'
label var incwelfr  `"Welfare (public assistance) income"'

*Cleaning

drop if age < 18
drop if age > 65
describe age
summarize age

drop if met2013 == 00000
summarize met2013

drop if countyfip == 0

*Merging Wharton & ACS*
use "C:\Users\gulle\Downloads\WRLURI_01_15_2020 (2).dta", clear
sort cbsacode18
collapse (mean) LPPI18  CII18  LPAI18  LZAI18  LAI18  SRI18  DRI18  OSI18  EI18  AHI18 ADI18 WRLURI18,  by (cbsacode18)

save "C:\Users\gulle\Downloads\WRLURI_01_15_2020Collapsed.dta", replace

keep if cbsacode18 != .

use "C:\Users\gulle\Downloads\Thesis Stata\usa_00008.dta", clear
rename met2013 cbsacode18, replace

sort cbsacode18

describe cbsacode18
summarize cbsacode18

merge m:1 cbsacode18 using "C:\Users\gulle\Downloads\WRLURI_01_15_2020Collapsed.dta"

sum WRLURI18, detail
**Note to self: EI18 is a Dummy Variable 
**ADI18 is average time lag in months

*Post-merge data cleaning
**Dropping missing info**
keep if WRLURI18 != .
drop if age < 18
drop if age > 50
replace rent=. if rent==0
**Cleaning unmatched data
keep if sample != .
keep if countyfip != 0

**Dropping anyone with less than HS <- explanation: potentially removing cases of teen pregnancy**
describe educ
summarize educ, detail
keep if educ > 4
summarize educ, detail
reg incwage I.educd

*Clean valueh
replace valueh = . if valueh >= 9999997 | valueh == 0 

tab rent 
summarize rent
tab race
tab educ
tab hispan
ssc install outreg2

*Generating summary tables
summarize 
*tabout using summarystats.xls, cell(row col freq) format(2p 2p 0c) clab() layout(row) ptotal(single) replace
sum rent if rent > 0

*First stage relevance
reg valueh WRLURI18 i.race i.educ i.hispan incwage i.statefip, robust beta
outreg2 using ols_valueh1, word
reg rent WRLURI18 i.race i.educ i.hispan incwage i.statefip, robust beta
outreg2 using ols_rent1, word


*Home Value

reg nchild valueh i.race i.educ i.hispan incwage i.statefip, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage i.statefip if valueh != 0, first robust
outreg2 using results_valueh, word
estat first
estat endog
estat overid

	*Beta testing *BE SURE TO UPDATE THESE AS YOU CHANGE MODEL
	sum nchild age rent valueh if e(sample) == 1
	di  520283.3 * -3.17e-07
	di -.16492981/ 1.201161
*FOR THIS PART, I WOULD MAKE A TABLE THAT HAS JUST THE SECOND STAGE COEFFICIENT AND ALSO THE FIRST STAGE F-STATISTIC, WHICH YOU CAN GET FROM SQUARING THE T-STATISTIC OF THE INSTRUMENT IN THE FIRST STAGE.
ivregress 2sls nchild (valueh = LPPI18) i.race i.educ i.hispan incwage i.statefip, first robust
estat first
sum nchild age rent valueh if e(sample) == 1 & age < 50
di  517480.8* -4.94e-07
di -.25563552/1.205055
ivregress 2sls nchild (valueh = CII18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-6.23e-07
di -.32239054/1.205055
estat first
ivregress 2sls nchild (valueh = LPAI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-.0000156
di -8.0727005/1.205055
estat first
ivregress 2sls nchild (valueh = LZAI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-2.64e-07
di -.13661493/1.205055
estat first
ivregress 2sls nchild (valueh = LAI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*2.72e-07
di .14075478/1.205055
estat first
ivregress 2sls nchild (valueh = SRI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*9.89e-07 
di .51178851/1.205055
estat first
ivregress 2sls nchild (valueh = DRI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-3.01e-07 
di -.15576172/1.205055
estat first
ivregress 2sls nchild (valueh = OSI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-7.62e-07
di -.39432037/1.205055
estat first
ivregress 2sls nchild (valueh = EI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-3.17e-07
di -.16404141/1.205055
estat first
ivregress 2sls nchild (valueh = AHI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-2.62e-07
di -.13557997/1.205055
estat first
ivregress 2sls nchild (valueh = ADI18) i.race i.educ i.hispan incwage i.statefip, first robust
sum nchild age rent valueh if e(sample) == 1 & age < 50
di 517480.8*-3.22e-07
di -.16662882/1.205055
estat first

*Rent
reg nchild rent i.race i.educ i.hispan incwage i.statefip if rent != 0, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust
outreg2 using results_rent, word

estat first
estat endog
estat overid
	*Beta testing
	sum nchild rent valueh if e(sample) == 1 & age < 50
	di  720.8524 *  -.0002327
	di -.16774235/ 1.111117 
ivregress 2sls nchild (rent = LPPI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0003168
di -.22836604/1.111117
estat first
ivregress 2sls nchild (rent = CII18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0001303 
di -.09392707/1.111117
estat first
ivregress 2sls nchild (rent = LPAI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0056915 
di -4.1027314/1.111117
estat first
ivregress 2sls nchild (rent = LZAI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0002816 
di -.20299204/1.111117
estat first
ivregress 2sls nchild (rent = LAI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*.0036334
di 2.6191451/1.111117
estat first
ivregress 2sls nchild (rent = SRI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*.0006348
di .4575971/1.111117
estat first
ivregress 2sls nchild (rent = DRI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0001528
di -.11014625/1.111117
estat first
ivregress 2sls nchild (rent = OSI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.000287 
di -.20688464/1.111117
estat first
ivregress 2sls nchild (rent = EI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0002702
di -.19477432/1.111117
estat first
ivregress 2sls nchild (rent = AHI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0001646
di -.11865231/1.111117
estat first
ivregress 2sls nchild (rent = ADI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust 
sum nchild rent valueh if e(sample) == 1 & age < 50 
di 720.8524*-.0002041
di -.14712597/1.111117
estat first
*Trying low income
drop if incwage == .
generate lowincome = incwage < 64324
tab lowincome
sum incwage
sum lowincome

*Low income
reg nchild rent i.race i.educ i.hispan if rent != 0 & lowincome == 1, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan  if rent != 0 & lowincome == 1, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 & lowincome == 1
di 647.9372* -.0000525
di -.0340167/ 1.131837 
*-.03005442
reg nchild valueh i.race i.educ i.hispan if rent == . & lowincome == 1, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan if rent == . & lowincome == 1, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 & lowincome == 1

di 481390.8 *  -4.03e-07
di -.19400049/1.189083
*-.16315134

*High Income
reg nchild rent i.race i.educ i.hispan if rent != 0 & lowincome == 0, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan if rent != 0 & lowincome == 0, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 & lowincome == 00
di  817.1714 * -.0000603
di -.04927544/1.003064
*-.04912492
reg nchild valueh i.race i.educ i.hispan if lowincome == 0, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan if lowincome == 0, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 & lowincome == 00
di -2.52e-07*579085.3 
di -.1459295/ 1.180101 
*-.12365848
**State specific regressions

*Alabama <- lowest housing costs (WPI )
*Renters
reg nchild rent i.race i.educ i.hispan incwage if rent != 0 & statefip == 01, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage if rent != 0 & statefip == 01, first robust
sum nchild rent valueh if e(sample) == 1 & age < 50 & statefip == 01

estat first
estat endog
estat overid

*Beta testing
sum nchild rent valueh if e(sample) == 1 & age < 50 & statefip == 01
di 394.5787 * -3.24e-06 
di -.00127843/1.077109

*Homeowners
reg nchild valueh i.race i.educ i.hispan incwage if rent == 0 & statefip == 01, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage if rent == 0 & statefip == 01, first robust
estat first
estat endog
estat overid

*Beta testing
sum nchild rent valueh if e(sample) == 1 & age < 50 & statefip == 01
di  263476.5 *  -7.43e-07
di -.19576304/1.157685

*California <- highest housing costs
*Renters
reg nchild rent i.race i.educ i.hispan incwage if rent != 0 & statefip == 06, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage if rent != 0 & statefip == 06, first robust
estat first
estat endog
estat overid

*Beta testing
sum nchild rent valueh if e(sample) == 1 & age < 50 & statefip == 06
di 781.2192 * -.0003125
di -.244131/ 1.134098


*Homeowners
reg nchild valueh i.race i.educ i.hispan incwage if rent == 0 & statefip == 06, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 LAI18 SRI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage if rent == 0 & statefip == 06, first robust
estat first
estat endog
estat overid

*Beta testing
sum nchild valueh if e(sample) == 1 & age < 50 & statefip == 06
di 774158.6 *  -4.28e-07
di -.33133988/1.156781

**HValue TEST
reg nchild valueh i.race i.educ i.hispan incwage i.statefip, robust beta
ivregress 2sls nchild (valueh = LPPI18 CII18 LPAI18 LZAI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage i.statefip if valueh != 0, first robust
estat first
estat endog
estat overid

sum nchild age rent valueh if e(sample) == 1 & age < 50
di 520283.3 * -3.06e-07
di -.15920669/1.201161

**RENT TEST
reg nchild rent i.race i.educ i.hispan incwage i.statefip if rent != 0, robust beta
ivregress 2sls nchild (rent = LPPI18 CII18 LPAI18 LZAI18 DRI18 OSI18 EI18 AHI18 ADI18) i.race i.educ i.hispan incwage i.statefip if rent != 0, first robust

estat first
estat endog
estat overid

sum nchild rent valueh if e(sample) == 1 & age < 50
di 720.8524 * -.0002124
di -.15310905/1.111117 
log close