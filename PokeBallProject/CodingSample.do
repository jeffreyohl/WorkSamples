* Jeffrey Ohl, 2021, STATA Coding sample


* Detailed output and the assignment task can be found on my GitHub: https://github.com/jeffreyohl/WorkSamples/tree/main/PokeBallProject



* The assignment goal is to determine if PokeBall factories are causing asthma
* in the towns sampled.

*******************************************************************



* First, I'll clean the data to get it into a form usable for analysis.


* Second, I'll explore the data.


* Third, I'll build a  difference in differences regression to see if 
* towns introducing the subsidy law had an increase in asthma
* above the trend we'd expect.


* Fourth, to get a better idea of the pollutive effect of PokeBalls,
* I'll  build a regression to see if higher levels of PokeBall production
* have higher levels of asthma.


*******************************************************************

* Global variables:



// Directories

// The folder containing hospital.csv and city.csv
global input_dir = "input" 
// An initially empty folder, which will contain all datasets
// created from the two original files.
global intermediate_dir = "intermediate"
// An initially empty folder, which will contain all tables and
// charts created.
global output_dir = "output"

* The first year for the data
global startingYear=2000 

* The first year that subsidies took effect
global subsidiesBegin=2007





*******************************************************************

*** Part 1: Data Cleaning ***


* We begin by importing a csv of hospital data and examining it

import delimited ${input_dir}/hospital.csv, clear


list in f/10

* We would like to merge this data with city.csv, but there is no "city" column. 
* It seems, however, that the hospital column  always contains the city's name.

* Specifically, we need to get all but the last word of the
* "hospital" column in order to get its city.

split hospital


* Some hospitals have 3 words, and some have 4, 
* in either case, we want to drop the last word. 

gen city = hospital1 + " " + hospital2  if hospital4 == ""

replace city = hospital1 + " " + hospital2  + " " + hospital3  if hospital4 != ""

* We have no use for the hospital column and its component words now *

drop hospital*


* Since our analysis will be at the city level, 
* we aggregate the dataset to get the total number of cases by city

collapse (sum) cases*, by(city)

save ${intermediate_dir}/hospital_adjusted.dta, replace

* Now we load city data and merge it with the hospital data.
import delimited ${input_dir}/city.csv, clear


merge 1:1 city using ${intermediate_dir}/hospital_adjusted.dta

	
* Remove the "_" character in factory_0, cases_0, production_0, etc. - this
* gets the data into the right format for "reshape"
rename *_* **

* Reformat data into long format, so regressions can be run
reshape long factory cases production, i(city) j(year)

* Recalculate year to be actual calendar year.
replace year = year+$startingYear


* We would expect cities with more people to have a higher raw 
* number of asthmas cases. We normalize
* asthma cases, production and factories to be per capita.

gen perCapitaProduction = production/population

label variable perCapitaProduction "PokeBall Production Per Capita"

gen perCapitaCases = cases/population

label variable perCapitaCases "Asthma Cases Per Capita"

gen perCapitaFactories = factory/population

label variable perCapitaFactories "Factories Per Capita"

* Create numerical column, "cities", corresponding to
* each city and drop old "city" column
egen cities = group(city)

drop city


* Create numerical column corresponding to states
* who enacted laws to subsidize PokeBall factories in 2007, and label accordingly.


egen subsidizingCity = group(lawchange)

* I subtract 1 to ensure values are 0 and 1.
replace subsidizingCity = subsidizingCity-1 
label variable subsidizingCity "City Enacted Subsidies "
drop lawchange

label define Subsidies 0 "Enacted subsidies in 2007" 1 "Never enacted subsides", replace

label values subsidizingCity Subsidies



* Part 2: Data Exploration 


* First, let's check correlations of the several causal variables:
* factories, subsidy indicator, and production, and the dependent variable: cases.
corr perCapitaProduction perCapitaCases perCapitaFactories subsidizingCity

* Let's also examine a scatter plot matrix 

graph matrix perCapitaProduction perCapitaCases perCapitaFactories subsidizingCity, half maxis(ylabel(none) xlabel(none))


graph export ${output_dir}/Scatters.png, replace

* It seems that factories and production are almost perfectly correlated
* Cases are also positively correlated  with both factories and number of cases

* The subsidy indicator has a weak correlation with production and factories.
* It makes sense that we don't see a high value here,
* since the variable is an indiactor. But the sign
* is positive, as we'd expect if subsidies encourage PokeBall production.

* I will do a few more sanity checks before proceeding with a regression.


* First, were cases higher in cities where factories were subsidized?

graph bar perCapitaCases, over(subsidizingCity)  ytitle(Mean per capita incidence of Asthma)  title(Incidence of Asthma by factory subsidy policy)

* Yes, they were slightly higher*

graph export ${output_dir}/BarChartAsthma.png, replace





* Second, do cases seem to grow after the subsidy was introduced?
* I plot case incidence by year for
* states that subsidized and those that didn't.


gen perCapitaCasesNoSubs = perCapitaCases if subsidizingCity==0
gen perCapitaCasesSubs = perCapitaCases if subsidizingCity==1



graph bar (mean)    perCapitaCasesSubs perCapitaCasesNoSubs , over(year )  scale(*.5) legend( label(1 "Enacted subsidies in 2007")  label(2 "Never enacted subsidies")) ytitle("Asthma incidence", size(large))  title("Asthma incidence, 2000-2015, by PokeBall subsidy policy")


graph export ${output_dir}/BarChartAsthmaOverTime.png, replace

  
* Two interesting findings arise.

* First, all cities experienced a rise in asthma 
* after 2007 - perhaps emissions from 
* cities without PokeBall factories spilled over to ones without. Or maybe diagnosis
* practices just got better.  But cities that subsidized saw 
* a much larger increase, nearly twice as high
* as the prevalence in the early 2000s.

* We also see that in many years before 2006, 5 out of 8, non-subsidizing cities
* had a higher rate of asthma than subsidizing cities, but non-subsizing cities 
* never attain a higher rate of asthma  after 2007, the year subsidies were introduced.


* Visual inspection suggests that PokeBall
* production is causing asthma. Now let's turn to OLS regression.


save ${intermediate_dir}/mergedPokeBallData.dta, replace	  





*** Part 3: Difference in differences Regression


* Generate indicator variable for years in which subsidies were present.
gen postSubsidies = (year>=$subsidiesBegin)
label variable postSubsidies "Post-Subsidies Enactment"

* Generate interaction term between city and pre/post subsidies

gen DID = postSubsidies*subsidizingCity

label variable DID "(Post-Enactment)x(City enacted subsidies)"

* The data is grouped by city - I will check if the size of each city
* varies much, in which case, I should weight the regression by city population.

 summ population
 
* It seems they vary by a factor of roughly 10 - ranging from 165k to 1.5m.
* I will weight the regression by city population to capture the fact that 
* each data point represents a city of a different size. 



* Run the DID regression with robust standard errors, clustered by city, since
* cases within a city will be serially correlated over time.

reg perCapitaCases  postSubsidies subsidizingCity DID  [aweight=population],  robust cluster(cities)
 
 
outreg2 using ${output_dir}/PokeBallReg.doc, replace ctitle(DID Model)   dec(5) sdec(5) label addtext(City Fixed Effects, No, Year Fixed Effects, No) adjr2     addnote("Note: Standard errors clustered by city.")

 
* We find that the DID term is not significant. 

* The sign on the DID coefficient
* is positive, though - this is the sign we'd expect
* if subsidizing did increase asthma rates. Furthermore, time periods following the
* enactment of subsidies included higher incidences of asthma, as evidenced
* by the significance of the "Post-Subsidies" indicator

* Overall, however, this model doesn't make me confident that there's a causal 
* effect on asthma from PokeBall factories.


* While the R^2 for this model is high, the variables used were blunt. Namely, we only
* had an indicator variable to measure PokeBall production - there was no
* independent variable for the precise amount of PokeBalls produced in each city-year. 
* We proceed to part 4 for a more sophisticated approach.







*** Part 4: Regression  on Production



* Now we use PokeBall production  to predict cases.
* The PokeBall production variable 
* contains more detailed data than the binary variable 
* indicating a city's subsidy choice, which was used in the DID regression.

* I do not use the number of factories in this regression
* since they are highly correlated with production.


reg perCapitaCases perCapitaProduction i.cities i.year [aweight=population],  robust  cluster(cities) 



* It seems that PokeBall Production is significant when controlling
* for city and time  effects.

* The point estimate of the production coefficient is significant
* at the .0001 level.  The coefficient value of .0007  suggests that 
* increasing production 
* by 15 PokeBalls per person implies a 1% higher annual incidence of 
* asthma for a city (.01/.0007 ~= 15).



* But is the the size of the coefficient practically meaningful? We answer below.

* First, we  must know if a 1% swing in asthma rates is high.

summarize perCapitaCases

* 1% seems material: asthma rates range from .06% to 3% in our data.


* Second, we must know how much per capita production ranges between cities.
summarize perCapitaProduction


* Cities produce between 0 and 25 Pokeballs per capita, so Pokeball production
* alone could increase a city's annual asthma cases by 1%.



* Thus, the effect size is both statistically significant and
* economically meaningful.

* Now we check for heteroskedasticity to confirm our use of 
* heteroskedasticity-robust standard errors.


predict resids, residuals


scatter resids perCapitaProduction
graph export ${output_dir}/Heteroskedasticity.png, replace


* It seems that there is some heteroskedasticity - predictions seem to get
* more accurate for higher levels of Pokeball production. This confirms
* the need for robust standard errors.



* Let's see how the prediction compares to actual values.

predict predictions

 graph twoway (scatter perCapitaCases perCapitaProduction )  (lfitci predictions perCapitaProduction )  ,   title("Regression Predictions vs Actuals") ytitle("Asthma Incidence") legend(label(1 "Actuals") )



save ${intermediate_dir}/mergedPokeBallData.dta, replace	  


* The fit seems to be fairly good.
* Again note the heteroskedasticity - the error seems to decrease for larger values.

graph export ${output_dir}/PredictionsVsActuals.png, replace

* Finally, let's check for multicollinearity:

vif

* Only the perCapitaProduction variable and one of the city fixed effects has a VIF > 5,
* Multicollinearity is probably not an issue in interpreting the results.


* I am not familiar with the mechanism for Pokeball production causing asthma, 
* but it's plausible that
* PokeBall production takes 1 year or more
* to begin causing asthma.  I compare a 1-year, 2-year, and 3-year lagged regression 
* to the non-lagged regression above.

xtset cities year

reg perCapitaCases L.perCapitaProduction  i.year i.cities [aweight=population],  robust  cluster(cities)

reg perCapitaCases L2.perCapitaProduction i.year i.cities [aweight=population] ,  robust  cluster(cities)

reg perCapitaCases L3.perCapitaProduction i.year i.cities [aweight=population] ,  robust  cluster(cities)

* These all result in lower R^2 and lower t-statistics for per capita production,
* so I go back to the plain model.

reg perCapitaCases perCapitaProduction i.year i.cities [aweight=population] ,  robust  cluster(cities)

outreg2 using ${output_dir}/PokeBallReg.doc, append title(PokeBall Factories' effect on Asthma) ctitle(PokeBall Production Model)  addtext(City Fixed Effects, Yes, Year Fixed Effects, Yes)    keep(perCapitaProduction) dec(5) sdec(5) adjr2 label


* Conclusion

* Visual inspection and the regression of cases on PokeBall production
* make me comfortable interpreting the correlation between PokeBall production 
* and asthma as a causal effect. 

* One piece of data we might 
* want to collect is if other (non-PokeBall) factories were built around 
* the same time as the PokeBall factories. 
* Perhaps the PokeBall factories are suppliers for another type of 
* firm and those firms are causing asthma.






  

	  
