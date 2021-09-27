/*
Food retailers data cleaning
Fernando Herandez
9/27/2021
*/


global data "\\Ares\CTX_RedirectedFolders$\FHernandez\Desktop\ArlCo Food Insecurity\Quantitative work\Data\"

*Charitable food sites data uploading
import delimited "${data}Food_resources.csv", bindquote(strict) clear 

	*dropping unnec vars
	drop name email phone website registration_contact auto_unique_id source ///
		timetaken updatedgeocod version error transaction ///
		featurematchingresulttype featurematchingresulttypetiebrea ///
		featurematchinghierarchynotes featurematchingresulttypenotes ///
		featurematchingresultcount
	
	format description %16s
	format eligibility_requirements %20s 
	ren orgid objectid
	gen county = "ARLINGTON"
	replace state = "VA" if state=="Virginia"
	save "${data}food_resources.dta", replace
	clear all

*SNAP retailers food sites data uploading
import delimited "${data}SNAP_Store_Locations_NOVA.csv", clear
	drop auto_unique_id source timetaken updatedgeocod version error ///
		transaction featurematchingresulttype ///
		featurematchingresulttypetiebrea featurematchinghierarchynotes ///
		featurematchingresulttypenotes featurematchingresultcount zip4

	gen accepts_snap = 1
	
	ren (store_name address zip5) (location_name location_address zip_code)
append using "${data}food_resources.dta"

	
keep if county == "ARLINGTON" // activate to keep only arlington food sites.

/*Processing 
tab type, gen(loctype)
	label define yesno 1 "Yes" 0 "No" 
	forvalues i = 1(1)6 {
		label val loctype`i' yesno
		replace loctype`i' = . if type==""
	}
	
	replace loctype4 = 1 if loctype5==1
	
	ren (loctype1 loctype2 loctype3 loctype4 loctype6) (discount_groceries ///
		food_pantry meal_kits prepped_meals school_meals) ///
		
		drop loctype* type
		
		bys location_name : gen n_ = _n
		keep if n_==1 // takes care of duplicates
	*/
	
	replace location_name=proper(location_name)
	
	gen store_type = ""
		replace store_type = "Convenience store" if regexm(location_name, "7-Eleven") ///
								| regexm(location_name, "Walgreens") ///
								| regexm(location_name, "Cvs") ///
								| regexm(location_name, "Sk Dominion") ///
								| regexm(location_name, "7 Eleven") ///
								| regexm(location_name, "Brook Market")
		replace store_type = "Farmer's market" if regexm(location_name, "Farmers Market") ///
								| regexm(location_name, "Freshfarm ") ///
								| regexm(location_name, "Fresh Farm Market")
		replace store_type = "Supermarket/Food Store" if regexm(location_name, "Supermarket")	///
								| regexm(location_name, "Giant")	///
								| regexm(location_name, "Safeway")	///
								| regexm(location_name, "Wholefoods")	///
								| regexm(location_name, "Whole Foods")	///
								| regexm(location_name, "Costco")	///
								| regexm(location_name, "Target Store")	///
								| regexm(location_name, "Mom'")	///
								| regexm(location_name, "Arlington Grocery") ///
								| regexm(location_name, "Trader Joe")	///
								| regexm(location_name, "Harris Teeter")	///
								| regexm(location_name, "Asni Inc")	///
								| regexm(location_name, "Lideta Market") ///
								| regexm(location_name, "Glebe Market") ///
								| regexm(location_name, "Lucky Seven") ///
								| regexm(location_name, "Asia Market") ///
								| regexm(location_name, "Bangla Bazar") ///
								| regexm(location_name, "Megamart Express") ///
								| regexm(location_name, "El Latino & African")
		replace store_type = "AHC/Apartments" if regexm(location_name, "Apartments") ///
								| regexm(location_name, "(Wesley Housing)")	///
								| regexm(location_name, "The Heights")	///
								| regexm(location_name, "Gilliam Place")	///
								| regexm(location_name, "(Ahc)")	///
								| regexm(location_name, "Colonial Village W") ///
								| regexm(location_name, "(Knoll Condos)")	
		replace store_type = "Church" if regexm(location_name, " Church") ///	
								| regexm(location_name, "Thomas More") 
		replace store_type = "School" if regexm(location_name, "Elementary School") ///
								| regexm(location_name, "High School") ///
								| regexm(location_name, "Middle School") 
		replace store_type = "Misc" if regexm(location_name, "Library") ///
								| regexm(location_name, "Parking Lot")	///
								| regexm(location_name, "Women'S Club Of")	///
								| regexm(location_name, "Afac Head") ///
								| regexm(location_name, "Virginia Hospital") ///
								| regexm(location_name, "New Start Cares") ///
								| regexm(location_name, "Salvation Army") ///
								| regexm(location_name, "Ft Myers Commissary") ///
								| regexm(location_name, "Community Center") ///	
								| regexm(location_name, "Career Center") ///
								| regexm(location_name, "al City Courtyard") ///
								| regexm(location_name, "Islamic Center")
							 	