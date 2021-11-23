/*
Food retailers data cleaning
Fernando Herandez
9/27/2021
*/

clear all
global data "\\Ares\CTX_RedirectedFolders$\FHernandez\Desktop\ArlCo Food Insecurity\Quantitative work\Data\"
global save "\\Ares\CTX_RedirectedFolders$\FHernandez\Desktop\ArlCo Food Insecurity\Quantitative work\Data\Final food data\"


*Charitable food sites data uploading
import excel "${data}Environmental Scan - Food Assistance in Arlington.xlsx", ///
	sheet("Food Distros Info") cellrange(A2:AK61) firstrow allstring

	ren *, lower
	ren (locationname locationaddress zipcode yearroundordates /// 
		registrationcontact eligibilityrequirements eligibilityrequirement ///
		freshproduce acceptssnap agerestrictionsforchildren ///
		agerestrictionsforseniorso) (location_name address zip ///
		year_round_or_dates registration_contact eligibility_requirements ///
		eligibility_requirement fresh_produce accepts_snap ///
		age_restrictions_for_children_or age_restrictions_for_seniors_onl)

	drop if type == ""
	gen location_type = "Charitable food-site"
	gen state = "Virginia"
	gen city = ""
	
	replace address = trim(address)
		drop if address == ""

	export delimited using "${save}Food site data\Charitable_food_sites_ArCo.csv", ///
		replace quote
	
	*At this point we go to TAMU for geocoding - no good stata command to do in-house
	clear all
	import delimited "${save}Food site data\Charitable_food_sites_ArCo_geocoded.csv", bindquote(strict)
	*dropping unnec vars
	drop name email phone website registration_contact auto_unique_id source ///
		timetaken updatedgeocod version error transaction ///
		featurematchingresulttype featurematchingresulttypetiebrea ///
		featurematchinghierarchynotes featurematchingresulttypenotes ///
		featurematchingresultcount naaccrqualcode naaccrqualtype ///
		matchedlocationtype regionsizeunits interpolationtype regionsize ///
		interpolationsubtype featurematchinggeographytype matchscore ///
		featurematchinghierarchy tiehandlingstrategytype geocodequalitytype ///
		matchtype
	
	drop if address == "2912 Woodlawn Trail"
	
	format time %50s
	format description %50s
	format eligibility_requirements %50s 
	gen county = "ARLINGTON"
	replace state = "VA" if state=="Virginia"
	
	*Fixing times for specific sites:
	
	gen objectid = _n

	foreach var of varlist eligibility_requirements eligibility_requirement ///
		nophotoidrequired noregistrationrequired restrictedaccessonlymembers ///
		age_restrictions_for_children_or age_restrictions_for_seniors_onl ///
		incomemaximumforeligibility deliveryavailable preparedmeals ///
		groceryitems fresh_produce accepts_snap otherservicesprovideddiaper ///
		otherservicesprovidedfinanc otherservicesprovidedclothi ///
		otherservicesprovided {
			replace `var' = "1" if `var'=="x"
			replace `var' = "0" if `var'==""
			destring `var', replace
		}
		
		drop city
		ren address location_address
		
	save "${data}food_resources.dta", replace

	clear all

*SNAP retailers food sites data uploading
import delimited "${data}SNAP_Store_Locations_NOVA.csv", clear
	drop auto_unique_id source timetaken updatedgeocod version error ///
		transaction featurematchingresulttype ///
		featurematchingresulttypetiebrea featurematchinghierarchynotes ///
		featurematchingresulttypenotes featurematchingresultcount zip4

	gen location_type = "SNAP-retailer"
	gen accepts_snap = 1
	
	ren (store_name address zip5) (location_name location_address zip_code)
	append using "${data}food_resources.dta"
	replace location_name=proper(location_name)

drop if objectid == 33525 | objectid==103791 | objectid==221002 | ///
	objectid==184948 | objectid==151312 | objectid==117198 | objectid==154843 | ///
	objectid==234775 | objectid==90277 | objectid==7066 | objectid==34684 | objectid==49446
	
gen store_type = ""
replace store_type = "Supermarket/Grocery Store" if regexm(location_name, "Supermarket")	///
	| regexm(location_name, "Giant")	///
	| regexm(location_name, "Afghan Market Llc") ///
	| regexm(location_name, "Al Amal Market Inc") ///
	| regexm(location_name, "La Bodega") ///
	| regexm(location_name, "Amazon Fresh") ///
	| regexm(location_name, "Beirut Halal Butcher") ///
	| regexm(location_name, "La Feria Latina") ///
	| regexm(location_name, "Market") ///
	| regexm(location_name, "Bismillah Groceries & ") ///
	| regexm(location_name, "Deshi Bazar & International") ///
	| regexm(location_name, "International") ///
	| regexm(location_name, "Al Makkah Mart") ///
	| regexm(location_name, "African Grocery And Meat Market") ///
	| regexm(location_name, "Bestway")	///
	| regexm(location_name, "Al-Amanah Market") ///
	| regexm(location_name, "Ali Baba Groceries And Halal Meat") ///
	| regexm(location_name, "Americana Grocery Of Va") ///
	| regexm(location_name, "Assal Market") ///
	| regexm(location_name, "International Market") ///
	| regexm(location_name, "Baitna Market 1") ///
	| regexm(location_name, "International Food") ///
	| regexm(location_name, "Beirut Butchery") ///
	| regexm(location_name, "Bien Hoa") ///
	| regexm(location_name, "Centro Market") ///
	| regexm(location_name, "Desi Bazar") ///
	| regexm(location_name, "Dns Family Market") ///
	| regexm(location_name, "El Compadre Iii") ///
	| regexm(location_name, "El Compadre Multiservices Inc 1") ///
	| regexm(location_name, "El Grande") ///
	| regexm(location_name, "El Mercadito Hispano") ///
	| regexm(location_name, "El Paisa Grocery") ///
	| regexm(location_name, "Engocha Llc") ///
	| regexm(location_name, "Yemex International Grocery") ///
	| regexm(location_name, "Yas Bakery & Grocery") ///
	| regexm(location_name, "World Foods Int") ///
	| regexm(location_name, "Wooden Bakery") ///
	| regexm(location_name, "Virginia Halal Meat Inc") ///
	| regexm(location_name, "Taj Grocery") ///
	| regexm(location_name, "Tenadam Market")	///
	| regexm(location_name, "Super Halal") ///
	| regexm(location_name, "Fair Price") ///
	| regexm(location_name, "Fresh World Herndon, Inc.") ///
	| regexm(location_name, "Gateway International Market") ///
	| regexm(location_name, "Skyline Butcher Shop") ///
	| regexm(location_name, "Shri Krishna Grocery") ///
	| regexm(location_name, "Showa Baltena") ///
	| regexm(location_name, "Food Star")	///
	| regexm(location_name, "Global")	///
	| regexm(location_name, "La Mart")	///
	| regexm(location_name, "Safeway")	///
	| regexm(location_name, "Lotte Plaza")	///
	| regexm(location_name, "Wholefoods")	///
	| regexm(location_name, "B Js")	///
	| regexm(location_name, "Bjs")	///
	| regexm(location_name, "Target ")	///
	| regexm(location_name, "Food Lion")	///
	| regexm(location_name, "Super A Market")	///
	| regexm(location_name, "Tiger Market")	///
	| regexm(location_name, "The Fresh Market")	///
	| regexm(location_name, "Aldi")	///
	| regexm(location_name, "H Mart")	///
	| regexm(location_name, "Grandmart")	///
	| regexm(location_name, "Shoppers Food")	///
	| regexm(location_name, "Mart")	///
	| regexm(location_name, "Lidl")	///
	| regexm(location_name, "Whole Foods")	///
	| regexm(location_name, "Costco")	///
	| regexm(location_name, "Target Store")	///
	| regexm(location_name, "Mom'")	///
	| regexm(location_name, "Brook Market")	///
	| regexm(location_name, "Arlington Grocery") ///
	| regexm(location_name, "Trader Joe")	///
	| regexm(location_name, "Harris Teeter")	///
	| regexm(location_name, "Asni Inc")	///
	| regexm(location_name, "Margarita`S Grocey") ///
	| regexm(location_name, "Mercadito Ramos") ///
	| regexm(location_name, "Lideta Market") ///
	| regexm(location_name, "Mi Tierra Mercado") ///
	| regexm(location_name, "Glebe Market") ///
	| regexm(location_name, "La Latina") ///
	| regexm(location_name, "Lucky Seven") ///
	| regexm(location_name, "Asia Market") ///
	| regexm(location_name, "Bangla Bazar") ///
	| regexm(location_name, "Walmart") ///
	| regexm(location_name, "Wegman") ///
	| regexm(location_name, "Marhaba") ///
	| regexm(location_name, "Pho Supplies") ///
	| regexm(location_name, "Megamart Express") ///
	| regexm(location_name, "El Latino & African")
replace store_type = "Supermarket/Grocery Store"  if regexm(location_name, " Grocery") & store_type==""
replace store_type = "Convenience store/Other retail" if regexm(location_name, "7-Eleven") ///
	| regexm(location_name, "Walgreens") ///
	| regexm(location_name, "Cvs") ///
	| regexm(location_name, "Dollar ") ///
	| regexm(location_name, "Convenience ") ///
	| regexm(location_name, "Mobil ") ///
	| regexm(location_name, "Mobile ") ///
	| regexm(location_name, "Royal Farm ") ///
	| regexm(location_name, "Sai Express") ///
	| regexm(location_name, "Exxon ") ///
	| regexm(location_name, "Q Mart") ///
	| regexm(location_name, "Speedway") ///
	| regexm(location_name, "Bazaar")	///
	| regexm(location_name, "Dollartree")	///
	| regexm(location_name, "Sk Dominion") ///
	| regexm(location_name, "One Stop Beer") ///
	| regexm(location_name, "A Plus") ///
	| regexm(location_name, "Aplus") ///
	| regexm(location_name, "Wawa") ///
	| regexm(location_name, "7 Eleven") ///
	| regexm(location_name, "Afghan Mini Market") ///
	| regexm(location_name, "Brook Market") ///
	| regexm(location_name, "Bakhtar Bakery") ///
	| regexm(location_name, "C-More Drug Store") ///
	| regexm(location_name, "Colonial Market") ///
	| regexm(location_name, "Deggi Market & Cafe") ///
	| regexm(location_name, "Ez Stop & Go Food Mart") ///
	| regexm(location_name, "Super Mini Mart")	///
	| regexm(location_name, "Mart Eleven Convenience Store") ///
	| regexm(location_name, "Asia Bazaar")
replace store_type = "Farmer's market" if regexm(location_name, "Farmers Market") ///
	| regexm(location_name, "Freshfarm ") ///
	| regexm(location_name, "Fresh Farm Market") ///
	| regexm(location_name, "Alexandria Old Town Farmers' Market") ///
	| regexm(location_name, "Southern Towers Farmers' Market")
replace store_type = "Apartments" if regexm(location_name, "Apartments") ///
	| regexm(location_name, "(Wesley Housing)")	///
	| regexm(location_name, "The Heights")	///
	| regexm(location_name, "Gilliam Place")	///
	| regexm(location_name, "(Ahc)")	///
	| regexm(location_name, "Colonial Village W") ///
	| regexm(location_name, "Harvey Hall") ///
	| regexm(location_name, "(Knoll Condos)")	
replace store_type = "Church/Mosque/Community Center" if regexm(location_name, " Church") ///	
	| regexm(location_name, "Thomas More")  ///
	| regexm(location_name, "Dhic")  ///
	| regexm(location_name, "Community Center")  ///
	| regexm(location_name, "Al-Hijrah")  ///
	| regexm(location_name, "Baitul Mukarram Masjid")
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
	| regexm(location_name, "Islamic Center") ///
	| regexm(location_name, "Welburn Square") ///
	| regexm(location_name, "Central Place Plaza") ///
	| regexm(location_name, "Good Company") ///
	| regexm(location_name, "Na") ///
	| regexm(location_name, "Mediterranean Bakery & Cafe") ///
	| regexm(location_name, "24 Express") ///
	| regexm(location_name, "Gilgamesh") ///
	| regexm(location_name, "Gajin Ramen Shop")
replace store_type = "Supermarket/Grocery Store" if regexm(location_name, "Brook Market")	

replace type = store_type if type==""
replace type = "Prepared meals/Meal kits" if type=="Prepared meals"
replace type = "Prepared meals/Meal kits" if type=="Prepared Meals"
replace type = "Prepared meals/Meal kits" if type=="Meal Kits"
replace type = "Discount grocery pick-up site" if type=="Discount groceries"
	
*Fixing produce variable
	replace fresh_produce = 1 if regexm(store_type, "Supermarket/Grocery Store")
	replace fresh_produce = 1 if regexm(store_type, "Farmer's market")
	replace fresh_produce = 0 if regexm(location_name, "New Start Cares Llc")
	replace fresh_produce = 0 if regexm(location_name, "Ft Myers Commissary ") ///
		| regexm(location_name, "Sk Dominion Market")
	replace fresh_produce = 0 if regexm(location_name, "Brook Market")
	replace fresh_produce = 0 if regexm(location_name, "7-Eleven") ///
					| regexm(location_name, "Walgreens") ///
					| regexm(location_name, "Cvs") ///
					| regexm(location_name, "7 Eleven") 

*dropping unnecessary vars
drop naaccrqualcode	naaccrqualtype	matchedlocationtype	regionsizeunits	///
	interpolationtype interpolationsubtype	featurematchinggeographytype ///
	featurematchinghierarchy	tiehandlingstrategytype	geocodequalitytype 

*Cleaning the fresh_produce var
replace fresh_produce = 0 if regexm(location_name, "Exxon")
replace fresh_produce = 0 if regexm(location_name, "Speedway")
replace fresh_produce = 0 if regexm(location_name, "Dollar Tree")
replace fresh_produce = 0 if regexm(location_name, "Dollartree")
replace fresh_produce = 1 if regexm(location_name, "Amazon Fresh")

*Dropping sites outside of the sample frame (restaurants and c-stores)
drop if store_type == "Convenience store/Other retail"
drop if location_name=="Good Company Doughnuts & CafÃÂ©" | ///
	location_name=="Gajin Ramen Shop"
drop if regexm(location_name, "Randolph ")

*TIMING
**Frequency with which one can visit the site
gen frequency_visit = .
	replace frequency_visit = 1 if regexm(frequency, "3 times weekly") | ///
		regexm(frequency, "Bi-weekly") | regexm(frequency, "Weekly") | ///
		regexm(frequency, "Twice weekly") | regexm(frequency, "Daily") | ///
		regexm(frequency, "Participants can visit once per week")
	replace frequency_visit = 2 if regexm(frequency, "Once every 3 weeks") | ///
		regexm(frequency, "Every other week") | regexm(frequency, "Monthly") | ///
		regexm(frequency, "Weekly. Participants")
	replace frequency_visit = 3 if (regexm(frequency, "By Appointment") & ///
		regexm(location_name, "Lomax ")) | ///
		(regexm(frequency, "By Appointment") & regexm(location_name, "Randolph ")) ///
		| regexm(frequency, "Upon") | ///
		regexm(frequency, "NA")
	replace frequency_visit = .a if location_name== "Central Library"
	replace frequency_visit = .b if frequency_visit == .
		label define frequency_visit 1 "Weekly or more frequent" ///
			2 "Less than weekly, but monthly or more" 3 "Other frequency" ///
			.a "Unknown frequency" .b "NIS"
			label val frequency_visit frequency_visit 

**Is the site open year-round?
gen year_round = .
	replace year_round = 1 if regexm(year_round_or, "Year-round")
	replace year_round = 0 if year_round==. & year_round_!=""
	replace year_round = .b if year_round==. & year_round_!=""	
		label define year_round 1 "Open year-round" 0 "Not open year-round" ///
			.a "UNK" .b "NIS"
		label val year_round year_round
		
**Is the site open on weekends
gen weekends = .
	replace weekends = 1 if regexm(day, "Sat") | regexm(day, "Saturday") | ///
		regexm(day, "Sun") | regexm(day, "Sunday") | regexm(times, "Sat ") | ///
		(day == "By Appointment" & times == "By Appointment")
	replace weekends = 0 if weekends==. & day !=""
	replace weekends = .a if weekends==. & location_type!="Charitable food-site"
	replace weekends = .b if weekends==. & location_type=="Charitable food-site"
		label define yesno 1 "Yes" 0 "No" .a "Unknown times" .b "NIS" 
		label val weekends yesno 

	replace times = "Unknown" if times=="" & location_type=="Charitable food-site"
	replace day = "Unknown" if day=="" & location_type=="Charitable food-site"

**Is the site open after regular business hours?
gen open_afterhrs= .
	replace open_afterhrs = 1 if regexm(times, "7 PM") | regexm(times, "-7PM") | ///
		regexm(times, "6:00 PM") | regexm(times, "6:30 PM") | ///
		regexm(times, "5:00 PM") | regexm(times, "5:30 PM")
	replace open_afterhrs = 2 if open_afterhrs==. & regexm(location_type,"Charitable")
	replace open_afterhrs = 3 if regexm(times, "By ") 
	replace open_afterhrs = .a if times=="" & regexm(location_type,"Charitable")
	replace open_afterhrs = .b if times=="" & regexm(location_type,"Charitable")
		label define open_afterhrs 1 "Open at or after 5:00 PM" ///
			2 "Open typical business hours" 3 "By appointment" .a "UNK" .b "NIS"
		label val open_afterhrs open_afterhrs 
		
**Summing up frequency into one variable
gen freq_chars = .
	replace freq_chars =  1 if frequency_visit == 1 & weekends==1 & ///
		year_round == 1
	replace freq_chars = 2 if frequency_visit == 1 & weekend==0 & ///
		open_afterhrs==1 & year_round == 1
	replace freq_chars = 3 if frequency_visit == 1 & weekend==0 & ///
		open_afterhrs!=1 & year_round == 1
	replace freq_chars = 4 if frequency_visit==2 | open_after==3
	replace freq_chars = 5 if year_round==0 & regexm(location_type,"Charitable")
	replace freq_chars = .a if frequency_visit==.a
	replace freq_chars = .b if location_type!="Charitable food-site"
		label define freq_chars 1 "Weekly or more, weekends, and year-round" ///
			2 "Weekly or more, weekdays only, after-hours, year-round" ///
			3 "Weekly or more, weekdays only, after-hours, year-round"  ///
			4 "Less than weekly, by appointment, year-round" ///
			5 "Open seasonally" .a "UNK" .b "NIS"
		label val freq_chars freq_chars 

*RESTRICTIONS TO ACCESS
**Eligibility requirements fixed
replace eligibility_requirement = 1 if ///
	regexm(eligibility_requirements, "Open to senior residents of this apartment building that are income eligible.")
replace eligibility_requirement = 1 if ///
	regexm(eligibility_requirements, "FRESH match is only available for those ")
replace eligibility_requirement = 0 if ///
	regexm(eligibility_requirements, "Open to all Arlington residents")
replace eligibility_requirement = 0 if ///
	regexm(eligibility_requirements, "Bring photo ID on first visit, ")
replace eligibility_requirement = 0 if ///
	regexm(eligibility_requirements, "Open to all residents of the Arlington")
	
gen restrictions = .
	replace restrictions = 1 if eligibility_requirement==0
	replace restrictions = 2 if regexm(eligibility_requirements, "Open to AHC residents") | ///
		regexm(eligibility_requirements, "Open to APAH residents") | ///
		regexm(eligibility_requirements, "Open to Colonial Village") | ///
		regexm(eligibility_requirements, "Open to Wesley Housing") | ///
		regexm(eligibility_requirements, "residents of the Buckingham") 	
	replace restrictions = 3 if regexm(eligibility_requirements, "enrolled at ") | ///
		eligibility_requirements== "Child under 18" | ///
		regexm(eligibility_requirements, "Open to CSES ") | ///
		regexm(eligibility_requirements, "Open to Abingdon Elementary School families") | ///	
		regexm(eligibility_requirements, "Families of students at")  
	replace restrictions = 4 if regexm(eligibility_requirements, "Elder") | ///
		regexm(eligibility_requirements, "residents of Claridge House ") | ///
		regexm(eligibility_requirements, "residents of Culpepper Gardens ") | ///
		regexm(eligibility_requirements, "residents of Hunter's Park ") | ///
		regexm(eligibility_requirements, "residents of The Carlin Senior ") | ///
		regexm(eligibility_requirements, "residents of Woodland Hill ") | ///
		regexm(eligibility_requirements, "Open to senior residents of this ") | ///
		regexm(eligibility_requirements, "Arlington County residents age 60") 
	replace restrictions = 5 if regexm(eligibility_requirements, "FRESH match is only available for those ")
	replace restrictions = 6 ///
		if regexm(eligibility_requirements, "School aged child or elderly") | ///
		regexm(eligibility_requirements, "Active Duty military") | ///
		regexm(eligibility_requirements, "Open to VHC")
	
	label define restrictions 1 "Open to all" 2 "Open to residents" ///
		3 "Serving children only" 4 "Serving elders only" ///
		5 "SNAP requirement" 6 "Veterans only or other restriction"
	label val restrictions restrictions

gen access = .
	replace access = 1 if restrictions==1
	replace access = 2 if restrictions==3 | restrictions==4
	replace access = 3 if restriction == 5
	replace access = 4 if restriction == 2 | restriction==6
		label define access 1 "Open to all" 2 "Open to children/elders only" ///
			3 "Requires having SNAP" 4 "Another restriction"
		label val access access
*replace county = "ARLINGTON" if location_type == "Non-SNAP-retailer"
	
save "${save}food_stores_data_TRANSPORT.dta", replace
export delimited using "${save}Food_retailers_TRANSPORT.csv", replace

	keep if county == "ARLINGTON"
	
	save "${save}food_stores_data_MAPPING.dta", replace
		export delimited using "${save}Food_retailers_MAPPING.csv", replace
	
	preserve
		keep if access == 1
		export delimited using "${save}Food_retailers_cfs_o2a.csv", replace
	restore
	
	preserve
		keep if restriction  == 3
		export delimited using "${save}Food_retailers_cfs_child.csv", replace
	restore
	
	preserve
		keep if restriction == 4
		export delimited using "${save}Food_retailers_cfs_elder.csv", replace
	restore
	
	
	
	
/* GARBAGE BIN

*Typologies

gen loctype_fresh = .
	replace loctype_fresh = 1 if fresh_produce==0 & location_type == "Charitable food-site"
	replace loctype_fresh = 2 if fresh_produce==0 & location_type == "SNAP-retailer"
	replace loctype_fresh = 3 if fresh_produce==0 & location_type == "Non-SNAP-retailer"
	replace loctype_fresh = 4 if fresh_produce==1 & location_type == "Charitable food-site"
	replace loctype_fresh = 5 if fresh_produce==1 & location_type == "SNAP-retailer"
	replace loctype_fresh = 6 if fresh_produce==1 & location_type == "Non-SNAP-retailer"
		label define loctype_fresh 1 "Charitable food-site without fresh produce" ///
		2 "SNAP-retailer without fresh produce" ///
		3 "Non-SNAP-retailer without fresh produce" ///
		4 "Charitable food-site with fresh produce" ///
		5 "SNAP-retailer with fresh produce"	///
		6 "Non-SNAP-retailer with fresh produce"	
		label val loctype_fresh loctype_fresh 
		
		*/
