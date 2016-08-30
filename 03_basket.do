/* -----------------------------------------------------------------------------

     WFP - Policy and Programme Division - Analysis & Trends Service 
		   Economic and Market Analysis Unit 
		   
     CONTACT: Lucia Latino 
			  lucia.latino@wfp.org
			  Latino@Economia.uniroma2.it
      
	 AIM: generate the historical cost of food baskets for each country 
     
     This version: May 31, 2016

----------------------------------------------------------------------------- */


*** Clear environment 
	clear
	set more off

	use $path/output/data_all_clean.dta
		
	keep if t<tm(2016, 12) | Notes!="" // DATE TO BE CHANGED FOR NEXT UPDATE. 
									  // The drop is needed because only few prices have been uploaded, thus the minum calorie for the bood basked will be too low
	
	gen price_g=price/1000
	
	drop if price_g==. & Notes==""
	
	gen total_cal=2100
		
	gen kcal=total_cal*fao_fct_kcalshare/100
	label var kcal "kcal/day/person from cm_name"
		
	gen qt=kcal/cm_kcal_100g*100
	label var qt "g/person/day from cm_name"
	
	gen cost=qt*price_g*30.5
	label var cost "national currency/person/month for cm_name"
	
	drop if cost==. & Notes==""
		
*** by country, time and food group choose the commodity with the highest priority
	bys adm0_id t fao_fct_name: egen max_pr=min(priority)
	keep if priority==max_pr 

*** find the minimum calorie content for the food basket of each country
	* NOTE: time cut off should be changed by x month forward when updated is done in next x months
	bys adm0_id t: egen calorie=total(fao_fct_kcalshare) 
	gen l_cal=.
	
	levelsof adm0_id if Notes=="", local (country)
	 foreach num of numlist `country' { 
		
		local i = 1
		while `i'<40 {
			sum t if calorie==`i' & adm0_id==`num'
			gen l_time=r(max) if adm0_id==`num'
			drop if t<=l_time & l_time<=tm(2013, 6) & adm0_id==`num'  // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			egen l_cal_`num'=min(calorie) if adm0_id==`num'
			
			tempvar check
			gen `check'=1 if adm0_id==`num' & (l_time<=tm(2013, 6) | l_time==.) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			replace `check'=2 if adm0_id==`num' & l_time>tm(2013, 6) & l_time!=. // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			levelsof `check' , local(g)
				if  `g'==1 {
					levelsof l_cal_`num' if adm0_id==`num' & (l_time<=tm(2013, 6) | l_time==.), local(i) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
				}
				else {
					local i = 200
				}
				display `i'
				drop l_time l_cal_`num'
		}	
		sum calorie if adm0_id==`num'
		replace l_cal=r(min) if adm0_id==`num'
		
		sum calorie if adm0_id==`num' & t>=tm(2011, 7) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
		local min=r(min)
		levelsof l_cal if adm0_id==`num', local (c)
		while `c'<`min' {
			sum calorie if adm0_id==`num' & t>=tm(2011, 7) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			sum t if calorie==r(min) & adm0_id==`num'
			gen l_time=r(min) if adm0_id==`num'
			drop if adm0_id==`num' & t<l_time  & t<tm(2011, 7) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			sum calorie if adm0_id==`num'
			replace l_cal=r(min) if adm0_id==`num'
			drop l_time
			
			levelsof l_cal if adm0_id==`num', local (d)
			
				if `d'<`min'{
					sum t if adm0_id==`num' & l_cal==calorie
					gen l_time=r(max) if adm0_id==`num'
					drop if adm0_id==`num' & t<=l_time & t<tm(2011, 7) // NOTE: time cut off should be changed by x month forward when updated is done in next x months 
					drop l_time
				}
				
			levelsof l_cal if adm0_id==`num', local (c)
			sum calorie if adm0_id==`num' & t>=tm(2011, 7) // NOTE: time cut off should be changed by x month forward when updated is done in next x months
			local min=r(min)
		}
		
	}
	
	
	table adm0_name if l_cal<40, c(mean cal mean l_cal)
	
*** generate the time series for the cost of food basket 	
	gsort adm0_id t -fao_fct_k fao_fct_name
	by adm0_id t: gen basket=sum(fao_fct_kcalshare) 
			
	bys adm0_id t: egen food_basket=total(cost) if basket<=l_cal+(l_cal*0.05)
	label var food_basket "cost of the food basket - national currency/person/month"

	bys adm0_id t: egen kcal_share=total(fao_fct_k) if basket<=l_cal+(l_cal*0.05)
	label var kcal_share "share of kcal per food basket (time specific)"
	
	bys adm0_id: egen avg_kcal_share=mean(kcal_share)
	label var kcal_share "average share of kcal per food basket"

*** obtain and save in excel food basket's details
	table adm0_name, c(mean kcal_share min kcal_share max kcal_share) format(%9.0f)
	
preserve
	replace adm0_name="State of Palestine" if adm0_id==999
	drop if food_basket==. | food_basket==0
	bys adm0_id: egen basket_mean_kcalshare=mean(kcal_share)
	format %9.0f basket_mean_kcalshare
	bys adm0_id: egen basket_min_kcalshare=min(kcal_share)
	bys adm0_id: egen basket_max_kcalshare=max(kcal_share)
	
	sort series t
	egen   start_date = min(t), by (series)
	egen   end_date   = max(t), by (series)
	format %tmMon-yy t start_date end_date
	gen 	month_cover = end_date - start_date +1
	
	bys series: egen data_count = count(price) 
	gen gap=1-(data_count/month_cover)

	replace cm_name=cm_name_F if cm_name==""
	duplicates drop adm0_name cm_name series pt start_date end_date, force
	
	egen tag=tag(adm0_name end_date)
	bys adm0_name:egen basket_changes=total(tag)
	replace basket_change=basket_change-1
	
	gen price_type="retail" if pt==15
	replace price_type="wholesale" if pt==14
	replace price_type="producer" if pt==17
	replace price_type="farm gate" if pt==18

	
	keep adm0_name cm_name fao_fct_name start_date end_date basket_* fao_fct_kcalshare national data_sour price_type
	rename adm0_name Country
	rename cm_name commodity
	rename fao_fct_name food_group
	rename fao_fct_kcalshare commodity_kcalshare
	
	gsort Country -commodity_kcalshare
	
	egen tag=tag(Cou)
	replace Country="" if tag==0
	replace basket_mea=. if tag==0
	replace basket_mi=. if tag==0
	replace basket_ma=. if tag==0
	drop tag
	
	
	order Country basket_mean_kcalshare basket_min_kcalshare basket_max_kcalshare commodity price_type food_group start_date end_date basket_change 
	export excel using $path/output/SFE.xlsx, sheet("basket") sheetreplace firstrow(varia)
	putexcel set $path/output/SFE.xlsx, sheet("basket") modify
	putexcel (A1:P1), bold hcenter vcenter font(Calibri, 11, darkblue) 
	putexcel (A1:A500), bold  font(Calibri, 11)	
	putexcel (B2:D500), nformat(number)
restore	
	
	egen keep=tag(adm0_id time) if food_basket!=. 
	
	keep if keep | Notes!=""
	
	keep adm0_name adm0_id t* food_basket cur* avg_kcal_share Notes
		
	sort adm0_id time
	
	save $path/output/basket.dta, replace
	
