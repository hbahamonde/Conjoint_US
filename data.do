* THis here is to clean the Qualtrics Data Set for the US Sample on vote-selling
use "/Users/hectorbahamonde/RU/research/Vote_Selling/data.dta", clear 

rename *, lower
keep age gender reg partyid  trust* knowledge1-knowledge6 q230 q230 q221 q223 q219 cj_1-cj_5 f_* socideo ecideo state zip educ relig race* marital income tknow3_page_submit-tknow6_page_submit q232_1 q232_2

rename trust_1 trustfed
rename trust_2 trustrep
rename trust_3 trustdem

rename knowledge1 roberts
rename knowledge2 supremecourt
rename knowledge3 vp
rename knowledge4 veto
rename knowledge5 majority
rename knowledge6 conservative

rename q232_1 pricecheap
rename q232_2 priceexpensive


recode roberts (2=0) (3=0) (4=0) 
label drop A_37
recode supremecourt (1=0) (2=0) (3=1) (99=0)
label drop A_3C
recode vp (2=0) (3=0) (4=0) (99=0)
label drop A_3H
recode veto (2=0) (3=0) (99=0)
label drop A_3M
recode majority (99=0)
label drop A_3R
recode conservative (99=0)
label drop A_3W

rename q261_page_submit supremecourt_t
rename tknow3_page_submit vp_t
rename tknow4_page_submit veto_t
rename tknow5_page_submit majority_t
rename tknow6_page_submit conservative_t

rename q230 sell1
rename q221 treatment100
rename q223 treatment500
rename q219 control



label define race_1 1 "amindian"
label define race_2 1 "asian"
label define race_3 1 "black"
label define race_4 1 "hisp"
label define race_5 1 "hawaii"
label define race_6 1 "white"
label define race_7 1 "other"
label  values race_1  race_1
label  values race_2  race_2
label  values race_3  race_3
label  values race_4  race_4
label  values race_5  race_5
label  values race_6  race_6
label  values race_7  race_7

drop *first_click* *last_click* *click_count*

* f_*  

*cj_*

saveold "/Users/hectorbahamonde/RU/research/Vote_Selling/data_list.dta", replace  version(12)


***
use "/Users/hectorbahamonde/RU/research/Vote_Selling/data.dta", clear 

rename *, lower

keep f_*_*_* cj_1 cj_2 cj_3 cj_4 cj_5 

order f_1* cj_1 f_2* cj_2 f_3* cj_3 f_4* cj_4 f_5* cj_5 



saveold "/Users/hectorbahamonde/RU/research/Vote_Selling/data_cj.dta", replace version(12)

clear all


