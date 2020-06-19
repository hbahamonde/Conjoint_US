* Import, Clean and Merge Socio-Demographic ZIP info from US Census

import delimited "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/CENSUS/ZIP_population.csv", clear

rename v1 zip
rename v2  zipstate
rename v3  ziptotpop
rename v4  zipurbpop
rename v5  ziprurpop
rename v6  zipperurban
rename v7  zipperrural
rename v8  ziphousingtot
rename v9  ziphousingurban
rename v10 ziphousingrural
rename v11 zipperhousurb
rename v12 zipperhousrur
rename v13 ziplandareasqml
rename v14 zipwaterareasqml
rename v15 zippopdensity

note: ZIP Code Urban/Rural Geography & Demographics. From http://proximityone.com/zip_urban_rural.htm

save population, replace

clear all

import delimited "/Users/hectorbahamonde/RU/research/Vote_Selling/US/Data/CENSUS/ZIP_econ.csv", clear


keep v1 v5 v12 v51 v55-v66 v89-v91 

rename v1 zip
rename v5 ziplabforce
rename v12 zipperumepl
rename v51 zipgovtworkers
renvars v55-v66 \ zipless10k zip1015k zip1525k zip2535k zip3550k zip5075k zip75100k zip100150k zip150200k zip200morek zipmedianincome zipmeanincome
renvars v89-v91 \ zipmedianfamincome zipmeanfamincome zippercamincome

note: It's projection for 2012, from  http://proximityone.com/zip12dp3.htm

save economic, replace

* merge

merge 1:m zip using  population

 * drop

drop zipstate _merge

saveold zipdata
