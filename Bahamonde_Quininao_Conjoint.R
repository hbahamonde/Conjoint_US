############################## 
# Cleaning
##############################

## ---- constructing:data
cat("\014")
rm(list=ls())


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)

dat = read.dta("https://github.com/hbahamonde/Conjoint_US/raw/master/data_list.dta")

# Data Cleaning
## Drop if Treatments are Missing

f = c("f_1_1_1",  "f_1_1_2", "f_1_1_3", "f_1_1_4", "f_1_1_5", "f_1_2_1", "f_1_2_2", "f_1_2_3", "f_1_2_4", "f_1_2_5", "f_2_1_1",  "f_2_1_2", "f_2_1_3", "f_2_1_4", "f_2_1_5", "f_2_2_1", "f_2_2_2", "f_2_2_3", "f_2_2_4", "f_2_2_5", "f_3_1_1", "f_3_1_2", "f_3_1_3", "f_3_1_4", "f_3_1_5", "f_3_2_1", "f_3_2_2", "f_3_2_3", "f_3_2_4", "f_3_2_5", "f_4_1_1", "f_4_1_2", "f_4_1_3", "f_4_1_4", "f_4_1_5", "f_4_2_1", "f_4_2_2", "f_4_2_3", "f_4_2_4", "f_4_2_5", "f_5_1_1", "f_5_1_2", "f_5_1_3", "f_5_1_4", "f_5_1_5", "f_5_2_1", "f_5_2_2", "f_5_2_3", "f_5_2_4", "f_5_2_5")

dat = dat[!with(dat,is.na(treatment100) & is.na(treatment500) & is.na(control) |
                        is.na(cj_1) | is.na(cj_2) | is.na(cj_3) | is.na(cj_4) | is.na(cj_5) ),]



## Combine treatment100 & treatment500 & control
dat$ycount = rowSums(data.frame(cbind(dat$control, dat$treatment100, dat$treatment500)), na.rm = T)
## Create treatment dummy variable
dat$treatment[dat$treatment100 != "NA" | dat$treatment500 != "NA"] <- 1 ; dat$treatment[is.na(dat$treatment)] <- 0
# Dummy for the treatment they got
dat$treatlow[dat$treatment100 != "NA"] <- 1 ; dat$treatlow[is.na(dat$treatlow)] <- 0


# Recoding Race
dat$white[dat$race_1 != "NA" | dat$race_2 != "NA" | dat$race_3 != "NA" | dat$race_4 != "NA" | dat$race_5 != "NA"] <- 0 ; dat$white[is.na(dat$white)] <- 1
# Renaming Religion
names(dat)[names(dat) == "relig"] <- "religion"

# renaming male
names(dat)[names(dat) == "gender"] <- "woman" 

# recoding sell (direct sell question)
names(dat)[names(dat) == "sell1"] <- "directquestion" # renaming
dat$directquestion.f <- ordered(dat$directquestion, levels = c(#recoding
        "No, I don't want to sell my vote", 
        "Yes, I want to sell my vote"), labels = c("No", "Yes"))

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(car)

dat$directquestion  <- recode(as.integer(dat$directquestion), "1 = 0 ; 2 = 1")

# Combine (weighted) pol. knowledge vars
dat$polknow = rowSums(data.frame(cbind(
        dat$roberts/dat$tknow1_3, 
        dat$supremecourt/dat$supremecourt_t, 
        dat$vp/dat$vp_t, 
        dat$veto/dat$veto_t,
        dat$majority/dat$majority_t,
        dat$conservative/dat$conservative_t)), na.rm = T)

# Merge ZIP data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)

zipdata <- read.dta("https://github.com/hbahamonde/Conjoint_US/raw/master/zipdata.dta") # import ZIP DTA DATA 
zipdata[zipdata == -1] <- NA
dat = merge(dat, zipdata, by=c("zip"), all.x =T)

## Transforming Vars

levels(dat$socideo)[levels(dat$socideo)=="Very liberal"] <- "VeryLiberal"
levels(dat$socideo)[levels(dat$socideo)=="Very conservative"] <- "VeryConservative"
levels(dat$partyid)[levels(dat$partyid)=="Something else"] <- "SomethingElse"
levels(dat$reg)[levels(dat$reg)=="Now registered to vote"] <- "Registered"
levels(dat$reg)[levels(dat$reg)=="Haven't been able to register so far"] <- "Unregistered"
levels(dat$trustfed)[levels(dat$trustfed)=="No Trust at All"] <- "NoTrustAtAll"
levels(dat$trustfed)[levels(dat$trustfed)=="Not Very Much Trust"] <- "NotVeryMuchTrust"
levels(dat$trustfed)[levels(dat$trustfed)=="Indifferent/In the Middle"] <- "Indifferent"
levels(dat$trustfed)[levels(dat$trustfed)=="Fair Amount of Trust"] <- "FairAmountOfTrust"
levels(dat$trustfed)[levels(dat$trustfed)=="A Great Deal of Trust"] <- "AGreatDealOfTrust"
levels(dat$educ)[levels(dat$educ) == "Some high school"] <- "SomeHighSchool"
levels(dat$educ)[levels(dat$educ) == "High school graduate (high school diploma or equivalent GED)"] <- "HighSchoolGraduate"
levels(dat$educ)[levels(dat$educ) == "Vocational/technical school"] <- "TechnicalSchool"
levels(dat$educ)[levels(dat$educ) == "Some college"] <- "SomeCollege"
levels(dat$educ)[levels(dat$educ) == "Associate degree (usually two years of college)"] <- "AssociateDegree"
levels(dat$educ)[levels(dat$educ) == "Bachelor's degree (usually four years of college)"] <- "BachelorsDegree"
levels(dat$educ)[levels(dat$educ) == "Graduate work (including a Master's degree, law or medical school, or PhD)"] <- "GraduateSchool"

dat$age.n = as.numeric(dat$age) 
dat$income.n = as.numeric(dat$income)
dat$educ.n = as.numeric(dat$educ)

# drop missings THESE ARE THE COVARIATES I AM USING TO ESTIMATE THE LIST PART
sapply(dat, function(x) sum(is.na(x)))
completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
}
dat = completeFun(dat, c("woman", "socideo", "partyid", "reg", "trustfed", "income.n", "educ.n", "polknow")) # variables



dat$proplabforgovtwork = dat$zipgovtworkers / dat$ziplabforc # Ratio Govt Workers/Labor Force ZIP level
dat$sizeofthepoor = rowSums(data.frame(dat$zipless10k,dat$zip1015k,dat$zip1525k)) / dat$ziplabforce # size of the poor
dat$zipinequality = as.numeric(dat$zipmeanincome - dat$zipmedianincome)

# generate the same IDNUM
idnum = data.frame(rep(1:nrow(dat)))
dat = data.frame(c(idnum, dat));colnames(dat)[1] <- "idnum"
dat<-dat[!(dat$idnum=="245"),] # no se por qué chucha el 245 esta vacio por la reconchadesumadre 
dat$idnum = NULL


############################## 
# CONJOINT Experiment DATA CLEANING
##############################
c = dat

# check for complete cases in CONJOINT and LIST treatments
c = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5", "f_1_1_1",  "f_1_1_2", "f_1_1_3", "f_1_1_4", "f_1_1_5", "f_1_2_1", "f_1_2_2", "f_1_2_3", "f_1_2_4", "f_1_2_5", "f_2_1_1",  "f_2_1_2", "f_2_1_3", "f_2_1_4", "f_2_1_5", "f_2_2_1", "f_2_2_2", "f_2_2_3", "f_2_2_4", "f_2_2_5", "f_3_1_1", "f_3_1_2", "f_3_1_3", "f_3_1_4", "f_3_1_5", "f_3_2_1", "f_3_2_2", "f_3_2_3", "f_3_2_4", "f_3_2_5", "f_4_1_1", "f_4_1_2", "f_4_1_3", "f_4_1_4", "f_4_1_5", "f_4_2_1", "f_4_2_2", "f_4_2_3", "f_4_2_4", "f_4_2_5", "f_5_1_1", "f_5_1_2", "f_5_1_3", "f_5_1_4", "f_5_1_5", "f_5_2_1", "f_5_2_2", "f_5_2_3", "f_5_2_4", "f_5_2_5"))

# change names again
all_d = subset(c, select = c("cj_1", "cj_2", "cj_3", "cj_4", "cj_5"))
colnames(all_d)[which(names(all_d) == "cj_1")] <- "d1"
colnames(all_d)[which(names(all_d) == "cj_2")] <- "d2"
colnames(all_d)[which(names(all_d) == "cj_3")] <- "d3"
colnames(all_d)[which(names(all_d) == "cj_4")] <- "d4"
colnames(all_d)[which(names(all_d) == "cj_5")] <- "d5"

c$idnum = rep(1:nrow(c))
nrowc = nrow(c) # I will use this here to entry the number of VALID subjects I've got

# leave main dataframe with just the attributes and idnum
c = subset(c, select = -c(cj_1, cj_2, cj_3, cj_4, cj_5) )

# reshape dataset vertically
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(reshape)

c = melt(c, id="idnum")

# create code variable
c$variable = as.character(c$variable)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(stringr)

c.string = data.frame(str_split_fixed(c$variable, "_", 4))
colnames(c.string) = c("drop", "pair", "candidate", "attribute")

# merge c.string and c dataframes
c = data.frame(c, c.string)
c = subset(c, select = -c(drop, variable, attribute))

# generate a variable which writes the TYPE OF ATTRIBUTE
c$type = ifelse(c$value == "Citizens CAN associate with others and form groups", "RightToAssociate", 
                ifelse(c$value == "Citizens CANNOT associate with others and form groups", "RightToAssociate", 
                       ifelse(c$value == "Citizens CAN run for office for the next two elections", "RightToRun", 
                              ifelse(c$value == "Citizens CANNOT run for office for the next two elections", "RightToRun", 
                                     ifelse(c$value == "Citizens CAN vote in the next two elections", "RightToVote", 
                                            ifelse(c$value == "Citizens CANNOT vote in the next two elections", "RightToVote", 
                                                   ifelse(c$value == "Media CAN confront the Government", "FreePress", 
                                                          ifelse(c$value == "Media CANNOT confront the Government", "FreePress", 
                                                                 ifelse(c$value == "President CAN rule without Congress", "PresAutonomy", 
                                                                        ifelse(c$value == "President CANNOT rule without Congress", "PresAutonomy", NA
                                                                        ))))))))))

###
c$code = paste(c$idnum,c$pair,c$candidate,sep = "-")
colnames(c)[2] <- "caracteristica"
colnames(c)[5] <- "descripcion"
c$descripcion = as.character(c$descripcion)
c$caracteristica = as.character(c$caracteristica)



# D (empty dataset)
idnum = rep(1:nrowc, each = 10)
pair = rep(c(1,1,2,2,3,3,4,4,5,5), times = nrowc) # this is the number of TASKS
candidate = rep(c(1,2), times = (nrowc*10)/2)
at.run = rep(NA,nrowc*10)
at.asso = rep(NA,nrowc*10)
at.press = rep(NA,nrowc*10)
at.presaut = rep(NA,nrowc*10)
at.vote = rep(NA,nrowc*10)
selected = as.character(rep(NA,nrowc*10))
code = paste(idnum,pair,candidate,sep = "-")
d = data.frame(code,idnum,pair,candidate,at.run, at.asso, at.press, at.presaut, at.vote,selected) # conjoint dataset


# Loops to populate d dataset
for (i in code) { # RightToRun
        d$at.run[d$code==i] = c$caracteristica[c$descripcion=="RightToRun" & c$code==i]
}

for (i in code) { # RightToAssociate
        d$at.asso[d$code==i] = c$caracteristica[c$descripcion=="RightToAssociate" & c$code==i]
}


for (i in code) { # FreePress
        d$at.press[d$code==i] = c$caracteristica[c$descripcion=="FreePress" & c$code==i]
}


for (i in code) { # PresAutonomy
        d$at.presaut[d$code==i] = c$caracteristica[c$descripcion=="PresAutonomy" & c$code==i]
}


for (i in code) { # RightToVote
        d$at.vote[d$code==i] = c$caracteristica[c$descripcion=="RightToVote" & c$code==i]
}

# Generate "outcome" dataset

# E
e = dat

idnum.e = rep(1:nrow(e), times = 10)
pair.e = rep(c(1,1,2,2,3,3,4,4,5,5), each = nrow(e)) # this is the number of TASKS
candidate.e = rep(c(1,2), each = nrow(e), times = 5)
code.e = paste(idnum.e, pair.e, candidate.e, sep = "-")

woman.e = c(e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman,e$woman)
socideo.e = c(e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo,e$socideo)
partyid.e = c(e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid,e$partyid)
reg.e = c(e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg,e$reg)
trustfed.e = c(e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed,e$trustfed)
income.n.e = c(e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income,e$income)
educ.n.e = c(e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ,e$educ)
polknow.e = c(e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow,e$polknow)
zipinequality.e = c(e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality,e$zipinequality)
sizeofthepoor.e = c(e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor,e$sizeofthepoor)
proplabforgovtwork.e = c(e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork,e$proplabforgovtwork)
vote.selling.e = c(e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion,e$directquestion)


# match with D
d$woman = rep(NA, nrowc*10)
d$socideo = rep(NA, nrowc*10)
d$partyid = rep(NA, nrowc*10)
d$reg = rep(NA, nrowc*10)
d$trustfed = rep(NA, nrowc*10)
d$income.n = rep(NA, nrowc*10)
d$educ.n = rep(NA, nrowc*10)
d$polknow = rep(NA, nrowc*10)
d$selected = rep(NA, nrowc*10)
d$vote.selling = rep(NA, nrowc*10)

d1.0 = c(e$cj_1)
d1 = c(d1.0,d1.0)
d2.0 = c(e$cj_2)
d2 = c(d2.0,d2.0)
d3.0 = c(e$cj_3)
d3 = c(d3.0,d3.0)
d4.0 = c(e$cj_4)
d4 = c(d4.0,d4.0)
d5.0 = c(e$cj_5)
d5 = c(d5.0,d5.0)

all_d = c(d1,d2,d3,d4,d5)

###

outcome = data.frame(code.e,idnum.e, pair.e, candidate.e,all_d, woman.e, socideo.e, partyid.e, reg.e, trustfed.e, income.n.e, educ.n.e, polknow.e,vote.selling.e)


# LOOPS: populating D dataset
for (i in code) {# selected
        d$selected2[d$code==i] = outcome$all_d[outcome$code.e==i] 
}

for (i in code) {# woman
        d$woman[d$code==i] = outcome$woman[outcome$code==i]
}

for (i in code) {# socideo
        d$socideo[d$code==i] = outcome$socideo[outcome$code==i]
}

for (i in code) {# partyid
        d$partyid[d$code==i] = outcome$partyid[outcome$code==i]
}

for (i in code) {# reg
        d$reg[d$code==i] = outcome$reg[outcome$code==i]
}

for (i in code) {# trustfed
        d$trustfed[d$code==i] = outcome$trustfed[outcome$code==i]
}

for (i in code) {# income.n
        d$income.n[d$code==i] = outcome$income.n[outcome$code==i]
}

for (i in code) {# educ.n
        d$educ.n[d$code==i] = outcome$educ.n[outcome$code==i]
}

for (i in code) {# polknow
        d$polknow[d$code==i] = outcome$polknow[outcome$code==i]
}

for (i in code) {# vote.selling
        d$vote.selling[d$code==i] = outcome$vote.selling[outcome$code==i]
}

# selected
d$vote = ifelse(d$candidate == d$selected2, 1, 0)
d$selected = d$vote
d$selected2 = NULL
d$vote = NULL


# Codebook
attr(d,"codebook") <- as.list(as.character(
        c("code: (a) first number denotes participant id., (b) second number denotes attribute pair, (c) third number denotes hypothetical candidate number/name.", 
        "idnum: subject id.",
        "pair: choice id---the pair attributes presented to the survey participant.",
        "candidate: hypothetical candidate id.",
        "at.run: hypothetical candidate attribute regarding the right to run as a candidate.",
        "at.asso: hypothetical candidate attribute regarding the right to associate and form civil society groups like parties.",
        "at.press: hypothetical candidate attribute regarding the right freedom of the press.",
        "at.presaut: hypothetical candidate attribute regarding presidential autonomy---weather the president can rule without a congress.",
        "at.vote: hypothetical candidate attribute regarding the right of the citizens to vote.",
        "selected: which of the two candidates is selected---the pair is denotes by variable pair.",
        "woman: whether the subject participant, the respondent, is woman or not. 0 otherwise.",
        "socideo: ideology type of the subject participant, the respondent--- 1 extremely lefty, 5 extremely right-wing.",
        "partyid: party identification of the subject participant, the respondent---1: Democrat, 2: Republican, 3: Independent, 4: Others.",
        "reg: weather the subject participant, the respondent, is registered to vote.", 
        "trustfed: How much trust and confidence do you have in the [FEDERAL GOBERNMENT]---1 no trust, 4 fair amount.",
        "income.n: income levels.",
        "educ.n = education levels.",
        "polknow = political knowledge index.",
        "vote.selling = observed but not necesarily unbiased answer to the question of weather survey respondents would be willing to sell their vote."
        )
))

# call codebook
attr(d, "codebook")

# Saving Data
save(d, file = "mergedconjoint.RData")
## ----











###############################################
# Direct CLIENTELISM question plot from LAPOP
###############################################

## ---- lapop:bar:chart:data ----
# cat("\014")
# rm(list=ls())
datLAPOP = read.dta("https://github.com/hbahamonde/Conjoint_US/raw/master/datLAPOP.rdata")
clientelism = datLAPOP$clien1
clientelism <- factor(clientelism, labels = c("Often", "Sometimes", "Never"))
clientelism <- na.omit(clientelism)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)

clientelism = data.table(clientelism)

lapop.bar.chart.N = nrow(clientelism)

percentage.never = round(as.numeric(as.numeric(table(clientelism)["Never"]) * 100) / lapop.bar.chart.N, 1)
percentage.sometimes = round(as.numeric(as.numeric(table(clientelism)["Sometimes"]) * 100) / lapop.bar.chart.N, 1)
percentage.often = round(as.numeric(as.numeric(table(clientelism)["Often"]) * 100) / lapop.bar.chart.N, 1)



if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)
lapop.bar.chart.p = ggplot(clientelism, aes(clientelism)) + 
        geom_bar(stat="count", width = 0.5) + 
        theme_bw() +
        xlab("") + 
        ylab("Subjects (N)") +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom") +
        scale_x_discrete(labels=c(
                paste("Often (", paste(percentage.often, "%", sep = ""), ")", sep = ""),
                paste("Sometimes (", paste(percentage.sometimes, "%", sep = ""), ")", sep = ""),
                paste("Never (", paste(percentage.never, "%", sep = ""), ")", sep = "")
        ))
## ----

## ---- lapop:bar:chart:plot ----
### calling plot
lapop.bar.chart.p
### defining legend, title and notes.
lapop.bar.chart.p.note <- paste(
        "{\\bf Frequency of Clientelism in the United States (2010)}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Figure shows the frequency of survey respondents, N = ", paste(lapop.bar.chart.N, ".", sep = ""), sep = ""),
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Source}: \\href{https://www.vanderbilt.edu/lapop/usa/2010_United_States_Questionnaire.pdf}{LAPOP}, 2010 wave for the United States. Question is \\texttt{clien1}: ``In recent years and thinking about election campaigns, has a candidate or someone from a political party offered you something, like a favor, food, or any other benefit or object in return for your vote or support? Has this happened often, sometimes, or never?''"),
        "\n")
## ----


######################################################
# Descriptive Maps
######################################################

## ---- us:map:plot ----

# Load Data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(RCurl)
myfile <- getURL("https://github.com/hbahamonde/Conjoint_US/raw/master/dat_list.RData")


## Map of Observations

# install package zipcode from source when I run this script during January 2020.
## zipcode was removed from R, and last version (bellow installed) is 1.0 (dated 2012).
install.packages("https://cran.r-project.org/src/contrib/Archive/zipcode/zipcode_1.0.tar.gz", repos=NULL, type="source")


# plyr is dicontinued/retired as it January 2020. Hence, I'll be installing from source
# install.packages("https://cran.r-project.org/src/contrib/plyr_1.8.6.tar.gz", repos=NULL, type="source")
# if (!require("pacman")) install.packages("pacman"); library(pacman) 
# p_load(plyr)

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(zipcode,ggplot2,ggmap)
# install.packages("ggmap", type = "source")library(ggmap) # if it gives the following error ("Error: GeomRasterAnn was built with an incompatible version of ggproto"), install ggmap from source.



data(zipcode)
zipcode <- zipcode[c("zip", "latitude", "longitude")]
dat <- merge(dat,zipcode,by=c("zip"))
us <- c(left = -160, bottom = 15, right = -55, top = 50)
map <- get_stamenmap(us, zoom = 4, maptype = "toner-lite", scale = 2, format = "png")

levels(dat$partyid)[levels(dat$partyid)=="SomethingElse"] <- "Something Else"


ggmap(map) + geom_point(aes(
        x = longitude,
        shape=partyid,
        y = latitude), 
        alpha = .7,
        size = 0.8,
        #shape = 1,
        data = dat) +
        xlab("Longitude") + 
        ylab("Latitude") +
        theme_bw() +
        labs(color='') +
        #scale_colour_manual(values=c("blue", "red", "forestgreen", "cyan1")) +
        scale_color_grey() +
        theme_bw() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom")

## ----


################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(c("Hello","World"), fileConn)
close(fileConn)
## ----


