############################## 
# Cleaning
##############################

## ---- constructing:data
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
          "at.press: hypothetical candidate attribute regarding the right of freedom of the press.",
          "at.presaut: hypothetical candidate attribute regarding presidential autonomy---weather the president can rule without a congress.",
          "at.vote: hypothetical candidate attribute regarding the right of the citizens to cast ballots",
          "selected: which of the two candidates is selected---the pair is denotes by variable pair.",
          "woman: whether the subject participant, the respondent, is a woman. 0 otherwise.",
          "socideo: ideology type of the subject participant, the respondent--- 1 extremely left-wing, 5 extremely right-wing.",
          "partyid: party identification of the subject participant, the respondent---1: Democrat, 2: Republican, 3: Independent, 4: Other.",
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

# total.sample.size
total.sample.size.c = as.character(formatC(c(nrow(d)/10), format="d", big.mark=","))
total.sample.size.n = nrow(d)/10
tasks = 5
candidates = 2

# Saving Data
save(d, file = "mergedconjoint.RData")
## ----











###############################################
# Direct CLIENTELISM question plot from LAPOP
###############################################

## ---- lapop:bar:chart:data ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(foreign)
datLAPOP = read.dta("https://github.com/hbahamonde/Conjoint_US/raw/master/datLAPOP.dta")
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

## ---- us:map:plot:d ----
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(zipcode,ggplot2,ggmap)

data(zipcode)
zipcode <- zipcode[c("zip", "latitude", "longitude")]
dat <- merge(dat,zipcode,by=c("zip"))
us <- c(left = -160, bottom = 15, right = -55, top = 50)
map <- get_stamenmap(us, zoom = 4, maptype = "toner-lite", scale = 2, format = "png")

levels(dat$partyid)[levels(dat$partyid)=="SomethingElse"] <- "Something Else"


us.map = ggmap(map) + geom_point(aes(
        x = longitude,
        shape=partyid,
        y = latitude), 
        alpha = .7,
        size = 0.8,
        #shape = 1,
        data = dat) +
        xlab("Longitude") + 
        ylab("Latitude") +
        #labs(color='') +
        #scale_colour_manual(values=c("blue", "red", "forestgreen", "cyan1")) +
        # scale_color_grey() +
        theme_bw() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_blank(),
              plot.title = element_text(size=7),
              legend.position="bottom")
## ----


## ---- us:map:plot ----
### calling plot
us.map
### defining legend, title and notes.
us.map.note <- paste(
        "{\\bf Geographical Distribution of Survey Respondents by Party Identification}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: The data (N=",total.sample.size.c,") were collected by \\emph{Research Now SSI} between March 2 and March 6 2016 and are representative at the national level. Survey respondents belong to the online panel owned and administered by SSI.", sep = ""),
        "\n")
## ----



############################## 
# CONJOINT Experiment DATA ANALYSES
##############################
cat("\014")
rm(list=ls())

# Load Data
load("/Users/hectorbahamonde/research/Conjoint_US/mergedconjoint.RData") # Load data

## ---- amce:plot:d ----

# example script to implement estimators of Average Marginal Component Effects (ACMEs) for Conjoint Data
# developed in :
# Causal Inference in Conjoint Analysis:
# Understanding Multidimensional Choices via Stated Preference Experiments
# Jens Hainmueller, Daniel Hopkins, Teppei Yamamoto

# function that does clustered SEs
vcovCluster <- function(
        model,
        cluster
)
{
        require(sandwich)
        require(lmtest)
        if(nrow(model.matrix(model))!=length(cluster)){
                stop("check your data: cluster variable has different N than model")
        }
        M <- length(unique(cluster))
        N <- length(cluster)           
        K <- model$rank   
        if(M<50){
                warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
        }
        dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
        uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
        rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
        return(rcse.cov)
}

if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(lmtest,sandwich,msm)

############################## 
# Selected Candidate
##############################

# make outcome numeric
d$selected <- as.numeric(d$selected)

# make treatments factors
d$at.run = as.factor(d$at.run)
d$at.asso = as.factor(d$at.asso)
d$at.press = as.factor(d$at.press)
d$at.presaut = as.factor(d$at.presaut)
d$at.vote = as.factor(d$at.vote)


# change reference ctegories // Reference is DEMOCRATIC value, so we see the effect of UNDEMOCRATIC treat on being elected.
d <- within(d, at.run <- relevel(at.run, ref = "Citizens CAN run for office for the next two elections" ))
d <- within(d, at.asso <- relevel(at.asso, ref = "Citizens CAN associate with others and form groups"))
d <- within(d, at.press <- relevel(at.press, ref = "Media CAN confront the Government"))
d <- within(d, at.presaut <- relevel(at.presaut, ref = "President CANNOT rule without Congress"))
d <- within(d, at.vote <- relevel(at.vote, ref = "Citizens CAN vote in the next two elections"))

model.1 = lm(selected ~ at.run, data=d)
model.2 = lm(selected ~ at.asso, data=d)
model.3 = lm(selected ~ at.press, data=d)
model.4 = lm(selected ~ at.presaut, data=d)
model.5 = lm(selected ~ at.vote, data=d)

acme.1 = coeftest(model.1, vcov = vcovCluster(model.1, cluster = d$idnum))
acme.2 = coeftest(model.2, vcov = vcovCluster(model.2, cluster = d$idnum))
acme.3 = coeftest(model.3, vcov = vcovCluster(model.3, cluster = d$idnum))
acme.4 = coeftest(model.4, vcov = vcovCluster(model.4, cluster = d$idnum))
acme.5 = coeftest(model.5, vcov = vcovCluster(model.5, cluster = d$idnum))



############################## 
# DF AMCE
##############################

acme.d <- data.frame(
        nrow = c(1:5),
        coefficients = c(
                # Candidate Selected
                acme.3[2], # press
                acme.4[2], # presaut
                acme.5[2], # vote
                acme.1[2], # run 
                acme.2[2]), # asso
        se = c(
                # Candidate Selected
                acme.3[4], # press
                acme.4[4], # presaut
                acme.5[4], # vote
                acme.1[4], # run 
                acme.2[4]), # asso
        variable = as.factor(c(
                # Candidate Selected
                "Media CANNOT confront the Government", # press
                "President CAN rule without Congress", # presaut
                "Citizens CANNOT vote in the next two elections", # vote
                "Citizens CANNOT run for office for the next two elections", # run 
                "Citizens CANNOT associate with others and form groups" # asso
                )),
        Model = c(rep("Candidate Selected", 5))
        )


acme.d$upper <- acme.d$coefficient + 1.96*acme.d$se
acme.d$lower <- acme.d$coefficient - 1.96*acme.d$se


p_load(forcats, tidyverse)
acme.d = acme.d %>% mutate(variable = fct_rev(fct_reorder(variable,nrow)))


# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

amce.plot = ggplot() + 
        geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
        geom_pointrange(data=acme.d, 
                        mapping=aes(x=variable, 
                                    y=coefficients, 
                                    ymin=upper, 
                                    ymax=lower),
                        shape=22, 
                        fill = "WHITE") +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        theme_bw() +
        theme(axis.text.y = element_text(size=9), 
              axis.text.x = element_text(size=9), 
              axis.title.y = element_text(size=9), 
              axis.title.x = element_text(size=9), 
              legend.text=element_text(size=9), 
              legend.title=element_text(size=9),
              plot.title = element_text(size=9),
              legend.position="bottom")
## ---- 


## ---- amce:plot ----
### calling plot
amce.plot
### defining legend, title and notes.
amce.plot.note <- paste(
        "{\\bf Classic AMCE Analysis: Candidate Selection and Dahl's Democratic Dimensions}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Following \\textcite{Hainmueller2014a}, the figure shows the corresponding AMCEs for every of the attributes explained in \\autoref{tab:dim}. All attributes are based on \\textcite{Dahl1971}. All reference categories were omitted---all of them are at the 0 vertical line and represent the opposite of the attribute shown in the plot. For substantive reasons, all categories displayed in the figure represent the non-democratic side of the attributes. The figure strongly suggests that respondents systematically preferred hypothetical candidates who supported democratic policies."),
        "\n")
## ----

################
#### W Analyses
################
cat("\014")
rm(list=ls())


## ---- w:analyses:d ----

# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(openxlsx,texreg,jtools,stargazer)

options(scipen=9999999) # turn off sci not
w = read.xlsx("https://github.com/hbahamonde/Conjoint_US/raw/master/w.xlsx") # loads data

# Correct Classification
# per.correct.class = round((as.numeric(table(w$X19)["TRUE"])*100)/nrow(w),0)

w = subset(w, select = c(k,w1,w2,w3,w4,w5)) # Keeping only columns I need
colnames(w)[which(names(w) == "k")] <- "idnum" # renames id variable
w = w[!duplicated(w$idnum), ] # dropping duplicates


# merge w's con conjoint dataset
load(url("https://github.com/hbahamonde/Conjoint_US/raw/master/mergedconjoint.RData")) # load data
d = d[!duplicated(d$idnum), ] # dropping duplicates
dat.w = merge(w,d,by="idnum") # merge
dat.w = subset(dat.w, select = c(w1,w2,w3,w4,w5,woman,socideo,partyid,reg,trustfed,income.n,educ.n,polknow,vote.selling)) # Keeping only columns I need

# Labels
## woman: 1 man, 2 woman
## reg: 1 yes, 2 no
## partyid: 1 dem, 2 rep, 3 ind, 4 something else


# party id to factor
dat.w$partyid = factor(dat.w$partyid)
levels(dat.w$partyid) <- c("Democrat","Republican","Independent", "Other")
dat.w$partyid = relevel(dat.w$partyid, ref = "Independent")

# change var name
colnames(dat.w)[which(names(dat.w) == "woman")] <- "Woman"
colnames(dat.w)[which(names(dat.w) == "socideo")] <- "Ideology"
colnames(dat.w)[which(names(dat.w) == "partyid")] <- "Party Id."
colnames(dat.w)[which(names(dat.w) == "reg")] <- "Registered to Vote"
colnames(dat.w)[which(names(dat.w) == "trustfed")] <- "Trust in Federal Gov."
colnames(dat.w)[which(names(dat.w) == "income.n")] <- "Income"
colnames(dat.w)[which(names(dat.w) == "educ.n")] <- "Education"
colnames(dat.w)[which(names(dat.w) == "polknow")] <- "Political Knowledge"
colnames(dat.w)[which(names(dat.w) == "vote.selling")] <- "Sell Vote"

colnames(dat.w)[which(names(dat.w) == "w3")] <- "Free Media"
colnames(dat.w)[which(names(dat.w) == "w4")] <- "Presidential Autonomy"
colnames(dat.w)[which(names(dat.w) == "w5")] <- "Right to Vote"
colnames(dat.w)[which(names(dat.w) == "w1")] <- "Right to Run for Office"
colnames(dat.w)[which(names(dat.w) == "w2")] <- "Right to Associate"

# Density plots (w's)
## dataset
density.plot.w.d = data.frame(
        Value = as.vector(rbind(dat.w$`Free Media`,
                                dat.w$`Presidential Autonomy`,
                                dat.w$`Right to Vote`,
                                dat.w$`Right to Run for Office`,
                                dat.w$`Right to Associate`
                                )),
        Dimension = as.factor(as.vector(rbind(rep("Free Media", nrow(dat.w)),
                      rep("Presidential Autonomy", nrow(dat.w)),
                      rep("Right to Vote", nrow(dat.w)),
                      rep("Right to Run for Office", nrow(dat.w)),
                      rep("Right to Associate", nrow(dat.w))
                      )))
        )

p_load(ggplot2)

density.plot.w.p = ggplot(density.plot.w.d, aes(x=Value, fill=Dimension)) + geom_density(alpha=0.4) + theme_bw() +
        xlab("Afinity (w)") + 
        ylab("Density") +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="none") +
        facet_grid(. ~ Dimension)


# OLS

## define iv's
independent.variables = paste0(' `Sell Vote` + Woman + `Party Id.` + Ideology + Education + `Political Knowledge` + `Registered to Vote` + `Trust in Federal Gov.` + Income ')


## fit models
m1 = lm(paste(' `Free Media` ~ ', independent.variables), dat.w) # Free Media
m2 = lm(paste(' `Presidential Autonomy` ~ ', independent.variables), dat.w) # Presidential Autonomy
m3 = lm(paste(' `Right to Vote`  ~ ', independent.variables), dat.w) # Right to Vote
m4 = lm(paste(' `Right to Run for Office` ~ ', independent.variables), dat.w) # Right to Run for Office
m5 = lm(paste(' `Right to Associate` ~ ', independent.variables), dat.w) # Right to Associate
## ----


## ---- w:analyses:p:d ----
# p_load(jtools)
w.analyses.p = plot_summs(m1,m2,m3,m4,m5, legend.title="Democracy Dimension", colors = "Rainbow", point.shape = F, scale = TRUE, model.names = c("Free Media", "Presidential Autonomy", "Right to Vote", "Right to Run for Office", "Right to Associate"))
## ----

## ---- w:analyses:p:p ----
### calling plot
w.analyses.p
### defining legend, title and notes.
w.analyses.p.note <- paste(
        "{\\bf SVM Analysis: Vote Selling and \\textcite{Dahl1971}'s Democracy Dimensions}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: The figure shows OLS models where PENDING. \\autoref{w:analyses:t} shows the respective regression table."),
        "\n")
## ----



## ---- density:plot:w:p ----
### calling plot
density.plot.w.p
### defining legend, title and notes.
density.plot.w.p.note <- paste(
        "{\\bf SVM Analyses: Five Democracy Attributes \\parencite{Dahl1971}}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: The figure shows the five dependent variables used in \\autoref{w:analyses:t}."),
        "\n")
## ----


## ---- w:analyses:t ----
texreg(list(m1,m2,m3,m4,m5), custom.model.names=c("Free Media", "Presidential Autonomy", "Right to Vote", "Right to Run for Office", "Right to Associate"), label = "w:analyses:t", custom.note= c("Every column represents each of \\textcite{Dahl1971} democracy dimensions. All models OLS. Intercept omitted."), scalebox=0.7, use.packages = F, omit.coef="(Intercept)", float.pos="H")
## ----


## ---- summary:stats:t ----
stargazer(dat.w, label = "summary:stats:t")
## ----


################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(c("This paper explains that democracy has been theorized as a multidimensional concept. Yet, the quantitative study of clientelism---as a democracy failure---has been studied almost exclusively from a unidimensional perspective. For instance, list experiments usually study one aspect at a time by manipulating a word, a sentence or a framing. We argue that to better understand clientelism quantitative studies should situate the phenomena within the multidimensionality of democracy. This paper makes both methodological and substantive contributions to the literature by leveraging a conjoint experiment on hypothetical vote selling in a consolidated democracy. Conjoint designs ask respondents to choose from hypothetical profiles that combine multiple attributes. To study which democratic dimension(s) should fail to produce clientelism, we presented subjects two hypothetical candidates that supported (or not) every policy (attribute). Using machine learning techniques, we identify which dimensions should ``fail'' to produce likely vote-sellers."), fileConn)
close(fileConn)
## ----


