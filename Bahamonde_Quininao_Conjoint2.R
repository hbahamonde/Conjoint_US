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


################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(c("Hello","World"), fileConn)
close(fileConn)
## ----



############################## 
# CONJOINT Experiment DATA ANALYSES
##############################
cat("\014")
rm(list=ls())

# Load Data
load("/Users/hectorbahamonde/research/Conjoint_US/mergedconjoint.RData") # Load data


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

# make outcome numeric
d$selected <- as.numeric(d$selected)

# make treatments factors
d$at.run = as.factor(d$at.run)
d$at.asso = as.factor(d$at.asso)
d$at.press = as.factor(d$at.press)
d$at.presaut = as.factor(d$at.presaut)
d$at.vote = as.factor(d$at.vote)


# change reference ctegories
d <- within(d, at.run <- relevel(at.run, ref = 2))
d <- within(d, at.asso <- relevel(at.asso, ref = 2))
d <- within(d, at.press <- relevel(at.press, ref = 2))
d <- within(d, at.presaut <- relevel(at.presaut, ref = 1))
d <- within(d, at.vote <- relevel(at.vote, ref = 2))

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


acme.d <- data.frame(coefficients = c(acme.1[2], acme.2[2],acme.3[2],acme.4[2],acme.5[2]),
                     se = c(acme.1[4], acme.2[4],acme.3[4],acme.4[4],acme.5[4]),
                     variable = c(
                             "Citizens CAN run for office for the next two elections", 
                             "Citizens CAN associate with others and form groups",
                             "Media CAN confront the Government",
                             "President CANNOT rule without Congress",
                             "Citizens CAN vote in the next two elections"
                     )
)
acme.d$upper <-acme.d$coefficient + 1.96*acme.d$se
acme.d$lower <-acme.d$coefficient - 1.96*acme.d$se



acme.0 = data.frame(
        variable = c("Citizens CANNOT run for office for the next two elections", 
                     "Citizens CANNOT associate with others and form groups",
                     "Media CANNOT confront the Government",
                     "President CAN rule without Congress",
                     "Citizens CANNOT vote in the next two elections"),
        coefficients = c(rep(NA, times = 5)), 
        se = c(rep(NA, times = 5)),
        upper = c(rep(NA, times = 5)),
        lower = c(rep(NA, times = 5))
)

acme.d = rbind(acme.d,acme.0)

acme.d$attribute = c(
        "Right To Run", "Right to Associate", "Free Press", "President Autonomy", "Right to Vote",
        "Right To Run", "Right to Associate", "Free Press", "President Autonomy", "Right to Vote"
)




# Plot
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(ggplot2)

ggplot() + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
        geom_pointrange(data=acme.d, 
                        mapping=aes(x=variable, 
                                    y=coefficients, 
                                    ymin=upper, 
                                    ymax=lower),
                        #shape=22, 
                        fill = "WHITE") +
        coord_flip() + 
        xlab("") + 
        ylab("Coefficient") +
        guides(colour=FALSE) +
        theme(legend.position="none") + 
        theme_bw()

######################################################
# Descriptive Maps
######################################################
cat("\014")
rm(list=ls())

## ---- us:map:plot ----

# Load Data
load("/Users/hectorbahamonde/RU/research/Conjoint_US/dat_list.RData") # Load data


## Map of Observations

# install package zipcode from source when I run this script during January 2020.
## zipcode was removed from R, and last version (bellow installed) is 1.0 (dated 2012).
install.packages("https://cran.r-project.org/src/contrib/Archive/zipcode/zipcode_1.0.tar.gz", repos=NULL, type="source")


# plyr is dicontinued/retired as it January 2020. Hence, I'll be installing from source
# install.packages("https://cran.r-project.org/src/contrib/plyr_1.8.6.tar.gz", repos=NULL, type="source")
library(plyr)

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


######################################################
# Descriptive Maps: Vote Sellers
######################################################
cat("\014")
rm(list=ls())

## ---- us:map:vote:selling:data ----

# Load Data
load( "/Users/hectorbahamonde/RU/research/Conjoint_US/mergedconjoint_with_predicted_voteselling.RData") # Load data


## Map of Observations
### loading a package with ZIP codes and their respective Lat's and Long's.
if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(zipcode,ggplot2,ggmap)




data(zipcode)
zipcode <- zipcode[c("zip", "latitude", "longitude")]
dat.with.predict <- merge(dat.with.predict,zipcode,by=c("zip"))
us <- c(left = -160, bottom = 15, right = -55, top = 50)
map <- get_stamenmap(us, zoom = 4, maptype = "toner-lite", scale = 2, format = "png") # maptype = "toner-lite"


colnames(dat.with.predict)[which(names(dat.with.predict) == "fit")] <- 'Probability of Vote Selling'


us.map.vote.selling.plot <- ggmap(map) + geom_point(aes(
        x = longitude,
        #colour=fit,
        y = latitude,
        colour = `Probability of Vote Selling`), 
        #colour = "red",
        alpha = .4,
        #size = 0.8,
        #shape = 21,
        data = dat.with.predict[dat.with.predict$sign==1,]) +
        xlab("Longitude") + 
        ylab("Latitude") +
        theme_bw() +
        #labs(color='') +
        theme_bw() + 
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=7), 
              axis.title.y = element_text(size=7), 
              axis.title.x = element_text(size=7), 
              legend.text=element_text(size=7), 
              legend.title=element_text(size=7),
              plot.title = element_text(size=7),
              legend.position="bottom") +
        scale_colour_gradient(low = "green", high = "red")
## ----

## ---- us:map:vote:selling:plot ----
us.map.vote.selling.plot
us.map.vote.selling.note <- paste(
        "{\\bf Mapping (Predicted) Vote-Sellers}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}: Figure shows the geographical location (at the ZIP code level) of estimated vote-sellers. Using the estimations in \\autoref{tab:regression}, individual probabilities of vote-selling were obtained (see \\autoref{fig:list:analysis:individual:predictions:plot}). This map shows the geographical location of the estimations that are statistically significant only (N = ", paste(length(dat.with.predict$'Probability of Vote Selling'[dat.with.predict$sign==1])), ").", sep = ""),
        "\n")
## ----





##############
# Plot Individual predictions (both High and Low conditions)
##############



## ---- list:analysis:individual:predictions:data  ----
### Individual posterior likelihoods of vote-selling (high)
list.high.predicted.2B <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)
list.high.predicted.2B$fit<-round(list.high.predicted.2B$fit, 2)
list.high.predicted.2B$se.fit<-round(list.high.predicted.2B$se.fit, 2)
indpred.p.high = data.frame(
        list.high.predicted.2B$fit, 
        list.high.predicted.2B$se.fit, 
        Significance = as.numeric(ifelse(sign(list.high.predicted.2B$fit$lwr) == sign(list.high.predicted.2B$fit$up), 1,0)))
indpred.p.high$Significance[indpred.p.high$Significance==1] <- "Yes"
indpred.p.high$Significance[indpred.p.high$Significance==0] <- "No"
names(indpred.p.high)[4] = "se.fit"
rownames(indpred.p.high) <- NULL
indpred.p.high.fit= indpred.p.high$fit

### Individual posterior likelihoods of vote-selling (low)
list.low.predicted.2B <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)
list.low.predicted.2B$fit<-round(list.low.predicted.2B$fit, 2)
list.low.predicted.2B$se.fit<-round(list.low.predicted.2B$se.fit, 2)
indpred.p.low = data.frame(
        list.low.predicted.2B$fit, 
        list.low.predicted.2B$se.fit, 
        Significance = as.numeric(ifelse(sign(list.low.predicted.2B$fit$lwr) == sign(list.low.predicted.2B$fit$upr), 1,0)))
indpred.p.low$Significance[indpred.p.low$Significance==1] <- "Yes"
indpred.p.low$Significance[indpred.p.low$Significance==0] <- "No"
names(indpred.p.low)[4] = "se.fit"
rownames(indpred.p.low) <- NULL
indpred.p.low.fit= indpred.p.low$fit


# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,grid,gridExtra)




## Low
ind.pred.low.cond.plot = ggplot() + geom_pointrange(data=indpred.p.low, 
                                                    mapping =aes(
                                                            x=1:nrow(indpred.p.low), 
                                                            y=indpred.p.low$fit, 
                                                            ymin=indpred.p.low$lwr, 
                                                            ymax=indpred.p.low$upr, 
                                                            colour = Significance), 
                                                    size=0.25, 
                                                    alpha=.5) + 
        #theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling\n(Low Condition)") +
        #guides(colour=FALSE) + 
        theme_bw() +
        scale_colour_grey() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=12), 
              axis.title.y = element_text(size=18), 
              axis.title.x = element_text(size=18), 
              legend.text=element_text(size=18), 
              legend.title=element_text(size=18),
              plot.title = element_text(size=7),
              legend.position="bottom")

## High
ind.pred.high.cond.plot = ggplot() + geom_pointrange(data=indpred.p.high, 
                                                     mapping =aes(
                                                             x=1:nrow(indpred.p.high), 
                                                             y=indpred.p.high$fit, 
                                                             ymin=indpred.p.high$lwr, 
                                                             ymax=indpred.p.high$upr, 
                                                             colour = indpred.p.high$Significance), 
                                                     size=0.25, 
                                                     alpha=.5) + 
        #theme(legend.position="none") + 
        geom_hline(yintercept=0, colour = "red", linetype = "dashed", size = 0.9) +
        xlab("Observations") + 
        ylab("Probability of Vote-Selling\n(High Condition)") +
        #guides(colour=FALSE) + 
        theme_bw() +
        scale_colour_grey() +
        theme(axis.text.y = element_text(size=7), 
              axis.text.x = element_text(size=12), 
              axis.title.y = element_text(size=18), 
              axis.title.x = element_text(size=18), 
              legend.text=element_text(size=18), 
              legend.title=element_text(size=18),
              plot.title = element_text(size=7),
              legend.position="bottom")

# computing the sample size of the list experiment (which also determines the sample size in the conjoint portion) for the paper
total.sample.size = as.character(formatC(c(nrow(indpred.p.high) + nrow(indpred.p.low)), format="d", big.mark=","))
# this here converts well into RNW if \Sexpr{} is not between $$ signs.

## merging the two plots
# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra)



# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
        require(ggplot2)
        require(gridExtra)
        plots <- list(...)
        g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        grid.arrange(
                do.call(arrangeGrob, lapply(plots, function(x)
                        x + theme(legend.position="none"))),
                legend,
                ncol = 1,
                heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: High Condition
ind.pred.social.desirability.high <- predict.ictreg(list.high, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)

ind.pred.social.desirability.high$fit<-round(ind.pred.social.desirability.high$fit, 10)
ind.pred.social.desirability.high$se.fit<-round(ind.pred.social.desirability.high$se.fit, 10)
ind.pred.social.desirability.high.d = data.frame(
        ind.pred.social.desirability.high$fit, 
        ind.pred.social.desirability.high$se.fit, 
        sign = ifelse(sign(ind.pred.social.desirability.high$fit$lwr) == sign(ind.pred.social.desirability.high$fit$upr), 1, 0))
names(ind.pred.social.desirability.high.d)[4] = "se.fit"
rownames(ind.pred.social.desirability.high.d) <- NULL


# USE THE VECTOR WITH INDIVUDUAL PREDICTIONS: Low Condition
ind.pred.social.desirability.low <- predict.ictreg(list.low, se.fit = TRUE, interval= "confidence", avg = F, return.draws = T, level = ci.level)

ind.pred.social.desirability.low$fit<-round(ind.pred.social.desirability.low$fit, 10)
ind.pred.social.desirability.low$se.fit<-round(ind.pred.social.desirability.low$se.fit, 10)
ind.pred.social.desirability.low.d = data.frame(
        ind.pred.social.desirability.low$fit, 
        ind.pred.social.desirability.low$se.fit, 
        sign = ifelse(sign(ind.pred.social.desirability.low$fit$lwr) == sign(ind.pred.social.desirability.low$fit$upr), 1, 0))
names(ind.pred.social.desirability.low.d)[4] = "se.fit"
rownames(ind.pred.social.desirability.low.d) <- NULL

# cbind regular DFs (with the two conditions) with the predictions
dat.low.with.predict = data.frame(cbind(dat.low, ind.pred.social.desirability.low.d))
dat.high.with.predict = data.frame(cbind(dat.high, ind.pred.social.desirability.high.d))
dat.with.predict = data.frame(rbind(dat.low.with.predict, dat.high.with.predict))

# Saving Data
save(dat.with.predict, file = "/Users/hectorbahamonde/RU/research/Conjoint_US/mergedconjoint_with_predicted_voteselling.RData")
## ---- 



## ---- list:analysis:individual:predictions:plot  ----
grid_arrange_shared_legend(
        ind.pred.low.cond.plot, 
        ind.pred.high.cond.plot,
        ncol = 1, nrow = 2)

individual.predictions.plot.note <- paste(
        "{\\bf Individual Estimated Probabilities of Vote-Selling}.",
        "\\\\\\hspace{\\textwidth}", 
        paste(paste(paste(paste("{\\bf Note}: Figure shows the individual probabilities of vote-selling (N = ", total.sample.size, ")",  sep = ""), sep = ""), "under the ``low'' and ``high'' conditions. After fitting the model, and following the advice of \\textcite[]{Blair2012} and \\textcite[]{Imai2014a}, individual probabilities of vote-selling under the ``low'' and ``high'' conditions were estimated. ", paste("The figure also shows", paste(ci.level*100,"\\%", sep = ""), "confidence intervals.", sep = " "))),
        "\n")
## ----





