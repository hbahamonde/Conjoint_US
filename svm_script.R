# Libraries
library(pacman)
p_load(e1071)
library(xlsx)
library(openxlsx)
library(readxl)

# Read merged Conjoint Data
setwd("~/Documents/Research/Hector_Conjoint/analisis_visuales")
load("mergedconjoint.RData")

# numeric variables filter
str2int <- function(x) as.numeric(!grepl("CANNOT",x,fixed=TRUE))
d_mod <- data.frame(lapply(d[5:9], str2int),d[10])

# supervector with individual preferences type='C-classification'
W <- data.frame(NULL)
Training <- data.frame(NULL)
for (k in seq(1,max(d$idnum))){
  cand1 <- subset(d,idnum==k & candidate==1,c("at.run","at.asso","at.press","at.presaut","at.vote"))
  cand2 <- subset(d,idnum==k & candidate==2,c("at.run","at.asso","at.press","at.presaut","at.vote"))
  cand1 <- data.frame(lapply(cand1,str2int))
  cand2 <- data.frame(lapply(cand2,str2int))
  X_train <- cand1-cand2
  sel1 <- subset(d,idnum==k & candidate==1,"selected")
  sel2 <- subset(d,idnum==k & candidate==2,"selected")
  y_train <- sel1-sel2
  
  x <- as.matrix(X_train)
  y <- as.matrix(y_train)
  w_answer <- getsvm(x)
  w_answer <- c(k,w_answer)
  W <- rbind(W,w_answer)
  
  training_data <- data.frame(X_train,y_train)
  Training <- rbind(Training,training_data)
  
  print(k)
}
names(W)<- c("k","w1","w2","w3","w4","w5")
A <- cbind(W,Training)

wb <- createWorkbook("weights_20210111(scratch).xlsx")
addWorksheet(wb,"Pesos")
writeData(wb,sheet="Pesos",W,startCol=1,startRow=1,rowNames = FALSE)
saveWorkbook(wb, file = "weights_20210111(scratch).xlsx", overwrite = TRUE)

wb <- createWorkbook("weights_and_data_20210111(scratch).xlsx")
addWorksheet(wb,"Weights n Data")
writeData(wb,sheet="Weights n Data",A,startCol=1,startRow=1,rowNames = FALSE)
saveWorkbook(wb, file = "weights_and_data_20210111(scratch).xlsx", overwrite = TRUE)

#write.csv(W,".csv", row.names = TRUE)

svm_gradient<- function(x,eta=0.001,R=10000){
  X <- x
  n <- nrow(X)  #number of sample
  p <- ncol(X) #number of feature+1 (bias)
  w_intial <- rep(0,p)
  W <- matrix(w_intial ,nrow = R+1,ncol = p,byrow = T) #matrix put intial guess and the procedure to do gradient descent
  for(i in 1:R){
    for(j in 1:p)
    {
      W[i+1,j]<- W[i,j]+eta*sum(((y*(X%*%W[i,]))<1)*1 * y * X[,j] )  
    }
  }
  return(W)  
}

getsvm <- function(x){
  w_answer<- svm_gradient(x)[nrow(svm_gradient(x)),]
  return(w_answer )
}