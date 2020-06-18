############  May, 24th 2020 - São Paulo - Brazil ########################
############  Newcomb-Benford Law applied to Brazilian Welfare Program - Bolfa Familia #######
############  Developed by Caio Azevedo ########################


#https://insightr.wordpress.com/2018/11/17/benfords-law-for-fraud-detection-with-an-application-to-all-brazilian-presidential-elections-from-2002-to-2018/
#Benford Law applied do Bolfa Familia Programme
#https://github.com/cran/benford.analysis/blob/master/R/functions-new.R
#Esse artigo contesta a validade do teste da soma
#https://vixra.org/pdf/1809.0158v1.pdf

#*************** Best practice - Initial Commands ***************
cat("\014") #Clean console screen
rm(list=ls()) #clean all variables on global environment


library(dplyr) #used to group dataset
#*************** Best practice - Initial Commands ***************
library(benford.analysis)

#used to substr function
library(tidyverse)

#used to advanced bar chat
library(ggplot2)

#used to compute skewness
library(e1071)

prepare_grouped_data <- function(myDataSet) {
  myDataSet$Withdrawal.Value = as.numeric(myDataSet$Withdrawal.Value)
  rem = ifelse(myDataSet$Withdrawal.Value>=10,1,NA)
  myDataSet$fd = as.numeric(str_sub(myDataSet$Withdrawal.Value,1,1))
  myDataSet$sd = as.numeric(str_sub(myDataSet$Withdrawal.Value,2,2))*rem
  myDataSet$f2d = as.numeric(str_sub(myDataSet$Withdrawal.Value,1,2))*rem
  
  myDataSet <- na.omit(myDataSet) #data cleaning - blank, NaN values removed 
  output = list()
  
  d1 = myDataSet%>% group_by(fd) %>% count()
  d2 = (myDataSet%>% group_by(sd) %>% count())[1:10,]
  d12 = (myDataSet%>% group_by(f2d) %>% count())[1:90,]
  
  n1 = nrow(myDataSet)
  n2 = n12 = nrow(filter(myDataSet,!is.na(sd)))
  
  df1 = data.frame(1:9,d1[,2]/n1,d1[,2],n1)
  df2 = data.frame(0:9,d2[,2]/n2,d2[,2],n2)
  df12 = data.frame(10:99,d12[,2]/n12,d12[,2],n12)
  
  colnames(df1) = colnames(df2) = colnames(df12) = c("digit", "prop", "count","n")
  output = list(first = df1, second = df2, fands = df12)
  
  return(output)
  
}

myFirstDigitDoubleBarChartPresention <- function(df){
  
  Expected =df$first$n*log10(1+1/1:9)
  Observed = df$first$count
  
  benford1 <- rbind(Observed,Expected)   
  
  data <- matrix(benford1 , nrow=2)
  colnames(data) <- c(1:9)
  rownames(data) <- c("Observed data","Benford Expected")
  data
  barplot(benford1, 
          col=c("#FF9900","#0066FF") , 
          border="white", 
          font.axis=2, 
          beside=T, 
          legend=rownames(data), 
          xlab="group", 
          font.lab=2)
}

myFirstDigitBarLineChartPresention <- function(dt){
  benford1 = log10(1+1/1:9)
  names(benford1) = 1:9
  
  bar = barplot(benford1, ylim = c(0,0.4), main = "Bolsa Familia First Digit Analysis")
  lines(bar,dt$first$prop,type ="b", col = 1)
  legend("topright", legend = "municipalities payment", col = c(2:(length(dt)+1)), cex = 0.6, seg.len = 0.4 ,lty = 1,bty = "n")
}

myFirstandSecondDigitBarLineChartPresention <- function(dt){
  benford12 = log10(1+1/10:99)
  names(benford12) = 10:99
  
  #bar = barplot(benford12, ylim = c(0,0.06), main = "Bolsa Familia First and Second Digits")
  #lines(bar,dt$fands$prop,type ="b", col = 1)
  
  bar = barplot(dt$fands$prop, ylim = c(0,0.06), main = "Bolsa Familia First and Second Digits Analysis")
  lines(bar,benford12,type ="b", col = 1)
  
  
  legend("topright", legend = "municipalities payment", col = c(2:(length(dt)+1)), cex = 0.6, seg.len = 0.4 ,lty = 1,bty = "n")
}

myFibonnaciSerie <- function(len){
  fibo <- data.frame("Withdrawal.Value"=1, "value"=1)
  fibo <- rbind(fibo, c(1, 1))
  fibvals <- numeric(len)
  fibvals[1] <- 1
  fibvals[2] <- 1
  for (i in 3:len) { 
    fibvals[i] <- fibvals[i-1]+fibvals[i-2]
    fibo <- rbind(fibo, c(fibvals[i], fibvals[i]))
  } 
  return(fibo)
}

myPrepareDataSet2ZandSummationTests <- function(Result) {
  myDataSet <- data.frame()
  for (row in 1:nrow(Result$bfd)){
    myDataSet[row,"digits"] <- Result$bfd[row]$digits
    myDataSet[row,"observed_distribution"] <- Result$bfd[row]$data.dist
    myDataSet[row,"expected_distribution"] <- Result$bfd[row]$benford.dist
    
    myDataSet[row,"observed_frequency"] <- Result$bfd[row]$data.dist.freq
    myDataSet[row,"expected_frequency"] <- Result$bfd[row]$benford.dist.freq
    myDataSet[row,"summation"] <- Result$bfd[row]$data.summation
    myDataSet[row,"excess.summation"] <- Result$bfd[row]$abs.excess.summation
  }
  
  return(myDataSet)
}

myComputeZStatistics <- function(myDataSet) {
  
  total <- sum(myDataSet$expected_frequency)
  for (row in 1:nrow(myDataSet)){
    
    EP <- myDataSet[row, "expected_distribution"] 
    AP <- myDataSet[row, "observed_distribution"]
    Z <- abs(AP-EP)
    fCorrecao <- 1/(2 * total)
    Z <- Z - fCorrecao
    
    if (Z < 0)
      Z <- Z + fCorrecao
    
    
    Z <- Z / sqrt((EP * (1 - EP)) / total)
    Z <- round(Z,4)
    myDataSet[row,"Z"] <- Z
  }
  
  return(myDataSet)
}

myComputeSummationTest <- function(myDataSet) {
  total_summation <- sum(myDataSet$summation)
  
  for (row in 1:nrow(myDataSet)){
    summation_test <- myDataSet[row, "summation"] 
    summation_test <- summation_test/total_summation
    myDataSet[row,"summation_test"] <- round(summation_test,4)
  }
  return(myDataSet)
}



#Dataset Reading - dataSet as Global Variable
# reading big files https://stackoverflow.com/questions/38536226/r-reading-a-huge-csv
setwd("C:/Users/cazev/OneDrive/Desktop/docs/Mestrado/paper benford law/references/datasources")

################### Use this codification for grouped dataset ######################
#After raw data is grouped use one of this files to further analysis

############ Group data by Municipalities in order to use the second approach ############################
#BolsaFamilia_Municipalities_Jan_2018.csv BolsaFamilia_Municipalities_Mar_2018.csv
#BolsaFamilia_Municipalities_Jan_2019.csv BolsaFamilia_Municipalities_Mar_2019.csv

#csv2 is used due to decimal point is comma format (brazilian format)
dataSet <- read.csv2(file="BolsaFamilia_Municipalities_Jan_2018.csv",sep=',',header=TRUE,fileEncoding = "latin1") 
print(paste("Total of rows from original dataset ", nrow(dataSet)))
dataSet <- na.omit(dataSet) #data cleaning - blank, NaN values removed 
print(paste("Total of rows after empty 'any-attribute' rows cleaning ", nrow(dataSet))) #all records are complete, no missing attibute
head(dataSet,10)
tail(dataSet,10)

df <- prepare_grouped_data(dataSet)
myFirstDigitBarLineChartPresention(df)
myFirstandSecondDigitBarLineChartPresention(df)
myFirstDigitDoubleBarChartPresention(df)

data <- dataSet$Withdrawal.Value
data = as.numeric(data)

bfordResult <- benford(data, 1, discrete=TRUE, sign="both") #generates benford object
plot(bfordResult) #plots
sampleSize <- bfordResult$info$n
chiSquare <- round(chisq(bfordResult)$statistic,3)

print(paste("sample size ", sampleSize))
print(paste("chi-square first digit ", chiSquare))

#First-two digits analysis
bfordResult <- benford(data, 2, discrete=TRUE, sign="both") #generates benford object
plot(bfordResult) #plots

sampleSize <- bfordResult$info$n
chiSquare <- round(chisq(bfordResult)$statistic,4) #gets the Chi-squared test
mad <-round(MAD(bfordResult),3) #gets the Mean Absolute Deviation

total_amount <- sum(data)
skew <- round(skewness(data),3)
mean <- round(mean(data),3)
median <- round(median(data),3)


print(paste("Sample Size ", sampleSize))
print(paste("Sample Mean", mean))
print(paste("Sample Median", median))
print(paste("Sample Skewness", skew))

print(paste("Chi-square first-two digits ", chiSquare))
print(paste("MAD Value ", mad))
print(paste("MAD conformity [", bfordResult$MAD.conformity, "]"))


print(paste("Total amount (R$)", total_amount))


myDataSet <- myPrepareDataSet2ZandSummationTests(bfordResult)
head(myDataSet)
myDataSet <- myComputeZStatistics(myDataSet)
head(myDataSet)

#Scan those that failed Z-statistics test (Z > 1.96, due to statistical significance of 5%)
zStatistics <- subset(myDataSet, Z > 1.96)
head(zStatistics[order(-zStatistics$Z),] %>% select(digits,Z))


myDataSet <- myComputeSummationTest(myDataSet)
#Scan those that failed Summation test (summation > 0.01375, as proposed 25% of upper tolerance)
summationTest <- subset(myDataSet, summation_test > 0.01375)
summationTest <- summationTest[order(-summationTest$summation_test),]

#List top 10 divergence in Summation test)
suspectDigits <- head(summationTest %>% select(digits,observed_frequency,summation, summation_test),10)
suspectDigits

#Given top 10 summation test divergences, select municipalities in which test variable leading digits begin with.
suspectMunicipalities <- c()
for (row in 1:nrow(suspectDigits)){
  p1 <- paste("^",suspectDigits[row,"digits"],sep = "")
  suspectMunicipalities <- rbind(suspectMunicipalities,subset(dataSet[order(-dataSet$Withdrawal.Value),], grepl(p1, Withdrawal.Value)))
}
suspectMunicipalities$X <- NULL
nrow(suspectMunicipalities)

####### Save municipalities under suspect into a csv file ######################
#MunicipalitiesUnderSuspect_Jan_2018.csv MunicipalitiesUnderSuspect_Mar_2018.csv
#MunicipalitiesUnderSuspect_Jan_2019.csv MunicipalitiesUnderSuspect_Mar_2019.csv

write.csv(suspectMunicipalities,"MunicipalitiesUnderSuspect_Jan_2018.csv", row.names = TRUE,sep=';')




fibo <- myFibonnaciSerie(100)
str(fibo)
head(fibo)
fibo <- data.frame(lapply(fibo, as.character), stringsAsFactors=FALSE)
print(paste("Total of rows from original dataset ", nrow(fibo)))
fibo <- na.omit(fibo) #data cleaning - blank, NaN values removed 
print(paste("Total of rows after empty 'any-attribute' rows cleaning ", nrow(fibo))) #all records are complete, no missing attibute
head(fibo,10)
tail(fibo,10)

df <- prepare_grouped_data(fibo)
myFirstDigitBarLineChartPresention(df)
myFirstandSecondDigitBarLineChartPresention(df)
myFirstDigitDoubleBarChartPresention(df)




dados = as.numeric(fibo$Withdrawal.Value)
dados = fibo$total.amount
modelo = benford(dados,number.of.digits = 1)
modelo
plot(modelo)

options(scipen = 999)
fibo <- data.frame("Withdrawal.Value"="1", "value"="1")
str(fibo)
fibo <- rbind(fibo, c(1, 1))
fibo <- data.frame(lapply(fibo, as.character), stringsAsFactors=FALSE)
str(fibo)
fibvals <- numeric(3500)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:3500) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
  fibo <- rbind(fibo, c(fibvals[i], fibvals[i]))
} 
str(fibo)




recurse_fibonacci <- function(n) {
  if(n <= 1) {
    return(n)
  } else {
    return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
  }
}
print (recurse_fibonacci(10))
