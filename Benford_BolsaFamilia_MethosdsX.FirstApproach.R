############  May, 24th 2020 - São Paulo - Brazil ########################
############  Newcomb-Benford Law applied to Brazilian Welfare Program - Bolfa Familia #######
############  Developed by Caio Azevedo ########################


#https://insightr.wordpress.com/2018/11/17/benfords-law-for-fraud-detection-with-an-application-to-all-brazilian-presidential-elections-from-2002-to-2018/
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


### group dataSet by municipalities counting the number of beneficiaries as well as #############################
# summing up the total amount paid by the municipality to their population
myGroupDataByMunicipalities <- function(myDataSet, fileName){
  myDataSet <- myDataSet %>%
    group_by(Accounting.period = MÊS.COMPETÊNCIA, Municipality.Name = NOME.MUNICÍPIO,State = UF) %>%
    summarise(Number.of.Beneficiaries=n(), 
              Withdrawal.Value=sum(VALOR.PARCELA,na.rm=TRUE), 
              avg.amount=round(mean(VALOR.PARCELA,na.rm=TRUE),2),
              max.amount=max(VALOR.PARCELA, na.rm=TRUE))
  
  # dataSet
  # Write this dataset so I don´t have handle huge dataSet anymore
  write.csv(myDataSet,fileName, row.names = TRUE,sep=';')
  return(myDataSet)
}

prepare_analytical_data <- function(myDataSet) {
  
  #dataSet <- dataSet %>%
  #  group_by(MÊS.COMPETÊNCIA, NOME.MUNICÍPIO,UF) %>%
  #  summarise(total.count=n(), 
  #            total.amount=sum(VALOR.PARCELA,na.rm=TRUE), 
  #            avg.amount=mean(VALOR.PARCELA,na.rm=TRUE),
  #            max.amount=max(VALOR.PARCELA, na.rm=TRUE))
  
  rem = ifelse(myDataSet$VALOR.PARCELA>=10,1,NA)
  myDataSet$fd = as.numeric(str_sub(myDataSet$VALOR.PARCELA,1,1))
  myDataSet$sd = as.numeric(str_sub(myDataSet$VALOR.PARCELA,2,2))*rem
  myDataSet$f2d = as.numeric(str_sub(myDataSet$VALOR.PARCELA,1,2))*rem
  
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
          xlab="group of significant digits", 
          font.lab=2)
}

myFirstDigitBarLineChartPresention <- function(dt){
  benford1 = log10(1+1/1:9)
  names(benford1) = 1:9

  bar = barplot(benford1, ylim = c(0,0.4), main = "Bolsa Familia First Digit")
  lines(bar,dt$first$prop,type ="b", col = 1)
  legend("topright", legend = "municipalities payment", col = c(2:(length(dt)+1)), cex = 0.6, seg.len = 0.4 ,lty = 1,bty = "n")
}

myFirstandSecondDigitBarLineChartPresention <- function(dt){
  benford12 = log10(1+1/10:99)
  names(benford12) = 10:99

  #bar = barplot(benford12, ylim = c(0,0.06), main = "Bolsa Familia First and Second Digits")
  #lines(bar,dt$fands$prop,type ="b", col = 1)
  
  bar = barplot(dt$fands$prop, ylim = c(0,0.06), main = "Bolsa Familia First and Second Digits")
  lines(bar,benford12,type ="b", col = 1)
  
  
  legend("topright", legend = "municipalities payment", col = c(2:(length(dt)+1)), cex = 0.6, seg.len = 0.4 ,lty = 1,bty = "n")
}

myFibonnaciSerie <- function(len){
  fibo <- data.frame("total.amount"=1, "value"=1)
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

#############################  Author´s note: ############################################
#Original dataset provided by Portal da Tansparência - First approach to analyse Bolsa Familia payment values
#Once this data set is grouped there is no need to use these huge files anymore.
#Change file parameter according to period of analysis as following:

# 201801_BolsaFamilia_Saques.csv 
# 201803_BolsaFamilia_Saques.csv 
# 201901_BolsaFamilia_Saques.csv
# 201903_BolsaFamilia_Saques.csv

#csv2 is used due to decimal point is comma format (brazilian format)
###### CHANGE 'file' PARAMETER TO ANALYZE OTHER DATA SET ###########################
dataSet <- read.csv2(file="201801_BolsaFamilia_Saques.csv",sep=';',header=TRUE,fileEncoding = "latin1") #csv2 is used due to decimal point is comma format

print(paste("Total of rows from original dataset ", nrow(dataSet)))
dataSet <- na.omit(dataSet) #data cleaning - blank, NaN values removed 
print(paste("Total of rows after empty 'any-attribute' rows cleaning ", nrow(dataSet))) #all records are complete, no missing attibute
head(dataSet,10)
tail(dataSet,10)




 ####  Author´s note: use next code snippet when analysing analytical ############################
 #### datasets, otherwise comment it all, till END OF SNIPPET CODE comment below #################
 
  hist(dataSet$VALOR.PARCELA, main="Histogram of Payment", xlab="F1", border="black", col="green",prob = FALSE,breaks=300)
  lines(density(dataSet$VALOR.PARCELA))
  abline(v=mean(dataSet$VALOR.PARCELA),lty=2)

  data = as.numeric(dataSet$VALOR.PARCELA)
 
  bfordResult <- benford(data, 1, discrete=TRUE, sign="both") #generates benford object
  plot(bfordResult) #plot benford analysis
  sampleSize <- bfordResult$info$n
  chiSquare <- round(chisq(bfordResult)$statistic,4)
 
  print(paste("sample size ", sampleSize))
  print(paste("chi-square first digit ", chiSquare))
  
  df <- prepare_analytical_data(dataSet)
  head(df, 10)
  myFirstDigitBarLineChartPresention(df)
  myFirstandSecondDigitBarLineChartPresention(df)
  myFirstDigitDoubleBarChartPresention(df)
  
  ############ Group data by Municipalities in order to use the second approach ############################
  #BolsaFamilia_Municipalities_Jan_2018.csv BolsaFamilia_Municipalities_Mar_2018.csv
  #BolsaFamilia_Municipalities_Jan_2019.csv BolsaFamilia_Municipalities_Mar_2019.csv
  
  GroupedDataSet <- myGroupDataByMunicipalities(dataSet,"BolsaFamilia_Municipalities_Jan_2018.csv")

  ##################  END OF SNIPPET CODE to analytical dataset analysis #############################
  
 
