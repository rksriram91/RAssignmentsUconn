#Assignment 6 | R Professor: Ram Gopal | Author : Sriraman Krishnamurthy |Date : 14th Feb 2017

#1.Create a function that takes in a numeric vector. The output should be a vector with running mean
#values. The ith element of the output vector should be the mean of the values in the input vector
#from 1 to i. (worth 20 points)

####################################################################################
#Function for Question 1
runMean <- function(x)
{
  meanVec <- vector(mode="numeric", length=0)
  for(i in 1:length(x))
  {
    mean <- mean(x[1]:x[i])
    meanVec<-c(meanVec,mean)
  }
  return(meanVec)
}

runMean(c(1,2,3))  #Verificarion
#runMean(c(1,2,3,8,9,45)) #Verificarion

#####################################################################################
#2
#Create a function that takes in a numeric vector. The objective is to forecast using exponential
#smoothing. The formula is (????????
exponentialSmoothing <- function(y,alpha=0.8)
{
  y_predicted <-vector(mode="numeric", length=0)
  for(i in 1:length(y))
  {
    if(i == 1) {y_predicted=c(y_predicted,y[i])}
    else {
      y_predicted=c(y_predicted,y_predicted[i-1]+alpha*(y[i-1]- y_predicted[i-1]))
      }
  }
  finalValues=data.frame(actual_value = numeric(),predicted_value = numeric())
  finalValues=cbind(y,y_predicted)
  print(y_predicted)
  return(finalValues)
}
s<-exponentialSmoothing(c(1,4,2,7,3,5,5,5,6,8,7,1))
s
#####################################################################################

install.packages("schoolmath") 
library(schoolmath) 
is.prim(3)

##Create a function that takes in two integers (set default values of 1 to both). 
#The function should calculate the number of prime numbers between the two values. (worth 20 points)
returnPrime <- function( a=1, b=1 )
{
  primeVec <- vector(mode="numeric", length=0)
  for(i in a:b)
  {
   if(is.prim(i))
   {primeVec<-c(primeVec,i)}
  }
  return(length(primeVec))
}

returnPrime(1,49) # returns Number of Prime numbers between two numbers

#####################################################################################
#Simulate a function to roll a dice. 
#Note that a dice turns up with numbers 1, 2, 3, 4, 5 or 6. 
#The function should do the following: you roll the dice twice, 
#and if both the numbers are the same then return 'You Win' otherwise return 'You Lose' (worth 20 points) 
#Hint: use the function sample(vector, size, replace=T/F). 
#The argument vector indicates the vector of values to draw from. 
#The argument size indicates how many values to draw, and replace argument indicates whether you sample with replacement. 
#Also note that this function does not take in any parameters, it returns a string, i.e. if the result of two rolling are 3 and 6, the function should return "You Lose"
rolldice <- function()
{
  a<-sample(x=1:6,size= 2, replace=T)
  x<-ifelse(a[1]==a[2],"You win","You Lose")
  print(paste("first Roll is :",a[1],"second Roll is :",a[2])) # print the samples that were chosen
  return(x)
}
rolldice()

#####################################################################################
#Create a function called Missing that takes in a data frame as the input and outputs 
#another data frame with column names, number of missing values in each column, percentage of missing values in each column, 
#and the number of unique values in each column. An example output is:

Missing <- function(df)
{
  #Initializing Empty vectors
 cname=vector(mode="numeric",length=0)
 nmiss=vector(mode="numeric",length=0)
 percentmiss=vector(mode="numeric",length=0)
 uniquevalues=vector(mode="numeric",length=0)
 
 for(i in 1:ncol(df))
  {
    cname<-c(cname,colnames(df[i]))
    nmiss <-c(nmiss,sum(is.na(df[,i])))
    percentmiss <- c(percentmiss,nmiss[i]/(length(df[,i])*100))
    uniquevalues <- c(uniquevalues,length(unique(df[,i])))
  }
  df1=data.frame(cname,nmiss,percentmiss,uniquevalues) #creating data frame out o 4 vectors
  return(df1)  
}

Missing(survey) #testing
#s ####
