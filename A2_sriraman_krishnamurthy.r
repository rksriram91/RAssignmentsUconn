#R Assignment 2 #Aim: DataFrame Practice #by: Sriraman Krishnamurthy

#installing and loading ISwR package
install.packages("ISwR")
library(ISwR)

#1.     Mean, median and 75th percentile values of age
mean(stroke$age)#Ans :69.8854
quantile(stroke$age,c(0.5,0.75))#median 0.5th percentile :71   0.75th percentile :81

#2.     Conduct a t-test to evaluate the hypothesis for the population age of 71 for age variable.
t.test(stroke$age,mu=71)
#The above line is the t-test for the claim that age=71 for the stroke dataset.
#But the 95% confidence interval is (68.94369 70.82711).Hence hypothesis true mean not equal to 71 is more valid

#3.Create a subset data frame called s1 of all patients who are not dead. Draw a plot of sex and age for these individuals. What is your takeaway from the visual?
s1<- stroke[stroke$dead==FALSE,] #creates subset of patients who are not dead
plot(s1$sex,s1$age,col="blue") #plots sex on x axis and age on Y axis
#TakeAway from Graph :
#1.The median and average age of female ig greater than male. 
#2.The lowest age is of male .There are 4 males below 20 and only 1 female below 20.
#3.The highest age is represented by female

#4.What is the mean age of all patients who have diabetes and are not dead?
condition<- (stroke$diab=="Yes" && stroke$dead==FALSE)
mean(stroke$age[(stroke$diab=="Yes" & stroke$dead==FALSE)],na.rm=TRUE)#Ans: 65.11429 
#filtering for diab=yes and dead=False and applying mean over it,the na values are filtered out using na.rm=true
#or
mean(s1$age[s1$diab=="Yes"],na.rm=TRUE)#s1 already represents patients who are not died adding diab condition on it and getting mean from it na values are filtered out

#5. What is the mean age of all patients who have diabetes and are dead?
mean(stroke$age[(stroke$diab=="Yes" & stroke$dead==TRUE)],na.rm=TRUE)#Ans :71.37097 
#filtering diab=yes and dead=true and applying mean over it,the na values are filtered out using na.rm=true
