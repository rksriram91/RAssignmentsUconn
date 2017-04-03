#rm(list=ls())
setwd("C:/MSBAPM_courses/Sem2/R/Assignments")

##############Question 1########################
#1. Consider the following workflow (worth 20 points).
#Task 1 follows a random uniform distribution between 5 and 9; 
#the completion time for Task 2 follows a random exponential distribution with a rate of 0.1; 
#Task 3 follows a Poisson distribution with a lambda of 4; 
#Task 4 follows uniform distribution between 3 and 10. Compute the following.
task1=runif(10000,min=5,max=9)
task2=rexp(10000,rate=0.1)
task3=rpois(10000,lambda = 4)
task4=runif(10000,min=3,max=10)
totaltimedf <-cbind(task1,task2,task3)
tt <- apply(totaltimedf,1,max)
#tasks are parallel ..Hence maximum time is taken.#pmax funtion can also be used
#tt=pmax(task1,task2,task3)
#tt has the maximum time between task1,task2,task3 you can veify by df<-cbind(totaltimedf,tt),View(df)
totaltime=tt+task4 #tasks are sequential
#a. What are the mean and median times to complete all the tasks?
meantime<-mean(totaltime)
mediantime <- median(totaltime) #mean and median times are computed
#b. What is the probability that all the tasks are completed in 15 hours? = 0.4172
p=length(totaltime[totaltime<15])/length(totaltime)
p
#c. Create a plot of the density of the total completion time.
plot(density(totaltime)) 
abline(v=15)

##############Question 2########################
#rm(list=ls())
#Read the file diabetes.csv. 
diab<-read.csv("C:/MSBAPM_courses/Sem2/R/Assignments/diabetes.csv")
#There is a variable called Pregnancies, which indicates the number of pregnancies.
Preg<- diab$Pregnancies
#Assuming this follows a poisson distribution,
#test the hypothesis that the mean number of pregnancies is 3.7 (worth 20 points).

tstat=mean(Preg)
f1<-function() 
{
  x=rpois(n=length(Preg),lambda=3.7)
  return((mean(x)))
}
# Create the sampling distribution
sdist = replicate(10000,f1())

# Plot the sampling distribution and create p-value
gap = abs(mean(sdist)-tstat)
plot(density(sdist))
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue
#since p value s less than 0.05 ,I reject hypothesis that mean is 3.7

##############Question 3########################
#rm(list=ls())
#Read the file diabetes.csv.
diab<-read.csv("C:/MSBAPM_courses/Sem2/R/Assignments/diabetes.csv")
#There is a variable called Insulin.
#Conduct both a parametric and a non-parametric test for the median value of 80. 
#Are the results from both the tests similar?
#If not, explain why and which test you would trust more (worth 20 points).
tstat=median(diab$Insulin)

f1 = function() 
{ 
  x = median(rnorm(n=length(diab$Insulin),80,sd=sd(diab$Insulin))) 
  return(x) 
}
# Create the sampling distribution
sdist = replicate(10000,f1())

# Plot the sampling distribution and create p-value
gap = abs(mean(sdist)-tstat)
plot(density(sdist))
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)
pvalue
#p value is as negligible as zero. Hypothesis median=80 Rejected

#non parametric testing
tstat = sum(ifelse(diab$Insulin>80,1,0))
f2 = function() 
{ 
  v = c(1,0) 
  p = c(0.5,0.5) 
  x = sample(x = v,replace = T,prob = p,size = length(diab$Insulin)) 
  return(sum(x)) 
}
sdist = replicate(10000,f2())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)
pvalue
#p value is as negligible as zero. Hypothesis median=80 Rejected
#out of Both the tests Non parametric testing results can be trusted more ,because it makes least assumptions

##############Question 4########################
#rm(list=ls())
#Read the file diabetes.csv.
diab<-read.csv("C:/MSBAPM_courses/Sem2/R/Assignments/diabetes.csv") 
#There are two variables called BMI and Outcome. 
#The variable Outcome takes on only two values: 0 and 1. 
#Conduct a non-parametric two sample test for the hypothesis that the standard deviation
#of BMI is the same for both Outcome values (worth 20 points).
one = diab[diab$Outcome==1,6] 
zero = diab[diab$Outcome==0,6] 
tstat = abs(sd(one)-sd(zero))
tstat
#With the null hypothesis of no difference between the two groups,
#we define the mean of each group to be the total mean of both the groups. 
#Rest of the code is as follows.
gmean = median(diab$BMI) 
f4 = function() 
{ 
  x1=sample(one,size=length(one),replace=T)
  x2 =sample(zero,size=length(zero),replace=T)
  return(abs(sd(x1)-sd(x2)))
}
sdist = replicate(10000,f4()) 
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)
pvalue
#pvalue >0.6 .Cannot reject the hypothesis that SD of BMI for both outcomes is the same
#accepting

##############Question 5########################
#rm(list=ls())
#Read the file diabetes.csv.
diab<-read.csv("C:/MSBAPM_courses/Sem2/R/Assignments/diabetes.csv") 
#There are two variables called Glucose and BloodPressure.
# Conduct a non-parametric test for the shapes of the two distributions are identical 
glucose<-diab$Glucose
bp<-diab$BloodPressure
plot(density(glucose))
points(density(bp))
glucose1 = (glucose-mean(glucose))/sd(glucose)
bp1 = (bp-mean(bp))/sd(bp)
plot(density(glucose1),col="blue")
lines(density(bp1),col="red")

# Compute test statistic
q = c(0.2,0.4,0.6,0.8,0.9)
x1 = quantile(glucose1,probs = q)
x2 = quantile(bp1,probs = q)
tstat = sum(abs(x1-x2))

f1 =function()
{
  s1 = sample(x=glucose1,size=length(glucose1),replace=T)
  s2 = sample(x=glucose1,size=length(glucose1),replace=T)#creating population twice and computing difference
  q = c(0.2,0.4,0.6,0.8,0.9)
  x1 = quantile(s1,probs = q)
  x2 = quantile(s2,probs = q)
  return(sum(abs(x1-x2)))
}
#To test if they came from same sample we have to check if compare difference between sample 
#and two populations generated from same sample
sdist = replicate(10000,f1()) 
plot(density(sdist)) 
gap = abs(mean(sdist)-tstat)
abline(v=(mean(sdist)-gap))
abline(v=(mean(sdist)+gap))
pvalue = length(sdist[sdist<(mean(sdist)-gap)|sdist>(mean(sdist)+gap)])/length(sdist)
pvalue
#p-value = 0.0 Reject the hypothesis that they came from the same population
