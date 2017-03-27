#rm(list=ls()) 
setwd("C:/MSBAPM_courses/Sem2/R/Test1")
list.files()
#read gapminder
gapminder<-read.csv("C:/MSBAPM_courses/Sem2/R/Test1/gapminder.csv")

#Avggdppercap
asiagdp<-mean(gapminder$gdpPercap[gapminder$continent=='Asia'])
europegdp<-mean(gapminder$gdpPercap[gapminder$continent=='Europe'])
length(gapminder$gdpPercap[gapminder$continent=='Ocenia'])

#Create a strip chart as shown in the Question
xx<-levels(gapminder$continent)

plot(gapminder$gdpPercap,gapminder$continent,xlab="GDP per Capita",ylab="",col="blue",
     xlim=c(0, 30000))
abline(v=10000,col="red")
axis(side=2,at=1, paste('Africa'))
axis(side=2,at=2, paste('America'))
axis(side=2,at=3, paste('Asia'))
axis(side=2,at=4, paste('Europe'))
axis(side=2,at=5, paste('Ocenia'))

#Question2
#homework time is normal with mean =5min and sd=2min
hw<-rnorm(40,mean=5,sd=2)
#prob hw takes less than 3 hrs ie 180 mins
pnorm(180,mean=5,sd=2)
#95% confidence Interval
qnorm(0.025,mean=5,sd=2)
qnorm(0.975,mean=5,sd=2)

#Question3
#read admissions.csv
admissions<-read.csv("C:/MSBAPM_courses/Sem2/R/Test1/admission.csv")
install.packages("moments")
library(moments)
skewness(admissions$GPA)

#hypothesis skewness of GPA variable is zero
tstat=skewness(admissions$GPA)

#Normal curve also has zero skewness.The below test is gives the probability value 
#that the above sample came from the population generated below
#(we are generting normal pop with sample mean and sd)
f1 =function()
{
  x = rnorm(length(admissions$GPA),mean = mean(admissions$GPA),sd = sd(admissions$GPA))
  return(skewness(x))
}

# Create Sampling Distribution
sdist = replicate(10000,f1())

# Plot the sampling distribution and create p-value
gap = abs(mean(sdist)-tstat)
plot(density(sdist))
abline(v=mean(sdist)-gap,col="red")
abline(v=mean(sdist)+gap,col="red")
s1 = sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap]
pvalue = length(s1)/length(sdist)
pvalue

##############
#Question 4
#hypothesis Coin is Fair i.e Prob(heads)=0.5
#mysample
s<-rep(c('H','T'),times=c(10,5))
tstat <- length(s[s=='H'])/length(s) #prob of heads

#The idea here is to create a fair population of heads and tails 
#and get the probability that our sample came from this population
f2 = function() 
{ 
  v = c('H','T') 
  p = c(0.5,0.5) 
  x = sample(x = v,replace = T,prob = p,size = length(s)) 
  return(length(x[x=='H'])/length(x)) #
}
sdist = replicate(10000,f2())
plot(density(sdist))
gap = abs(mean(sdist)-tstat)
abline(v=mean(sdist)-gap)
abline(v=mean(sdist)+gap)
pvalue = length(sdist[sdist<mean(sdist)-gap|sdist>mean(sdist)+gap])/length(sdist)
pvalue