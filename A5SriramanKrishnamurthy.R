#Assignment 5
#Use the data set survey in the MASS package. 
#Create a plot of Age and Pulse. Color females over 21 years with red. Color males who exercise frequently with blue.
library(MASS) #loading Mass package
View(survey)

#Create a plot of Age and Pulse
plot(survey$Age,survey$Pulse,main="Assignment 5",pch=19)
#THe above plot command creates plot with title Assignment 5 and solid point character pch=19

#Color females over 21 years with red
points(x=survey$Age[survey$Age>21 & survey$Sex=="Female"],y=survey$Pulse[survey$Age>21 & survey$Sex=="Female"]
       ,col="red",pch=19,lwd=5)
#the points command is a subplotting function to plot points.
#The age females over21 is subsetted for x and pulse of females over 21 for y and the points are coloured wih red .
#The points have increased lwd to see them distintly

# Color males who exercise frequently with blue.
points(x=survey$Age[survey$Exer=="Freq" & survey$Sex=="Male"],y=survey$Pulse[survey$Exer=="Freq" & survey$Sex=="Male"]
       ,col="blue",pch=19,lwd=5)
#the points command is a subplotting function to plot points.
#The age of males who exercise frequently is subsetted for x and pulse of males who exercise frequently for y 
#and the points are coloured wih blue .
#The points have increased lwd to see them distintly

