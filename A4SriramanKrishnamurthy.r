#Create a vector called x and place values 10 to 1000.
x<-10:1000  # creates x vector as specified

# Create a y vector that takes the square root of the log of numbers in x. 
y<-sqrt(log(x)) # creates y vector as specified

#y<-log(x)
#NOTE : The question asks to take sqrt of log values of x. However the Graph shown in the picture is log x function
#and not sqrt of log x function. I chose to proceed with the wordings given than the visuals.You can comment line 5 
#and uncomment line 7 to get the exact visualization given in assignment pdf.

#Create a z vector with values 50/x. 
z<-50/x # creates z vector as specified

#Plot x, y, and z as shown below.
plot(x,y,col="blue",xlab="",ylab="",ylim=c(0,10))
#the plot function plots values of x and y and colors them blue. xlab and y lab 
#are intentionally given empty to not label axis as of now
#ylim marks y axis from 0 to 10(setting boundary to y axis)
par(new=TRUE)
# The above command allows us to add new plot over the existing plot.It is required to plot z values in the same plot.
#The y function is plotted in blue as given by col and as a line l as given by type


plot(x,z,col="red",xlab="",ylab="",axes=FALSE,ylim=c(0,10))
#The above command plots x and z values and the x,y axes labels are intentionally kept blank.
#The z function is plotted in red as given by col and as a line l as given by type

text(400,4,expression(y==alpha^2+gamma+sqrt(beta)))
# the above command the helps plotting the expression given in the visualization in the x and y coordinate that we require
title(main="Assignment4", 
  	xlab="x", ylab="y and z")
# The above command labels the plot ,x axis and Y axis

