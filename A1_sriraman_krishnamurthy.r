#R Assignment 1 #Aim: sequence and Repetition Practice #by: Sriraman Krishnamurthy

#Question1
#create a sequence from -10 to 10 incremented by 0.1
x=seq(-10,10,by=0.1)
#x #checking if the sequence is created
# creating a y vector that contains sin values of x vector
y=sin(x) 
#y ##checking if the sequence is created

plot(x,y,xlab = "x value",ylab = "sine",main="Assignment Graph",pch=19,col="orange") 
# Graph Plotted with the given header,xlabel,ylabel,color and point character)

#Question2
#a.       Repeat 7 times as follows
#"x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z" "x" "y" "z"
rep(c("x","y","z"),7)
#b.       Repeat x, y 4 times and z twice as follows.
#"x" "y" "x" "y" "x" "y" "x" "y" "z" "z"

rep(c(rep(c("x","y"),4),"z"),times=c(rep(c(1,1),4),2))
#OK !! this part was challenging ;) tried around 30 combinations before coming up with the above line
 
