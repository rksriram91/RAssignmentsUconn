#Read the file contribution.csv
#setwd("C:/MSBAPM_courses/Sem2/R/Assignmets")
readdata<-read.csv("contribution.csv")
#a.        Create a frequency table between Gender and Marital Status. 

x<-table(readdata$Gender,readdata$Marital.Status) #frequency table betweeen Gender and MartialStatus
x #prints x
#What percentage of divorcees (denoted as D) are male?
prop.table(x,margin=2) #Ans :0.3974 or 39.74%

#What percentage of the total are single (denoted as S) females? 
prop.table(x) #Percentage of total Single female ANS : 0.1764 or 17.64%

#What is the most common marital status of females?
prop.table(x,margin=1)  #Most common Martial status  among female  ANS : M because 55.44% have status as M as seen in the output

#b.       Compute the median value of FY04Giving based on Gender and Marital Status. Which two groups had the highest median giving?
aggregate(readdata$FY04Giving,list(readdata$Gender),median)
#Gender	#  Group.1 x
		#1       F 0
		#2       M 0 
aggregate(readdata$FY04Giving,list(readdata$Marital.Status),median)		
#MartialStatus	 Group.1  x
#			1       D  0
#			2       M  0
#			3       S  0
#			4       W 50
aggregate(readdata$FY04Giving,list(readdata$Marital.Status,readdata$Gender),median)	
#  Group.1 Group.2   x
#1       D       F   0
#2       M       F   0
#3       S       F   0
#4       W       F  50
#5       D       M   0
#6       M       M   0
#7       S       M   0
#8       W       M 275

#Which two groups had the highest median giving?
#Ans : looking at the above table  MartialStatus W with gender M(male) have 275 median and MartialStatus W and Gender F have 50 Median .
#Martial Status W with Gender M and F are the two groups with highest median giving


#c.        Cut ‘Class Year’ into 2 groups with 3 break points –Inf, 1980, Inf. Save it as cyear.
cyear <- cut(x=readdata$Class.Year,breaks=c(-Inf,1980,Inf))

#Calculate the average FY03Giving grouping by cyear and Gender. Which group gave the lowest?
aggregate(readdata$FY03Giving,list(cyear,readdata$Gender),mean)	
#          Group.1 Group.2         x
#1 (-Inf,1.98e+03]       F 257.97552
#2 (1.98e+03, Inf]       F  47.43432
#3 (-Inf,1.98e+03]       M 610.87432
#4 (1.98e+03, Inf]       M  46.36887
#Which group gave the lowest? 
#ANS : Infering from the above table Group (1980,Inf) in cyear with Male Gender have the least Fyo3giving with 46.03 mean, closely followed by second lowest (1980,Inf) F with 47.43 mean

#d.       Install and explore package dplyr.
#install.packages(dplyr)
library(dplyr)
library(help=dplyr)
#1.         Create a subset with individuals whose Next Degree is either MS or PHD.  Save it as S1.
s1 <- filter(readdata,readdata$Next.Degree==('MS') | readdata$Next.Degree==('PHD' )) # creates subset s1 based on filter condition

#2.         Sort the subset S1 on Gender and Next Degree. Save it as S2.
s2 <- arrange(.data = s1,Gender,Next.Degree) #sorts the data in asc order based on Cols given

#Using the dataset quakes in the datasets library to complete the following 2 tasks
#e.        Split the plotting region into 1 row and 3 columns with grey background and blue color and then plot the following graphs: lat and long, depth and mag, stations and mag.
# this expression sets up a plot with 1 row 3 columns, sets bg color
par(mfrow = c(1, 3),bg="Grey" ) # setting backgroung grey and creating 3 rows for 3 graphs in canvas
plot(quakes$lat,quakes$long,xlab="LAT",ylab="LONG",col="blue") # plotting first graph between latitude and longitude in blue color and labelling them approptiately 
plot(quakes$depth,quakes$mag,xlab="DEPTH",ylab="MAG",col="blue")#Plotting second graph between depth and mag in blue color and labelling them approptiately 
plot(quakes$stations,quakes$mag,xlab="STATIONS",ylab="MAG",col="blue") # plotting third graph between stations and mag in blue color and labelling them approptiately 

#f.        Draw a histogram for mag with density on the y-axis. Add a vertical line to indicate the mean of mag in red with line width 
par(mfrow = c(1, 1),bg="white") #resetting the plot with white background and 1 row 
plot(density(quakes$mag), xlab="Mag") #plots density
polygon(density(quakes$mag),col="blue") # adds blue color shading the density
abline(v=mean(quakes$mag),lwd=3,col="red")#Add a vertical line to indicate the mean of mag in red with line width 3
#3. Give the line an appropriate label.
label1 <- paste("Avg Mag = ",mean(quakes$mag))#creating label to be pasted in graph. Created using paste function
text(4.8,0.3,label1,srt=90)# plotting the label in appropriate coordinates as specified (4.8,0.3) and turning the label by 90 deg using srt attriute