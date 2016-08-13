library(ks)
library(RColorBrewer)
source("CrimeUtil.R")
source("Shapefile.R")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
# Part a.)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#getting the theft crime data
crime.data <- sample.crime("C:/UVa/Data_Mining/workspace/Crimes_-_2014.csv", -1 , start.month = 1, end.month = 12)
# plotting the thefts casesa over the plotted police station.
points(crime.data$x, crime.data$y, pch="*", col= "RED")

## The plots shows that theft is not uniformly distributed and therefore to find the theft concentration
## we will construct a 2-dimentional KDE to create a heat map.

# Setting the kde resolution to 200.
kde.resolution.meters = 200
# reading the chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# getting the x and y co-ordinate of the thefts location
kde.theft <- crime.data[,c("x", "y")]
# Converting the columns in numeric format
kde.theft.data <- transform(kde.theft, x=as.numeric(x), y= as.numeric(y))

# get estimation points of KDE
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE
kde.est = run.spatial.kde(kde.theft.data, kde.est.points, 10000) # taking 10000 random observations from the sample
plot.spatial.kde(kde.est, kde.est.points) # Plotting the heat map

# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)
#
#
## CONLCUSION##
# Here the highest concentration of thefts are present in the downtown area of the city 
##
#
# plotting the spread of police stations over the map of chicago
policestations.points = read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")
points(school.points, pch="*", col= "GREEN")
#
#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Part b.)
#--------------------------------------------------------------------------------------------------------------------------------------

# To compare the theft concentration as per the different time of the day
#-------------------------------------------------------------------------

# I have divided the day in 4 different time zones
##1. The morning phase, from 5am to 11am, which implies the hours from 6 to 11,
##2. The afernoon phase 11am to 5pm i.e the hours form 12 to 17, 
##3. The evening phase 5pm to 11pm i.e hours -> 18 to 23 and 
##4. finally, the late night from 11pm to 5am, i.e. hours from 24 and 1 to 5

# Creating the four sample data sets
## the morning time.
theft.morning = crime.data[crime.data$hour == 7|
                           crime.data$hour == 8|
                           crime.data$hour == 9|
                           crime.data$hour == 10|
                           crime.data$hour == 11|
                           crime.data$hour == 6, ]
## the afternoon time
theft.afternoon = crime.data[crime.data$hour == 13|
                             crime.data$hour == 14|
                             crime.data$hour == 15|
                             crime.data$hour == 16|
                             crime.data$hour == 17|
                             crime.data$hour == 12, ]
## the eveing time
theft.evening = crime.data[crime.data$hour == 19|
                             crime.data$hour == 20|
                             crime.data$hour == 21|
                             crime.data$hour == 22|
                             crime.data$hour == 23|
                             crime.data$hour == 18, ]
## The late night time
theft.latenight = crime.data[crime.data$hour == 1|
                             crime.data$hour == 2|
                             crime.data$hour == 3|
                             crime.data$hour == 4|
                             crime.data$hour == 5|
                             crime.data$hour == 24, ]

# Getting the x and y co-ordinate from each of the dataset and converting the columns
# in numeric format.
kde.sample.1 <- theft.morning[,c("x", "y")]
kde.sample.morning <- transform(kde.sample.1, x=as.numeric(x), y=as.numeric(y))

kde.sample.2 <- theft.afternoon[,c("x", "y")]
kde.sample.afternoon <- transform(kde.sample.2, x=as.numeric(x), y=as.numeric(y))

kde.sample.3 <- theft.evening[,c("x", "y")]
kde.sample.evening <- transform(kde.sample.3, x=as.numeric(x), y=as.numeric(y))

kde.sample.4 <- theft.latenight[,c("x", "y")]
kde.sample.latenight <- transform(kde.sample.4, x=as.numeric(x), y=as.numeric(y))

# the Layout of the plot
obs1 <- par(mfrow=c(2,2),
            mar=c(2,2,0,0),
            oma=c(1,1,2,1))

# run and plot KDE for morning time
kde.est.morning = run.spatial.kde(kde.sample.morning, kde.est.points, 1000) 
plot.spatial.kde(kde.est.morning, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE, main="heat map for thefts in the morning")

# run and plot KDE for afternoon time
kde.est.afternoon = run.spatial.kde(kde.sample.afternoon, kde.est.points, 1000) 
plot.spatial.kde(kde.est.afternoon, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE, main="heat map for thefts in the afternoon")

# run and plot KDE for evening time
kde.est.evening = run.spatial.kde(kde.sample.evening, kde.est.points, 1000) 
plot.spatial.kde(kde.est.evening, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE, main="heat map for thefts in the evening")

# run and plot KDE for late night time.
kde.est.latenight = run.spatial.kde(kde.sample.latenight, kde.est.points, 1000) 
plot.spatial.kde(kde.est.latenight, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE, main="heat map for thefts done at latenight")

# adding heading to the plot
mtext("Heat Maps for the theft concentration for different hours of the day", outer = TRUE, cex = 1)

par(obs1)

## CONCLUSION##
# The highest concentration can still be found in the downtown area of the map also,
#	Most spread of theft concentration is observed in the morning time and the evening time
#	In the afternoon time almost all the thefts are concentrated in the downtown area of the town.
#	Again the late-night time the spread of thefts concentration is more but still the higher 
# concentration is spread across the coastal area of the town.
##
#
#
# To compare the theft concentrations as per different days of the week
#-----------------------------------------------------------------------

# creating the sample dataset for each day of the week
theft.monday = crime.data[crime.data$day.of.week == 2, ]
theft.tuesday = crime.data[crime.data$day.of.week == 3, ]
theft.wednesday = crime.data[crime.data$day.of.week == 4, ]
theft.thursday = crime.data[crime.data$day.of.week == 5, ]
theft.friday = crime.data[crime.data$day.of.week == 6, ]
theft.saturday = crime.data[crime.data$day.of.week == 7, ]
theft.sunday = crime.data[crime.data$day.of.week == 1, ]

# the layout of the plots
obs2 <- par(mfrow=c(2,4),
            mar= c(2,2,0,0),
            oma= c(1,1,2,1))

# getting the 'x' and 'y' co-ordinate for monday
kde.sample.5 <- theft.monday[,c("x", "y")]
# converting the co-ordinates in numeri format
kde.sample.monday <- transform(kde.sample.5, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for monday
kde.est.monday = run.spatial.kde(kde.sample.monday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.monday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for tuesday 
kde.sample.6 <- theft.tuesday[,c("x", "y")]
# converting the co-ordinates in numeric format
kde.sample.tuesday <- transform(kde.sample.6, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for tuesday
kde.est.tuesday = run.spatial.kde(kde.sample.tuesday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.tuesday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for wednesday
kde.sample.7 <- theft.wednesday[,c("x", "y")]
# converting the co-ordinate in numeric format
kde.sample.wednesday <- transform(kde.sample.7, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for wednesday
kde.est.wednesday = run.spatial.kde(kde.sample.wednesday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.wednesday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for thursday
kde.sample.8 <- theft.thursday[,c("x", "y")]
# converting the co-ordinate in numeric format
kde.sample.thursday <- transform(kde.sample.8, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for thursday
kde.est.thursday = run.spatial.kde(kde.sample.thursday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.thursday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for friday
kde.sample.9 <- theft.friday[,c("x", "y")]
# converting the co-ordinates in numeric format
kde.sample.friday <- transform(kde.sample.9, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for friday
kde.est.friday = run.spatial.kde(kde.sample.friday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.friday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for saturday
kde.sample.10 <- theft.saturday[,c("x", "y")]
# converting the co-ordinates in numeric format
kde.sample.saturday <- transform(kde.sample.10, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for saturday
kde.est.saturday = run.spatial.kde(kde.sample.saturday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.saturday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Getting the 'x' and 'y' co-ordinate for sunday
kde.sample.11 <- theft.sunday[,c("x", "y")]
# converting the co-ordinate in numeric format
kde.sample.sunday <- transform(kde.sample.11, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for sunday
kde.est.sunday = run.spatial.kde(kde.sample.sunday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.sunday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

mtext("Heat Maps for the theft concentration for day of the week in order of monday to sunday", outer = TRUE, cex = 1)

par(obs2)

##CONCLUSION##
# It is seen that the highest concentration still remains in the downtown area of 
# the city. But we can make some conclusions:
# On weekends the highest theft concentration is in downtown, while the spread 
# of thefts is not high, but during weekends people prefer to spend their weekends
# outdoors at specific landmarks and recreational location so, we can conclude that
# burglary is not high but other forms of thefts like motor thefts, pick-pocketing
# is higher.
#	More spread can be seen on the weekdays with highest spread of theft concentration 
# can be seen on Monday and Thursday which again implies that the concentration of 
# thefts like pick-pocketing, purse snatching, rental thefts, thefts from building etc
# are quite high.
#
#
# to compare the theft concentration as per the different month of the year
#---------------------------------------------------------------------------

# Creating a sample data for each month of the year.
theft.january = crime.data[crime.data$month == 1, ]
theft.feburary = crime.data[crime.data$month == 2, ]
theft.march = crime.data[crime.data$month == 3, ]
theft.april = crime.data[crime.data$month == 4, ]
theft.may = crime.data[crime.data$month == 5, ]
theft.june = crime.data[crime.data$month == 6, ]
theft.july = crime.data[crime.data$month == 7, ]
theft.august = crime.data[crime.data$month == 8, ]
theft.september = crime.data[crime.data$month == 9, ]
theft.october = crime.data[crime.data$month == 10, ]
theft.november = crime.data[crime.data$month == 11, ]
theft.december = crime.data[crime.data$month == 12, ]

#Layout of the plot:
obs3 <- par(mfrow= c(2,3),
            mar= c(3,3,0,0),
            oma= c(1,1,2,1))

# Creating a heat map for the thefts for the month of january.
# getting the 'x' and 'y' co-ordinate for the month of January
kde.sample.12 <- theft.january[,c("x", "y")]
# Converting the co-ordinate to numeric form
kde.sample.january <- transform(kde.sample.12, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.january = run.spatial.kde(kde.sample.january, kde.est.points, 1000) 
plot.spatial.kde(kde.est.january, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of feburary.
# getting the 'x' and 'y' co-ordinate for the month of feburary
kde.sample.13 <- theft.feburary[,c("x", "y")]
# converting the co-ordinate to numeric format
kde.sample.feburary <- transform(kde.sample.13, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.feburary = run.spatial.kde(kde.sample.feburary, kde.est.points, 1000) 
plot.spatial.kde(kde.est.feburary, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of march.
# getting the 'x' and 'y' co-ordinate for the month of march
kde.sample.14 <- theft.march[,c("x", "y")]
# converting the co-ordinates to numeric format
kde.sample.march <- transform(kde.sample.13, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.march = run.spatial.kde(kde.sample.march, kde.est.points, 1000) 
plot.spatial.kde(kde.est.march, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of april.
# getting the 'x' and 'y' co-ordinate for the month of april.
kde.sample.15 <- theft.april[,c("x", "y")]
# converting the co-ordinateds to numeric format
kde.sample.april <- transform(kde.sample.15, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.april = run.spatial.kde(kde.sample.april, kde.est.points, 1000) 
plot.spatial.kde(kde.est.april, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of may.
# getting the 'x' and 'y' co-ordinate for the month of may.
kde.sample.16 <- theft.may[,c("x", "y")]
# converting the co-ordinates to numeric form.
kde.sample.may <- transform(kde.sample.16, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.may = run.spatial.kde(kde.sample.may, kde.est.points, 1000) 
plot.spatial.kde(kde.est.may, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of june.
# getting the 'x' and 'y' co-ordinate for the month of june
kde.sample.17 <- theft.june[,c("x", "y")]
# converting the co-ordinates to numeric form
kde.sample.june <- transform(kde.sample.17, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.june = run.spatial.kde(kde.sample.june, kde.est.points, 1000) 
plot.spatial.kde(kde.est.june, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

mtext("Heat Maps for the theft concentration for the months of january to june", outer = TRUE, cex = 1)

# Creating a heat map for the thefts for the month of july.
# getting the 'x' and 'y' co-ordinate for the month of july.
kde.sample.18 <- theft.july[,c("x", "y")]
# converting the co-ordinate to numeric form.
kde.sample.july <- transform(kde.sample.18, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.july = run.spatial.kde(kde.sample.july, kde.est.points, 1000) 
plot.spatial.kde(kde.est.july, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of august.
# getting the 'x' and 'y' co-ordinate for the month of august.
kde.sample.19 <- theft.august[,c("x", "y")]
# converting the co-ordinates to numeric format.
kde.sample.august <- transform(kde.sample.19, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.august = run.spatial.kde(kde.sample.august, kde.est.points, 1000) 
plot.spatial.kde(kde.est.august, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of september.
# getting the 'x' and 'y' co-ordinate for the month of september.
kde.sample.20 <- theft.september[,c("x", "y")]
# converting the co-ordinates to numeric format.
kde.sample.september <- transform(kde.sample.20, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.september = run.spatial.kde(kde.sample.september, kde.est.points, 1000) 
plot.spatial.kde(kde.est.september, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of october.
# getting the 'x' and 'y' co-ordinate for the month of october.
kde.sample.21 <- theft.october[,c("x", "y")]
# converting the co-ordinates to numeric format.
kde.sample.october <- transform(kde.sample.21, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.october = run.spatial.kde(kde.sample.october, kde.est.points, 1000) 
plot.spatial.kde(kde.est.october, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of november.
# getting the 'x' and 'y' co-ordinate for the month of november
kde.sample.22 <- theft.november[,c("x", "y")]
# converting the co-ordinates to numeric format.
kde.sample.november <- transform(kde.sample.22, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.november = run.spatial.kde(kde.sample.november, kde.est.points, 1000) 
plot.spatial.kde(kde.est.november, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the thefts for the month of december.
# getting the 'x' and 'y' co-ordinate for the month of december.
kde.sample.23 <- theft.december[,c("x", "y")]
# converting the co-ordinates to numeric format.
kde.sample.december <- transform(kde.sample.23, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE
kde.est.december = run.spatial.kde(kde.sample.december, kde.est.points, 1000) 
plot.spatial.kde(kde.est.december, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

mtext("Heat Maps for the theft concentration for months of july to december", outer = TRUE, cex = 1)

par(obs3)

## CONCLUSION##
# The highest concentration hotspot still remains to be the downtown area of the 
# city irrespective of the month of the year.
# The highest spread in the theft concentration is found in the month of April 
# (which is the summer vacation break), September and October (These are the months 
# in which new fall session of many schools start and lot of new and unaware students 
# enter the city).
##
#
#
#---------------------------------------------------------------------------------
# Part-c
#-----------------------------------------------------------------------------------------------------

source("Shapefile.R")

#getting the assult crime data
crime.data2 <- sample.crime2("C:/UVa/Data_Mining/workspace/Crimes_-_2014.csv", -1 , start.month = 1, end.month = 12)
# plotting the crime data
points(crime.data2$x, crime.data2$y, pch="*", col= "RED")

# The assult cases are not uniformly distributed and therefore to find the  top
# concentration of assult crimes we will construct a 2-dimentional KDE to create a heat map.

kde.resolution.meters = 200
# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# getting the sample data
kde.assault <- crime.data2[,c("x", "y")]
kde.assault.data <- transform(kde.assault, x=as.numeric(x), y= as.numeric(y))

# get estimation points of KDE
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE
kde.est1 = run.spatial.kde(kde.assault.data, kde.est.points, 10000) # taking 10000 random observations from the sample
plot.spatial.kde(kde.est1, kde.est.points) # Plotting the heat map

# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# plotting the spread of police stations over the map of chicago
policestations.points = read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")
points(school.points, pch="*", col= "GREEN")
#
## CONCLUSION##
# As compared to the Thefts concentrations we can observe: 
# There is more number of hot spots locations in the assault crime 
# cases as compared to the theft cases.
##
#
#
# To compare the Assualt concentration as per the different time of the day
#-------------------------------------------------------------------------

# I have divided the day in 4 different time zones
##1. The morning phase, from 5am to 11am, which implies the hours from 6 to 11,
##2. The afernoon phase 11am to 5pm i.e the hours form 12 to 17, 
##3. The evening phase 5pm to 11pm i.e hours -> 18 to 23 and 
##4. finally, the late night from 11pm to 5am, i.e. hours from 24 and 1 to 5
#
# creating sample for each of the different time of the day
# the morning time 
assault.morning = crime.data2[crime.data2$hour == 7|
                             crime.data2$hour == 8|
                             crime.data2$hour == 9|
                             crime.data2$hour == 10|
                             crime.data2$hour == 11|
                             crime.data2$hour == 6, ]
# the afternoon time
assault.afternoon = crime.data2[crime.data2$hour == 13|
                               crime.data2$hour == 14|
                               crime.data2$hour == 15|
                               crime.data2$hour == 16|
                               crime.data2$hour == 17|
                               crime.data2$hour == 12, ]
# the evening time
assault.evening = crime.data2[crime.data2$hour == 19|
                             crime.data2$hour == 20|
                             crime.data2$hour == 21|
                             crime.data2$hour == 22|
                             crime.data2$hour == 23|
                             crime.data2$hour == 13, ]
# the late night time.
assault.latenight = crime.data2[crime.data2$hour == 1|
                               crime.data2$hour == 2|
                               crime.data2$hour == 3|
                               crime.data2$hour == 4|
                               crime.data2$hour == 5|
                               crime.data2$hour == 24, ]
# Getting the x and y co-ordinate from each of the dataset and converting the 
# co-ordinates in numeric format.
kde.sample.a1 <- assault.morning[,c("x", "y")]
kde.sample.a1.morning <- transform(kde.sample.a1, x=as.numeric(x), y=as.numeric(y))

kde.sample.a2 <- assault.afternoon[,c("x", "y")]
kde.sample.a2.afternoon <- transform(kde.sample.a2, x=as.numeric(x), y=as.numeric(y))

kde.sample.a3 <- assault.evening[,c("x", "y")]
kde.sample.a3.evening <- transform(kde.sample.a3, x=as.numeric(x), y=as.numeric(y))

kde.sample.a4 <- assault.latenight[,c("x", "y")]
kde.sample.a4.latenight <- transform(kde.sample.a4, x=as.numeric(x), y=as.numeric(y))

# Layout of the plot
obs4 <- par(mfrow=c(2,2),
            mar=c(2,2,0,0),
            oma=c(1,1,2,1))

# run and plot KDE for the morning time of the day.
kde.est.a1.morning = run.spatial.kde(kde.sample.a1.morning, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a1.morning, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# run and plot KDE for the afternoon time of the day.
kde.est.a2.afternoon = run.spatial.kde(kde.sample.a2.afternoon, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a2.afternoon, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# run and plot KDE for the evening time of the day.
kde.est.a3.evening = run.spatial.kde(kde.sample.a3.evening, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a3.evening, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Run and plot KDE for the late night time of the day.
kde.est.a4.latenight = run.spatial.kde(kde.sample.a4.latenight, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a4.latenight, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Adding header to the plot
mtext("Heat Maps for the assault concentration for different hours of the day", outer = TRUE)

par(obs4)

# CONCLUSION:
# As compared to the theft concentrations we can observe:
# There are more high concentration (red spots) areas in assault concentration as
# compared to theft concentration plot.
# The highest theft concentration was in downtown, but in case of assaults south 
# side and south Austin has higher concentration as compared to downtown.
##
#
#
# To compare the assault concentrations as per different days of the week
#-----------------------------------------------------------------------

# Creating the datasets for different day of the week from sunday to saturday
assault.monday = crime.data2[crime.data2$day.of.week == 2, ]
assault.tuesday = crime.data2[crime.data2$day.of.week == 3, ]
assault.wednesday = crime.data2[crime.data2$day.of.week == 4, ]
assault.thursday = crime.data2[crime.data2$day.of.week == 5, ]
assault.friday = crime.data2[crime.data2$day.of.week == 6, ]
assault.saturday = crime.data2[crime.data2$day.of.week == 7, ]
assault.sunday = crime.data2[crime.data2$day.of.week == 1, ]

#layout(matrix(c(1,2,3,0,4,5,6,7), 2,4, byrow = TRUE), respect = TRUE)
obs5 <- par(mfrow=c(2,4),
            mar= c(3,3,0,0),
            oma= c(1,1,2,1))

# Creating a heat map for the assaults on monday
# getting the 'x' and 'y' co-ordinate for monday
kde.sample.a5 <- assault.monday[,c("x", "y")]
kde.sample.a5.monday <- transform(kde.sample.a5, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for monday
kde.est.a5.monday = run.spatial.kde(kde.sample.a5.monday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a5.monday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on Tuesday
# getting the 'x' and 'y' co-ordinate for Tuesday
kde.sample.a6 <- assault.tuesday[,c("x", "y")]
kde.sample.a6.tuesday <- transform(kde.sample.a6, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for Tuesday
kde.est.a6.tuesday = run.spatial.kde(kde.sample.a6.tuesday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a6.tuesday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on wednesday
# getting the 'x' and 'y' co-ordinate for Wednesday
kde.sample.a7 <- assault.wednesday[,c("x", "y")]
kde.sample.a7.wednesday <- transform(kde.sample.a7, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for wednesday
kde.est.a7.wednesday = run.spatial.kde(kde.sample.a7.wednesday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a7.wednesday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on thursday
# getting the 'x' and 'y' co-ordinate for thursday
kde.sample.a8 <- assault.thursday[,c("x", "y")]
kde.sample.a8.thursday <- transform(kde.sample.a8, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for thursday
kde.est.a8.thursday = run.spatial.kde(kde.sample.a8.thursday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a8.thursday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on friday
# getting the 'x' and 'y' co-ordinate for friday
kde.sample.a9 <- assault.friday[,c("x", "y")]
kde.sample.a9.friday <- transform(kde.sample.a9, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for friday
kde.est.a9.friday = run.spatial.kde(kde.sample.a9.friday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a9.friday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on saturday
# getting the 'x' and 'y' co-ordinate for saturday
kde.sample.a10 <- assault.saturday[,c("x", "y")]
kde.sample.a10.saturday <- transform(kde.sample.a10, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for saturday
kde.est.a10.saturday = run.spatial.kde(kde.sample.a10.saturday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a10.saturday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for the assaults on sunday
# getting the 'x' and 'y' co-ordinate for sunday
kde.sample.a11 <- assault.sunday[,c("x", "y")]
kde.sample.a11.sunday <- transform(kde.sample.a11, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for sunday
kde.est.a11.sunday = run.spatial.kde(kde.sample.a11.sunday, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a11.sunday, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

mtext("Heat Maps for the assault concentration for each day of the week", outer = TRUE)

par(obs5)
##CONCLUSION##
# When compared to theft concentration
# As compared to thefts were concentration was in downtown, in assaults the higher
# concentration is in south and west side of the city.
# The spread and concentration is more in weekends in assaults spread as compared
# to thefts concentration where the spread was more in weedays.
##
#
#
# to compare the assault concentration as per the different month of the year
#---------------------------------------------------------------------------
#
# Creating the datasets for different months of the year from january to december.
assault.january = crime.data2[crime.data2$month == 1, ]
assault.feburary = crime.data2[crime.data2$month == 2, ]
assault.march = crime.data2[crime.data2$month == 3, ]
assault.april = crime.data2[crime.data2$month == 4, ]
assault.may = crime.data2[crime.data2$month == 5, ]
assault.june = crime.data2[crime.data2$month == 6, ]
assault.july = crime.data2[crime.data2$month == 7, ]
assault.august = crime.data2[crime.data2$month == 8, ]
assault.september = crime.data2[crime.data2$month == 9, ]
assault.october = crime.data2[crime.data2$month == 10, ]
assault.november = crime.data2[crime.data2$month == 11, ]
assault.december = crime.data2[crime.data2$month == 12, ]

#Layout of the plot:
obs6 <- par(mfrow= c(2,3),
            mar= c(3,3,0,0),
            oma= c(1,1,2,1))

# Creating a heat map for assaults cases for the month of january.
# getting the 'x' and 'y' co-ordinate for the month of january.
kde.sample.a12 <- assault.january[,c("x", "y")]
# converting the co-ordinates in numeric format.
kde.sample.a12.january <- transform(kde.sample.a12, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of january
kde.est.a12.january = run.spatial.kde(kde.sample.a12.january, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a12.january, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assaults cases for the month of febuary.
# getting the 'x' and 'y' co-ordinate for the month of febuary.
kde.sample.a13 <- assault.feburary[,c("x", "y")]
# converting the co-ordinated in numeric format
kde.sample.a13.feburary <- transform(kde.sample.a13, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of febuary.
kde.est.a13.feburary = run.spatial.kde(kde.sample.a13.feburary, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a13.feburary, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assaults cases for the month of march.
# getting the 'x' and 'y' co-ordinate for the month of march.
kde.sample.a14 <- assault.march[,c("x", "y")]
# converting the co-ordinates in numeric format
kde.sample.a14.march <- transform(kde.sample.a13, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of march.
kde.est.a14.march = run.spatial.kde(kde.sample.a14.march, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a14.march, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of april.
# getting the 'x' and 'y' co-ordinate for the month of april.
kde.sample.a15 <- assault.april[,c("x", "y")]
# converting the co-ordinates in numeric format.
kde.sample.a15.april <- transform(kde.sample.a15, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of april.
kde.est.a15.april = run.spatial.kde(kde.sample.a15.april, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a15.april, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of may.
# getting the 'x' and 'y' co-ordinate for the month of may.
kde.sample.a16 <- assault.may[,c("x", "y")]
# converting the co-ordinates in numeric format.
kde.sample.a16.may <- transform(kde.sample.a16, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of may.
kde.est.a16.may = run.spatial.kde(kde.sample.a16.may, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a16.may, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of june.
# getting the 'x' and 'y' co-ordinate for the month of june.
kde.sample.a17 <- assault.june[,c("x", "y")]
# converting the co-ordinates into numeric format.
kde.sample.a17.june <- transform(kde.sample.a17, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of june.
kde.est.a17.june = run.spatial.kde(kde.sample.a17.june, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a17.june, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# adding heading to the plot.
mtext("Heat Maps for the assault concentration from january to june", outer = TRUE)

# Creating a heat map for assault cases for the month of july.
# getting the 'x' and 'y' co-ordinate for the month of july.
kde.sample.a18 <- assault.july[,c("x", "y")]
# converting the co-ordinates into numeric format.
kde.sample.a18.july <- transform(kde.sample.a18, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of july.
kde.est.a18.july = run.spatial.kde(kde.sample.a18.july, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a18.july, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assaults cases for the month of august.
# getting the 'x' and 'y' co-ordinate for the month of august.
kde.sample.a19 <- assault.august[,c("x", "y")]
# converting the co-ordinates into numeric format.
kde.sample.a19.august <- transform(kde.sample.a19, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of august.
kde.est.a19.august = run.spatial.kde(kde.sample.a19.august, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a19.august, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of september.
# getting the 'x' and 'y' co-ordinate for the month of september.
kde.sample.a20 <- assault.september[,c("x", "y")]
# converting the co-ordinates into numeric format.
kde.sample.a20.september <- transform(kde.sample.a20, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of september.
kde.est.a20.september = run.spatial.kde(kde.sample.a20.september, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a20.september, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of october.
# getting the 'x' and 'y' co-ordinate for the month of october.
kde.sample.a21 <- assault.october[,c("x", "y")]
# converting the co-ordinates for the month of october.
kde.sample.a21.october <- transform(kde.sample.a21, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of october.
kde.est.a21.october = run.spatial.kde(kde.sample.a21.october, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a21.october, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of november.
# getting the 'x' and 'y' co-ordinate for the month of november.
kde.sample.a22 <- assault.november[,c("x", "y")]
# converting the co-ordinates for the month of november.
kde.sample.a22.november <- transform(kde.sample.a22, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of november.
kde.est.a22.november = run.spatial.kde(kde.sample.a22.november, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a22.november, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Creating a heat map for assault cases for the month of december.
# getting the 'x' and 'y' co-ordinate for the month of december.
kde.sample.a23 <- assault.december[,c("x", "y")]
# converting the co-ordinates into numeric format
kde.sample.a23.december <- transform(kde.sample.a23, x=as.numeric(x), y=as.numeric(y))

# run and plot KDE for the month of december
kde.est.a23.december = run.spatial.kde(kde.sample.a23.december, kde.est.points, 1000) 
plot.spatial.kde(kde.est.a23.december, kde.est.points)
# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# adding heading to the plot.
mtext("Heat Maps for the assault concentration from july to december", outer = TRUE)
par(obs6)
#
##CONCLUSION##
# When comparing with theft concentrations we can say:-
# In heat map of crime by thefts, the major concentration was present in the downtown 
# area of the city, but in the assault crime heat map, the spread of crime in more 
# uniformly spread across the city with hot spots in south and west end of the city.
# The thefts concentration peaked in the months of April, September and October
# but assault concentration remains spread throughout the year, just the location 
# of hot spots keeps varying a little with initial and later part of the year.
##
#
#
#---------------------------------------------------------------------------------
# Part-D
#-----------------------------------------------------------------------------------------------------
#
# Plot for KDE resolution =200
#-----------------------------------
kde.resolution.meters = 200
# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# getting the sample data
kde.assault <- crime.data2[,c("x", "y")]
kde.assault.data <- transform(kde.assault, x=as.numeric(x), y= as.numeric(y))

# get estimation points of KDE
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE
kde.est1 = run.spatial.kde(kde.assault.data, kde.est.points, 10000) # taking 10000 random observations from the sample
plot.spatial.kde(kde.est1, kde.est.points) # Plotting the heat map

# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# Plot for KDE resolution =1000m
#---------------------------------------------
kde.resolution.meters = 1000
# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# getting the sample data
kde.assault <- crime.data2[,c("x", "y")]
kde.assault.data <- transform(kde.assault, x=as.numeric(x), y= as.numeric(y))

# get estimation points of KDE
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE
kde.est1 = run.spatial.kde(kde.assault.data, kde.est.points, 10000) # taking 10000 random observations from the sample
plot.spatial.kde(kde.est1, kde.est.points) # Plotting the heat map

# add city boundary to KDE plot for interpretability
plot(city.boundary, axes=TRUE, border="black", asp=1, add=TRUE)

# KDE resolution divides the whole plot in grid and the value of KDE resolution is
# interpreted as the spacing between the points on which the estimate (heat map) 
# is plotted. Therefore, higher the resolution, higher is the spacing between the
# points, so fewer number of points on which the estimate made.
# 
#------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------
