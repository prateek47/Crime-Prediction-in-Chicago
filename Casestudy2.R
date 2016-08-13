library(ks)
library(RColorBrewer)
# changing the round off digits in AUC to 3 from 2.
source("CrimeUtil.R")

kde.resolution.meters = 200

# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# read crime data in R
theft.data <- sample.crime("2014_THEFT.csv", -1, 1, 12)
#
#*****************************
# Evaluation of the monthly KDE
#*****************************
#
## Creation of KDE for the months of Jan-to-April and Evaluating it for the month of May
#
# Creating the Training dataset
theft.jan_apr <- theft.data[theft.data$month == 1|
                            theft.data$month == 2|
                            theft.data$month == 3|
                            theft.data$month == 4, ]
# get KDE sample data from 
kde.sample.points.jan_apr = theft.jan_apr[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.jan_apr = run.spatial.kde(kde.sample.points.jan_apr, kde.est.points, 500) 
plot.spatial.kde(kde.est.jan_apr, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####

# Creating the dataset for the month of may
theft.may <- theft.data[theft.data$month == 5, ]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.jan_apr = as.data.frame(cbind(kde.est.points, kde.est.jan_apr))
names(theft.prediction.jan_apr) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on May
eval.crime.points.may = as.data.frame(theft.may[,c("x","y")])
names(eval.crime.points.may) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.jan_apr, eval.crime.points.may, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.708, inputting this value in a data frame for better evaluation.
AUC.Monthly <- data.frame(matrix( ncol = 3))
colnames(AUC.Monthly) <- c("Training dataset", "Evaluation dataset", "AUC Value")
AUC.Monthly[1, ] <- c("january-April", "May", 0.708)
#----------
## Creation of KDE for the months of July-October and Evaluating it for the month of November
#
# Creating the Training dataset
theft.jul_oct <- theft.data[theft.data$month == 7|
                              theft.data$month == 8|
                              theft.data$month == 9|
                              theft.data$month == 10, ]
# get KDE sample data from 
kde.sample.points.jul_oct = theft.jul_oct[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.jul_oct = run.spatial.kde(kde.sample.points.jul_oct, kde.est.points, 500) 
plot.spatial.kde(kde.est.jul_oct, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####
# Creating a dataset for november
theft.november <- theft.data[theft.data$month == 11, ]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.jul_oct = as.data.frame(cbind(kde.est.points, kde.est.jul_oct))
names(theft.prediction.jul_oct) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on May
eval.crime.points.nov = as.data.frame(theft.november[,c("x","y")])
names(eval.crime.points.nov) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.jul_oct, eval.crime.points.nov, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.710, inputting this value in a data frame for better evaluation.
AUC.Monthly[2, ] <- c("july-October", "November", 0.710)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#
# ********************************
# Evaluation of the Day wise KDE
# *********************************
#
# Taking the Wednesday's from the month of January to April and then predicting on the 
# Wednesday of the month of May
# Creating the dataset for use.
theft.wed.jan_apr <- theft.data[(theft.data$month == 1|
                            theft.data$month == 2|
                            theft.data$month == 3|
                            theft.data$month == 4) &
                            (theft.data$day.of.week == 4), ]

# get KDE sample data from 
kde.sample.points.wed.jan_apr = theft.wed.jan_apr[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.wed.jan_apr = run.spatial.kde(kde.sample.points.wed.jan_apr, kde.est.points, 500) 
plot.spatial.kde(kde.est.wed.jan_apr, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####

# Creating the data set
theft.wed.may <- theft.data[(theft.data$month == 5) & (theft.data$day.of.week == 4),]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.wed.jan_apr = as.data.frame(cbind(kde.est.points, kde.est.wed.jan_apr))
names(theft.prediction.wed.jan_apr) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on May
eval.crime.points.wed.may = as.data.frame(theft.wed.may[,c("x","y")])
names(eval.crime.points.wed.may) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.wed.jan_apr, eval.crime.points.wed.may, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.702, inputting this value in a data frame for better evaluation.
AUC.dayly <- data.frame(matrix( ncol = 3))
colnames(AUC.dayly) <- c("Training dataset", "Evaluation dataset", "AUC Value")
AUC.dayly[1, ] <- c("Wednesday; Jan-Apr", "Wednesday; May", 0.702)
#--------------
#
# Taking the Saturday's from the month of January to April and then predicting on the 
# Saturday of the month of May
# Creating the dataset for use.
theft.sat.jan_apr <- theft.data[(theft.data$month == 1|
                                   theft.data$month == 2|
                                   theft.data$month == 3|
                                   theft.data$month == 4) &
                                  (theft.data$day.of.week == 7), ]

# get KDE sample data from 
kde.sample.points.sat.jan_apr = theft.sat.jan_apr[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.sat.jan_apr = run.spatial.kde(kde.sample.points.sat.jan_apr, kde.est.points, 500) 
plot.spatial.kde(kde.est.sat.jan_apr, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####

# Creating the data set
theft.sat.may <- theft.data[(theft.data$month == 5) & (theft.data$day.of.week == 7),]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.sat.jan_apr = as.data.frame(cbind(kde.est.points, kde.est.sat.jan_apr))
names(theft.prediction.sat.jan_apr) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on May
eval.crime.points.sat.may = as.data.frame(theft.sat.may[,c("x","y")])
names(eval.crime.points.sat.may) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.sat.jan_apr, eval.crime.points.sat.may, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.742, inputting this value in a data frame for better evaluation.
AUC.dayly[2, ] <- c("Saturday; Jan-Apr", "Saturday; May", 0.742)
#------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#
# ********************************
# Evaluation of the Hourly KDE
# *********************************
#
# Creating a dataset by taking the time from 3am-4am for every day in month of january and 
# predicting the same time in the month of feburary
# Creating the dataset for use.
theft.jan.3am_4am <- theft.data[(theft.data$month == 1) &
                                  (theft.data$hour == 3), ]

# get KDE sample data from 
kde.sample.points.jan.3am_4am = theft.jan.3am_4am[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.jan.3am_4am = run.spatial.kde(kde.sample.points.jan.3am_4am, kde.est.points, 500) 
plot.spatial.kde(kde.est.jan.3am_4am, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####

# Creating the data set
theft.feb.3am_4am <- theft.data[(theft.data$month == 2) & (theft.data$hour == 3),]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.jan.3am_4am = as.data.frame(cbind(kde.est.points, kde.est.jan.3am_4am))
names(theft.prediction.jan.3am_4am) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on 3am_4am in the month of Feburary
eval.crime.points.feb.3am_4am = as.data.frame(theft.feb.3am_4am[,c("x","y")])
names(eval.crime.points.feb.3am_4am) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.jan.3am_4am, eval.crime.points.feb.3am_4am, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.668, inputting this value in a data frame for better evaluation.
AUC.hourly <- data.frame(matrix( ncol = 3))
colnames(AUC.hourly) <- c("Training dataset", "Evaluation dataset", "AUC Value")
AUC.hourly[1, ] <- c("3am_4am; Jan", "3am_4am; Feburary", 0.668)
#-------------
#
# Creating a dataset by taking the time from 3pm-4pm for every day in month of january and 
# predicting the same time in the month of feburary
# Creating the dataset for use.
theft.jan.3pm_4pm <- theft.data[(theft.data$month == 1) &
                                  (theft.data$hour == 15), ]

# get KDE sample data from 
kde.sample.points.jan.3pm_4pm = theft.jan.3pm_4pm[ ,c("x","y")]

# get estimation points for KDE -- a grid of evenly spaced points
kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)

# run and plot KDE, using 500 points from the sample
kde.est.jan.3pm_4pm = run.spatial.kde(kde.sample.points.jan.3pm_4pm, kde.est.points, 500) 
plot.spatial.kde(kde.est.jan.3pm_4pm, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add=TRUE)

#####
##### Evaluation of KDE via surveillance plots.
#####

# Creating the data set
theft.feb.3pm_4pm <- theft.data[(theft.data$month == 2) & (theft.data$hour == 15),]

# build prediction matrix for evaluation -- requires x, y, and threat columns
theft.prediction.jan.3pm_4pm = as.data.frame(cbind(kde.est.points, kde.est.jan.3pm_4pm))
names(theft.prediction.jan.3pm_4pm) = c("x","y","threat")

# build crime matrix for evaluation -- evaluate on 3am_4am in the month of Feburary
eval.crime.points.feb.3pm_4pm = as.data.frame(theft.feb.3pm_4pm[,c("x","y")])
names(eval.crime.points.feb.3pm_4pm) = c("x","y")

# plot results
plot.surveillance.curve(theft.prediction.jan.3pm_4pm, eval.crime.points.feb.3pm_4pm, kde.resolution.meters, city.boundary)

# The AUC value comes out to be 0.686, inputting this value in a data frame for better evaluation.
AUC.hourly[2, ] <- c("3pm_4pm; Jan", "3pm_4pm; Feburary", 0.686)
#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------

