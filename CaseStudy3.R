# NAME: Prateek Agrawal
# Computing ID: pa7sb
# Homework: Case Study-3

#***************************************************************************
#                               CASE STUDY-3
#***************************************************************************
#
library(car)
source("CrimeUtil.R")

# read crime data in R
theft.data <- sample.crime("2014_THEFT.csv", -1, 1, 12)

# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# set prediction resolution
prediction.resolution.meters = 200

#********************************************************
# Evaluation of Monthly KDE, with 3 spatial factors
#*********************************************************

#####
##### train model on responses from April data, using predictors from January to March.
#####

# creating the predictore dataset
theft.jan.to.march <-  theft.data[theft.data$month == 1|
                                    theft.data$month == 2|
                                    theft.data$month == 3,]
theft.april <- theft.data[theft.data$month == 4, ]
theft.may <- theft.data[theft.data$month == 5, ]

par(mfrow=c(1,2))

# get negative observations within chicago
non.crime.points <-  cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <-  "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from april within chicago
training.crime.points <-  cbind(1, theft.april[,c("x","y")])
names(training.crime.points)[1] <-  "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

par(mfrow=c(1,1))

# combine positive and negative points
training.data <-  rbind(non.crime.points, training.crime.points)

# calculate crime density for each training point based on january to march records
theft.density <-  run.spatial.kde(theft.jan.to.march[,c("x","y")], training.data[,c("x","y")], 1000)

# calculate distances to nearest school from each training point
school.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/Schools/CPS_School_Locations_SY1415", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
school.min.distance <-  get.min.distances(training.data[,c("x","y")], school.points)

## calculate distances to nearest hospital from each training point
hospital.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/Hospitals/Hospitals", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
hospital.min.distance <-  get.min.distances(training.data[,c("x","y")], hospital.points)

# calculate distances to nearest Police station from each training point
policestation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
policestation.min.distance <-  get.min.distances(training.data[,c("x","y")], policestation.points)

# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, theft.density, school.min.distance, hospital.min.distance, policestation.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit <-  glm(response ~ . -x -y, data = training.data, family=binomial)
summary(glm.fit)
# the model is response = - 1.087 + 2.425e+08 * theft density - 4.552e-04*school.min.distance
# -1.170e-04*hospital.min.distance -3.451e-05* policestation.min.distance
#
# Checking the multicollinearity for the model
vif(glm.fit)
# It shows the extend to which the variance is inflated due to predictors. but as the
# values are within limits therefore, there is no multicollinearity.

#####
##### predict responses on May data, using the fitted model and predictors from April.
#####

# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density <-  run.spatial.kde(theft.april[,c("x","y")], prediction.points, max.sample.size=1000)
school.min.distance <-  get.min.distances(prediction.points, school.points)
hospital.min.distance <- get.min.distances(prediction.points, hospital.points)
policestation.min.distance <- get.min.distances(prediction.points, policestation.points)

prediction.data <- as.data.frame(cbind(prediction.points, theft.density, school.min.distance, hospital.min.distance, policestation.min.distance))

# run prediction
threats <-  predict(glm.fit, prediction.data, type="response")

# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction <- cbind(prediction.points, threats)
names(theft.prediction) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
#----------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#********************************************************
# Evaluation of Monthly KDE, with 1 spatial factors
#*********************************************************
# with school spatial data
#####
##### train model on responses from April data, using predictors from January to March.
#####

par(mfrow=c(1,2))

# get negative observations within chicago
non.crime.points <-  cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <-  "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from april within chicago
training.crime.points <-  cbind(1, theft.april[,c("x","y")])
names(training.crime.points)[1] <-  "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

par(mfrow=c(1,1))

# combine positive and negative points
training.data <-  rbind(non.crime.points, training.crime.points)

# calculate crime density for each training point based on january to march records
theft.density <-  run.spatial.kde(theft.jan.to.march[,c("x","y")], training.data[,c("x","y")], 1000)

# calculate distances to nearest school from each training point
school.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/Schools/CPS_School_Locations_SY1415", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
school.min.distance <-  get.min.distances(training.data[,c("x","y")], school.points)

# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, theft.density, school.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit.school <-  glm(response ~ . -x -y, data = training.data, family=binomial)
summary(glm.fit.school)
#
#####
##### predict responses on May data, using the fitted model and predictors from April.
#####

# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density <-  run.spatial.kde(theft.april[,c("x","y")], prediction.points, max.sample.size=1000)
school.min.distance <-  get.min.distances(prediction.points, school.points)

prediction.data1 <- as.data.frame(cbind(prediction.points, theft.density, school.min.distance))

# run prediction
threats1 <-  predict(glm.fit.school, prediction.data1, type="response")

# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction1 <- cbind(prediction.points, threats1)
names(theft.prediction1) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction1, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# with hospital spatial data
#####
##### train model on responses from April data, using predictors from January to March.
#####

par(mfrow=c(1,2))

# get negative observations within chicago
non.crime.points <-  cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <-  "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from april within chicago
training.crime.points <-  cbind(1, theft.april[,c("x","y")])
names(training.crime.points)[1] <-  "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

par(mfrow=c(1,1))

# combine positive and negative points
training.data <-  rbind(non.crime.points, training.crime.points)

# calculate crime density for each training point based on january to march records
theft.density <-  run.spatial.kde(theft.jan.to.march[,c("x","y")], training.data[,c("x","y")], 1000)

# calculate distances to nearest hospital from each training point
hospital.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/Hospitals/Hospitals", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
hospital.min.distance <-  get.min.distances(training.data[,c("x","y")], hospital.points)

# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, theft.density, hospital.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit.hospital <-  glm(response ~ . -x -y, data = training.data, family=binomial)
summary(glm.fit.hospital)
#
#####
##### predict responses on May data, using the fitted model and predictors from April.
#####

# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density <-  run.spatial.kde(theft.april[,c("x","y")], prediction.points, max.sample.size=1000)
hospital.min.distance <-  get.min.distances(prediction.points, hospital.points)

prediction.data2 <- as.data.frame(cbind(prediction.points, theft.density, hospital.min.distance))

# run prediction
threats2 <-  predict(glm.fit.hospital, prediction.data2, type="response")

# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction2 <- cbind(prediction.points, threats2)
names(theft.prediction2) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction2, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# with Police Station spatial data
#####
##### train model on responses from April data, using predictors from January to March.
#####

par(mfrow=c(1,2))

# get negative observations within chicago
non.crime.points <-  cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <-  "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from april within chicago
training.crime.points <-  cbind(1, theft.april[,c("x","y")])
names(training.crime.points)[1] <-  "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

par(mfrow=c(1,1))

# combine positive and negative points
training.data <-  rbind(non.crime.points, training.crime.points)

# calculate crime density for each training point based on january to march records
theft.density <-  run.spatial.kde(theft.jan.to.march[,c("x","y")], training.data[,c("x","y")], 1000)

# calculate distances to nearest Police station from each training point
policestation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
policestation.min.distance <-  get.min.distances(training.data[,c("x","y")], policestation.points)

# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, theft.density, policestation.min.distance)

# fit GLM -- don't use x- and y-coordinates as predictors.
glm.fit.policestation <-  glm(response ~ . -x -y, data = training.data, family=binomial)
summary(glm.fit.policestation)
#
#####
##### predict responses on May data, using the fitted model and predictors from April.
#####

# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density <-  run.spatial.kde(theft.april[,c("x","y")], prediction.points, max.sample.size=1000)
policestation.min.distance <-  get.min.distances(prediction.points, policestation.points)

prediction.data3 <- as.data.frame(cbind(prediction.points, theft.density, policestation.min.distance))

# run prediction
threats3 <-  predict(glm.fit.policestation, prediction.data3, type="response")

# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction3 <- cbind(prediction.points, threats3)
names(theft.prediction3) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction3, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------