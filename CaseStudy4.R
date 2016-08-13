# Name: Prateek Agrawal
# Computing ID: pa7sb
#
#************************************************************************************************
#                                      CASE STUDY-4
#**********************************************************************************************

library(car)
source("CrimeUtil.R")
library(kernlab)

# read crime data in R
theft.data <- sample.crime("2014_THEFT.csv", -1, 1, 12)

# read chicago boundary
city.boundary <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/City_Boundary/City_Boundary", "poly", "+init=epsg:3435", "+init=epsg:26971")

# set prediction resolution
prediction.resolution.meters = 200

#####
##### train model on responses from April data, using predictors from January to March.
#####

# creating the predictore dataset
theft.jan.to.march <-  theft.data[theft.data$month == 1|
                                    theft.data$month == 2|
                                    theft.data$month == 3, ]
theft.april <- theft.data[theft.data$month == 4, ]
theft.may <- theft.data[theft.data$month == 5, ]

par(mfrow=c(1,2))

# get negative observations within chicago( The points with no crime)
non.crime.points <-  cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
names(non.crime.points)[1] <-  "response"
plot(non.crime.points[,c("x","y")], pch=".", asp=1)

# get positive observations from april within chicago ( The points where crime occurred)
training.crime.points <-  cbind(1, theft.april[,c("x","y")])
names(training.crime.points)[1] <-  "response"
plot(training.crime.points[,c("x","y")], pch=".", asp=1)

par(mfrow=c(1,1))


##### 1st Analysis ######
# (Both spatial factors and kde is used for analysis)

#
## Training Data ##
#-------------------
#
# combining positive and negative points to create the training data
training.data <-  rbind(non.crime.points, training.crime.points)

# calculate crime density for each training point based on january to march records
theft.density <-  run.spatial.kde(theft.jan.to.march[ ,c("x","y")], training.data[ ,c("x","y")], 1000)

# hypothesizing that Hospital has a non linear relationship with the crime occurences
## calculate distances to nearest hospital from each training point
hospital.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/Hospitals/Hospitals", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
hospital.min.distance <-  get.min.distances(training.data[,c("x","y")], hospital.points)

# calculate distances to nearest Police station from each training point
policestation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
policestation.min.distance <-  get.min.distances(training.data[,c("x","y")], policestation.points)

## calculate distances to nearest hospital from each training point
railwaystation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/CTA_Stations/CTA_Stations", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
railwaystation.min.distance <-  get.min.distances(training.data[,c("x","y")], railwaystation.points)

#
# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, theft.density, hospital.min.distance, policestation.min.distance, railwaystation.min.distance)


#****************************Logistic Regression**********************************
#
# we are not going to use the x- and y-coordinates as predictors.
glm.fit <-  glm(response ~ . -x -y, data = training.data, family=binomial)
#
summary(glm.fit)
# the model is response y = -1.477 +  3.248e(8)*theft.density - 1.554e(-4)*hospital.min.distance
#
# Checking the multicollinearity in the model
vif(glm.fit)
# It shows the extend to which the variance is inflated due to predictors. but as the
# values are within limits(<5) therefore, there is no multicollinearity.

#**************************** Support Vector Machines**********************************
#
# Creating the SVM models...
# A linear SVM
svm.fit <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="vanilladot", C=10, prob.model = TRUE)

# A degree 2 polynomial SVM
svm.fit2 <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="polydot", kpar=list(degree=2), C=10, prob.model = TRUE)

# An RBF kernel SVM
svm.fit3 <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="rbfdot", C=10, prob.model = TRUE)
--------------------------------------------------------------------------------------------------------------------------------------------
#
# After creation of the model we will the predict the data for the month of may!!
#
#####
##### predict responses on May data
#####

## Testing Data
#---------------
#
# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
theft.density <-  run.spatial.kde(theft.jan.to.march[,c("x","y")], prediction.points, max.sample.size=1000)
hospital.min.distance <- get.min.distances(prediction.points, hospital.points)
policestation.min.distance <- get.min.distances(prediction.points, policestation.points)
railwaystation.min.distance <- get.min.distances(prediction.points, railwaystation.points)

prediction.data <- as.data.frame(cbind(prediction.points, theft.density, hospital.min.distance, policestation.min.distance, railwaystation.min.distance))

#****************************Logistic Regression**********************************
#
# run prediction
threats <-  predict(glm.fit, prediction.data, type="response")
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction <- cbind(prediction.points, threats)
names(theft.prediction) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.714
#
#**************************** Support Vector Machines**********************************
#
## a) A linear SVM, we get the probabilities
threats1 <- predict(svm.fit, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction1 <- cbind(prediction.points, threats1)
names(theft.prediction1) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction1, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.306
#----------------------------------------------------------------------------------------------------------
#
# b.) A degree-2 polynomial kernel SVM
threats2 <- predict(svm.fit2, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction2 <- cbind(prediction.points, threats2)
names(theft.prediction2) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction2, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.326
#------------------------------------------------------------------------------------------------------------------
#
# c.) An RBF kernel SVM
threats3 <- predict(svm.fit3, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction3 <- cbind(prediction.points, threats3)
names(theft.prediction3) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction3, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.348
#--------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------


##### 2nd Analysis ##########
## (only spatial factors is used for analysis)

#
## Training Data ##
#-------------------
#
# combining positive and negative points to create the training data
training.data <-  rbind(non.crime.points, training.crime.points)

# hypothesizing that Hospital has a non linear relationship with the crime occurences
## calculate distances to nearest hospital from each training point
hospital.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/Hospitals/Hospitals", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
hospital.min.distance <-  get.min.distances(training.data[,c("x","y")], hospital.points)

# calculate distances to nearest Police station from each training point
policestation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/PoliceStations/PoliceStationsDec2012", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
policestation.min.distance <-  get.min.distances(training.data[,c("x","y")], policestation.points)

## calculate distances to nearest hospital from each training point
railwaystation.points <-  read.shapefile("C:/UVa/Data_Mining/Examples/Data/CTA_Stations/CTA_Stations", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
railwaystation.min.distance <-  get.min.distances(training.data[,c("x","y")], railwaystation.points)

#
# add predictor columns (theft density and school distances) to training data
training.data <-  cbind(training.data, hospital.min.distance, policestation.min.distance, railwaystation.min.distance)


#****************************Logistic Regression**********************************
#
# we are not going to use the x- and y-coordinates as predictors.
glm.fit <-  glm(response ~ . -x -y, data = training.data, family=binomial)
#
summary(glm.fit)
# the model is response y = -1.477 +  3.248e(8)*theft.density - 1.554e(-4)*hospital.min.distance
#
# Checking the multicollinearity in the model
vif(glm.fit)
# It shows the extend to which the variance is inflated due to predictors. but as the
# values are within limits(<5) therefore, there is no multicollinearity.

#**************************** Support Vector Machines**********************************
#
# Creating the SVM models...
# A linear SVM
svm.fit <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="vanilladot", C=10, prob.model = TRUE)

# A degree 2 polynomial SVM
svm.fit2 <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="polydot", kpar=list(degree=2), C=10, prob.model = TRUE)

# An RBF kernel SVM
svm.fit3 <- ksvm(response ~ ., data = training.data, type="C-svc", kernel="rbfdot", C=10, prob.model = TRUE)
--------------------------------------------------------------------------------------------------------------------------------------------
  #
  # After creation of the model we will the predict the data for the month of may!!
  #
  #####
##### predict responses on May data
#####

## Testing Data
#---------------
#
# build dataframe to predict, based on april's data
prediction.points <-  get.grid.points(city.boundary, prediction.resolution.meters, TRUE)
hospital.min.distance <- get.min.distances(prediction.points, hospital.points)
policestation.min.distance <- get.min.distances(prediction.points, policestation.points)
railwaystation.min.distance <- get.min.distances(prediction.points, railwaystation.points)

prediction.data <- as.data.frame(cbind(prediction.points, hospital.min.distance, policestation.min.distance, railwaystation.min.distance))

#****************************Logistic Regression**********************************
#
# run prediction
threats <-  predict(glm.fit, prediction.data, type="response")
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction <- cbind(prediction.points, threats)
names(theft.prediction) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.678
#
#**************************** Support Vector Machines**********************************
#
## a) A linear SVM, we get the probabilities
threats1 <- predict(svm.fit, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction1 <- cbind(prediction.points, threats1)
names(theft.prediction1) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction1, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.32
#----------------------------------------------------------------------------------------------------------
#
# b.) A degree-2 polynomial kernel SVM
threats2 <- predict(svm.fit2, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction2 <- cbind(prediction.points, threats2)
names(theft.prediction2) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction2, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.464
#------------------------------------------------------------------------------------------------------------------
#
# c.) An RBF kernel SVM
threats3 <- predict(svm.fit3, newdata = prediction.data, type = "probabilities")
#
# build prediction dataframe for evaluation -- must have x, y, and threat columns
theft.prediction3 <- cbind(prediction.points, threats3)
names(theft.prediction3) <- c("x", "y", "threat")

# evaluate prediction on ground-truth crime records from march
plot.surveillance.curve(theft.prediction3, theft.may[,c("x","y")], prediction.resolution.meters, city.boundary)
#
# The AUC value is 0.33
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------

