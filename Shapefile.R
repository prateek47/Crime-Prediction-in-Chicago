library(maptools)
library(rgdal)
source("CrimeUtil.R")

# read shapefile describing the city boundary of Chicago
city.boundary = readShapePoly("C:/UVa/Data_Mining/Examples/Data/City_Boundary/City_Boundary")
# The readShapePoly reads data from a polygon shapefile into a SpatialPolygonsDataFrame object. 
# The writePolyShape function writes data from a SpatialPolygonsDataFrame object to a shapefile. 

# set projection
proj4string(city.boundary) = "+init=epsg:3435"
# Sets or retrieves projection attributes on classes extending SpatialData;

# reproject to meters
city.boundary = spTransform(city.boundary, CRS("+init=epsg:26971"))
# object with coordinates transformed to the new coordinate reference system.

# display shapefile
plot(city.boundary, axes=TRUE, border="black", asp=1)
bbox(city.boundary)  # show bounding box coordinates

# display school points using the convenient read.shapefile function (from CrimeUtil.R), which combines the above functions into one.
school.points = read.shapefile("C:/UVa/Data_Mining/Examples/Data/Schools/CPS_School_Locations_SY1415", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
points(school.points, pch=".")

# display landmarks and their central points
plot(city.boundary, axes=TRUE, border="black", asp=1)
landmarks = read.shapefile("C:/UVa/Data_Mining/Examples/Data/Landmarks/LandmarksNationalRegister_nov2012", "poly", "+init=epsg:3435", "+init=epsg:26971")
plot(landmarks, add=TRUE)
points(coordinates(landmarks))

# instead of central points, show all points from the landmark polygons
landmark.points = c()
landmark.polys = slot(landmarks,"polygons")
for (i in 1:length(landmark.polys))
{
  polygons = slot(landmark.polys[[i]],"Polygons")
  points = slot(polygons[[1]],"coords")
  landmark.points = rbind(landmark.points, points)
}

plot(city.boundary, axes=TRUE, border="black", asp=1)
points(landmark.points, pch=".")
