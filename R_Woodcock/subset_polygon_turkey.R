###
#  Base Brood Range Code
###
install.packages("move")
install.packages("rgdal")
install.packages("PBSmapping")
install.packages("maptools")


library(move)
library(rgdal)
library(PBSmapping)
library(maptools)

##C:\Users\Nick\Desktop\Download\CSV_Incubated_Birds

#You will need to change this line to where your input file is at.
x=read.csv("C:/Users/Elisa/Documents/scott/GPS_Collar60492_160906132201.csv", header=TRUE)
head(x)
x=x[,-c(6:8,11:46)]

#Creates numeric lat/lon data
x$Lat=as.numeric(as.character(x$Latitude))
x$Lon=as.numeric(as.character(x$Longitude))
#Format new time variable
x$DateTime=strptime(paste(x$LMT_Date, x$LMT_Time),"%d.%m.%Y %H:%M:%S")
#x=x[x$DateTime !="1",]
x=x[order(x$DateTime),]
x$yr=format(x$DateTime, "%Y")
head(x)
#x=x[x$yr<= "2016",]
summary(x$DateTime)

#####subset times based on Landon's schedule

x <- subset(x, format(x$DateTime, '%H') %in% c('06', '14', '16', '18', '20', '22'))


#Create object of class: move and plots them
move.out <- move(x=x$Lon, y=x$Lat, time=as.POSIXct(x$DateTime, tz="GMT"), data=x, proj=CRS("+proj=longlat +datum=WGS84"), animal="xxx")
#plot(move.out)
#new object defined
object<-move.out

# date min/max function (just subsetting based on move object timestamps slot)
datefun <- function(daymin,daymax){object[slot(object, "timestamps") >= daymin & slot(object, "timestamps") <= daymax,]}
#function output for arbitrary date
##
###  this is where you change to the date range
##
out2=datefun("2016-05-12 00:00:00", "2016-05-22 24:00:00")
out3 <- spTransform(out2, CRSobj=CRS("+proj=aeqd"), center=TRUE)
res <- brownian.bridge.dyn(out3, location.error=15, margin=5, window.size=21, dimSize=85, ext=1)
plot(res)
res2<-raster2contour(res, level=c(0.5,0.75,0.99))
res3<-spTransform(res2, CRS("+proj=longlat  +datum=WGS84"))
#########
##writing to polygon instead
##of polylines
##specific to brownian bridge 
res4 <- SpatialLines2PolySet(res3)
res5 <- PolySet2SpatialPolygons(res4)
res6 <- as(res5, "SpatialPolygonsDataFrame")
########
plot(res6)


#for writing shapefile
#
#change location to write shapefiles here
writeOGR(res6, "C:/Users/Elisa/Documents/scott", "nickshit_poly", driver="ESRI Shapefile")
