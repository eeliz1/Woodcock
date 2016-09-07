#Example Code for Lisa
library(rgdal)
library(sp)
library(maptools)
library(geosphere)
library(PBSmapping)
library(ggplot2)
library(plyr)
library(scales)
library(gridExtra)
library(mgcv) 

setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint")
df=read.csv("pinpoints_season_1.csv", header=TRUE)
str(df)
head(df)
##df$Latitude <- as.character(df$Latitude)
##df$Latitude[df$Latitude == "0"] <-"NA"
##is.na(df)
##df=na.omit(df$Latitude)
##tail(df)

#create date/time object-You will need to fix to the pintpoint data
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
df$dt=df$gmt-(60*60*6)
df=df[order(df$dt),]
#Create numeric coordinates
#df$Latitude=as.numeric(as.character(df$Latitude))
#df$Longitude=as.numeric(as.character(df$Longitude))



df=df[!is.na(df$Latitude) | !is.na(df$Longitude),]
xy=df[,c(4,3)]
spdf=SpatialPointsDataFrame(xy, df, proj4string = CRS("+proj=longlat +datum=WGS84"))
##?SpatialPointsDataFrame()
plot(spdf)
spdf1=spdf[-nrow(spdf),]
spdf2=spdf[-1,]
df=df[-nrow(df),]
df$spdfdist=distVincentyEllipsoid(spdf1, spdf2)
head(df)
df=df[df$spdf<=5000,]
plot(df$dt, df$spdfdist, type="l")
summary(df$spdfdist)
