library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)


df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/Veg_pt_prep/TENS_veg_pts_Jan6export.csv")
df$Id <- NULL
df$REFFID <- NULL
df$REFId <- NULL
df$PNTID <- NULL

n = nrow(df)
total = trunc(nrow(df)/3)

pos = sample(1:n, total, replace=FALSE)

samps = df[pos,]
s2 = df[pos,]

##assign coordinate reference
coordinates(samps)=c("POINT_X", "POINT_Y")
proj4string(samps)<-CRS('+proj=longlat +datum=WGS84')

sp = SpatialPointsDataFrame(samps, s2)

##Write to file
writeOGR(sp, "C:/Users/Elisa/Documents/Woodcock/Data/GIS/Veg_pts",
         layer="TENS_Buffer_veg_pts_1", driver="ESRI Shapefile")




