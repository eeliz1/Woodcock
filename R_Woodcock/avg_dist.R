library(sp)
library(geosphere)
library(plr)

setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/all")

move_files <- Sys.glob("*.csv")
##get averages
time = NULL
move = NULL
hrs = c('09', 10:15)

for (file in move_files){
  md <- read.csv(file, header=TRUE, skip=4)
  
  ##create POSIX object
  md$gmt=strptime(paste(md$GMT.Time),"%m/%d/%Y %H:%M")
  md$dt=md$gmt-(60*60*6)
  md=md[order(md$dt),]
  
  ##omit non-fixes
  om <- with(md, (Latitude==0.00000))
  md <-md[!om,]
  
  ##from Woodcock move
  xy=cbind(md$Longitude, md$Latitude)
  
  spdf=SpatialPointsDataFrame(xy, md, proj4string = CRS("+proj=longlat +datum=WGS84"))
  #plot(spdf)
  spdf1=spdf[-nrow(spdf),]
  spdf2=spdf[-1,]
  md=md[-nrow(md),]
  md$spdfdist=distVincentyEllipsoid(spdf1, spdf2)
  #head(md)
  md=md[md$spdf<=5000,]
  
  ##set time
  dfp = subset(md, format (md$dt, '%H') %in% hrs)
  
  ##generate and save plot
  move = c(move, dfp$spdfdist)
}


move

move2 = move[which(move<500)]

mean(move2)
