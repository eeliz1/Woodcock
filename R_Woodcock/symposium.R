library(plyr)
library(dplyr)
library(rgdal)
###################################################################################
###################################################################################
###Selecting random points
###################################################################################
###################################################################################


########################
##season 1
########################


setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season1/")
files = Sys.glob("*.csv")

for(j in 1:length(files)){
df = read.csv(files[j],header=TRUE)

##for bird 171368602
if (df$BandNmbr[1]==171368602){
  df = df[1:265,]
}


##omit non-fixes
om <- with(df, (Latitude==0.00000))
df <-df[!om,]
##add date-times
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
df$dt=df$gmt-(60*60*6)


##day points only
df <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))

x = unique(substr(df$dt, 1, 10))

##create data container
points = NULL

for (i in x){
  temp_df = df[which(substr(df$dt, 1, 10)==i),]
  n = nrow(temp_df)
  
  ##select random point
  pt = temp_df[sample(1:n, 1),]
  
  points = rbind(points, pt)
}


#test = points
#coordinates(test)=c("Longitude", "Latitude")
#proj4string(test)<-CRS('+proj=longlat +datum=WGS84')
#plot(test)

write.csv(points, file=paste("C:/Users/Elisa/Documents/Woodcock/Thesis/sym/", 
                             points$BandNmbr[1], "_random_pts.csv", sep=""))
}


########################
##season 2
########################

setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/")
files = Sys.glob("*.csv")

for(j in 1:length(files)){
  df = read.csv(files[j],skip=4, header=TRUE)
  df$BandNmbr = substr(files[j], 1, 9)

  ##omit non-fixes
  om <- with(df, (Latitude==0.00000))
  df <-df[!om,]
  ##add date-times
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  
  
  ##day points only
  df <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
  
  x = unique(substr(df$dt, 1, 10))
  
  ##create data container
  points = NULL
  
  for (i in x){
    temp_df = df[which(substr(df$dt, 1, 10)==i),]
    n = nrow(temp_df)
    
    ##select random point
    pt = temp_df[sample(1:n, 1),]
    
    points = rbind(points, pt)
  }
  
  
  #test = points
  #coordinates(test)=c("Longitude", "Latitude")
  #proj4string(test)<-CRS('+proj=longlat +datum=WGS84')
  #plot(test)
  
  write.csv(points, file=paste("C:/Users/Elisa/Documents/Woodcock/Thesis/sym/", 
                               points$BandNmbr[1], "_random_pts.csv", sep=""))
}





###################################################################################
###################################################################################
###
###################################################################################
###################################################################################

setwd("C:/Users/Elisa/Documents/Woodcock/Thesis/sym")

##read in and combine files if new session
files = Sys.glob("*.csv")
cf=ldply(files, read.csv)

##format df; dt should be POSIXct, lat/long should be numeric
cf$dt = as.POSIXct(cf$dt)

##take each random point for each bird/day
##get a random direction and a specified distance

##distances from point in m
d=15

##storage vector
samp_pts = c()

for (i in 1:nrow(cf)){
  ##select random direction
  theta = sample(0:360, 1)
  
  ##current random point
  lat = cf$Latitude[i]
  long = cf$Longitude[i]
  
  ##get from d in m to degrees
  dx = d*sin(theta)  
  dy = d*cos(theta)  
  
  delta_longitude = dx/(111320*cos(lat))  # dx, dy in meters
  delta_latitude = dy/110540        
  
  ##sampling point values
  new_lat = cf$Latitude[i]+delta_latitude
  new_long = cf$Longitude[i]+delta_longitude
  
  ##store info
  samp_pts = rbind(samp_pts, c(lat, long, new_lat, new_long))

  
}



samp_pts = as.data.frame(samp_pts)
colnames(samp_pts) <- c("Location_Latitude", "Location_Longitude",
                        "Sample_Latitude", "Sample_Longitude")

write.csv(samp_pts, file=paste("C:/Users/Elisa/Documents/Woodcock/Thesis/", d,
                               "m_samppts_2.csv",sep=""))




#################################################################
##random sample point
#################################################################
##distances from point in m
d=c(10,20,30,45,60, 75, 100)

for(j in 1:length(d)){

setwd("C:/Users/Elisa/Documents/Woodcock/Thesis/sym")

##read in and combine files if new session
files = Sys.glob("*.csv")
cf=ldply(files, read.csv)

##format df; dt should be POSIXct, lat/long should be numeric
cf$dt = as.POSIXct(cf$dt)

##take each random point for each bird/day
##get a random direction and a specified distance

##storage vector
samp_pts = c()

for (i in 1:nrow(cf)){
  ##select random direction
  theta = sample(0:360, 1)
  
  ##current random point
  lat = cf$Latitude[i]
  long = cf$Longitude[i]
  
  ##get from d in m to degrees
  dx = d[j]*sin(theta)  
  dy = d[j]*cos(theta)  
  
  delta_longitude = dx/(111320*cos(lat))  # dx, dy in meters
  delta_latitude = dy/110540        
  
  ##sampling point values
  new_lat = cf$Latitude[i]+delta_latitude
  new_long = cf$Longitude[i]+delta_longitude
  
  ##store info
  samp_pts = rbind(samp_pts, c(lat, long, new_lat, new_long))

}



samp_pts = as.data.frame(samp_pts)
colnames(samp_pts) <- c("Location_Latitude", "Location_Longitude",
                        "Sample_Latitude", "Sample_Longitude")
##store paired points
write.csv(samp_pts, file=paste("C:/Users/Elisa/Documents/Woodcock/Data/GIS",
                               d[j],"m_samppts_3.csv",sep=""))

##shape file of random points only
coordinates(samp_pts)=c("Sample_Longitude", "Sample_Latitude")
proj4string(samp_pts)<-CRS('+proj=longlat +datum=WGS84')

writeOGR(samp_pts, "C:/Users/Elisa/Documents/Woodcock/ArcGIS/Symposium/Sample10",
         layer=(paste(d[j], "m_random_pts", sep="")), driver="ESRI Shapefile")
}