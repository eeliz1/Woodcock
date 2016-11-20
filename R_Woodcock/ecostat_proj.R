library(plyr)


setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/DERI")

phase=c(57,48,39,30,21,14,8,3,1,1,3,8,15,24,34,45,57,
        68,78,86,93,98,100,100,98,95,89,82,74,66, 57)
d=read.csv("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/January.csv")

mp=cbind(phase, d)
mp$date=strptime(paste(d[,1]),"%m/%d/%Y")
mp$date=format(mp$date, "%m/%d/%Y")


##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames
df=ldply(fileNames, read.csv)
head(df)

##conversions to df
df$Latitude=as.numeric(as.character(df$Latitude))
df$Longitude=as.numeric(as.character(df$Longitude))
##Create datetime column 
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
df$dt=df$gmt-(60*60*6)
df$date=format(df$dt, "%m/%d/%Y")
df$mp = as.character(1)


##adding moon phase to dataframe
for (i in 1:nrow(df)){
  for (j in 1:nrow(mp)){
    if (df$date[i] == mp$date[j]){
      df$mp[i]=mp$phase[j]
    }
  }
}


##cleaning up df
df$gmt <- NULL
df$Duration <- NULL
df$DOP <- NULL
df$Satellites <- NULL
df$GMT.Time <- NULL
om <- with(df, (Latitude==0.00000))
df <-df[!om,]

#########################################################################
##What now????
##determining field versus woods
##GIS extract a label for each location??

##Creating spatial aware data frame
df$Lon = df$Longitude
df$Lat = df$Latitude

coordinates(df)=c("Lon", "Lat")
proj4string(df)<-CRS('+proj=longlat +datum=WGS84')



x = bbox(df)
x
