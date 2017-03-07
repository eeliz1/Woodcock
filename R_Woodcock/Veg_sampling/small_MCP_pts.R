library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)

###################################
##Selecting 5 use points for MCPs
##too small to contain 5 points
###################################

##info for posix subsets;days=dates
hrs = c('07', '08', '09', 10:17)

days_68627 = c('04', '06')
days_68632 = c('04', '05', '09', '12', '13')
#days_68655 = c()
days_68657 = c('23', '20', '15', '16', '14', '19', '18', '21', '24')


##get location data path set up
setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/")

fileNames <- Sys.glob("*.csv")

##isolate birds with too small MCPs
small = c(fileNames[6])

for (i in small){
  ##read in data
  x <- read.csv(i, header=TRUE, sep=,, skip=4)
  df=x
  
  ##add Band Number
  df$BandNmbr = substr(i, 1, 9)
  ##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
  om <- with(df, (Latitude==0.00000))
  df <-df[!om,]
  
  ##assign coordinate reference
  coordinates(df)=c("Longitude", "Latitude")
  proj4string(df)<-CRS('+proj=longlat +datum=WGS84')
  
  ##assign object for corret days for individual bird
  dates = paste("days_", substr(i, 5, 9), sep="")
  days=get(dates)
  

  ##parse out correct datetimes for small MCPs
  df = subset(df, format(df$dt, '%d') %in% days)
  df = subset(df, format(df$dt, '%H') %in% hrs)
  
  ##need to isolate each individual day and save points
  mcps = unique(substr(df$dt, 1, 10))
  
  for (day in mcps){
    dfp = df[which(substr(df$dt, 1, 10)==day),]
    plot(dfp)
    
    ##save pts
    writeOGR(dfp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/MCP_pts",
             layer=paste(dfp$BandNmbr[1], "pts", day, "day", sep="_"), driver="ESRI Shapefile")
  }
  
}

