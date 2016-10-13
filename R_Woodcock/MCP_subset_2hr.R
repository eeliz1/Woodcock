library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)

############################################################
#######Takes pinpoint files and generates MCP (.shp) for each
#######day and night of each bandnumber
############################################################

##set working directory to folder with pinpoint files
setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season1")



##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames

for(fileName in fileNames){
  #browser()
  x <- read.csv(fileName, header=TRUE, sep=,)
  df=x
  ##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
  om <- with(df, (Latitude==0.00000))
  df <-df[!om,]

  ##assign coordinate reference
  coordinates(df)=c("Longitude", "Latitude")
  proj4string(df)<-CRS('+proj=longlat +datum=WGS84')

  dates <- sort(unique(gsub(" ..:..:..", "\\1", df$dt)))
  ##day dates
  ##date1 stores all times from 7-9
  ##date2 stores all times from 10-17
  ##x stores rows that match date1, then
  ##rows that match date2 are added to it.
  for (date in dates){
    day.sub<- subset(df, format(df$dt, '%H')%in%c("08", paste(seq(10,17,2))))
  
  ##change length for >i to adjust number of
  ##points needed to build MCP
    if (length(x)>0) {
      ##generate MCP as .shp
      my.mcp=mcp(day.sub)
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Temp",
               layer=paste(df$BandNmbr[1], "MCP", date, "day", sep="_"), driver="ESRI Shapefile")
    }
  }     
  #Night dates
  for (date in dates){
    night.sub<- subset(df, format(df$dt, '%H')%in%c('19', '21', '23', paste("0", seq(1,6,2), sep="")))
    if (length(night.sub)>0){
      ##generate MCP as .shp
      my.mcp=mcp(night.sub)
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Temp",
               layer=paste(df$BandNmbr[1], "MCP", date, "night", sep="_"), driver="ESRI Shapefile")
    }
  }
}






