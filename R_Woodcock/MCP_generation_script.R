library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)

############################################################
#######Takes pinpoint files and generates MCP (.shp) for each
#######day and night of each bandnumber
############################################################

##set working directory to folder with pinpoint files
setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/all")


##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")

for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, skip=4)
  df=x
  df$BandNmbr = substr(fileName,1, 9)
  ##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
  om <- with(df, (Latitude==0.00000))
  df <-df[!om,]

  ##assign coordinate reference
  coordinates(df)=c("Longitude", "Latitude")
  proj4string(df)<-CRS('+proj=longlat +datum=WGS84')

  #Find all unique dates in datum
  dates <- sort(unique(gsub(" ..:..:..", "\\1", df$dt)))
  ##day dates
  ##date1 stores all times from 7-9
  ##date2 stores all times from 10-17
  ##x stores rows that match date1, then
  ##rows that match date2 are added to it.
  for (date in dates){
    date1 <- paste(date,"0[7-9]")
    date2 <- paste(date,"1[0-7]")
    x <- grep(date1, df$dt)
    x <- c(x, grep(date2, df$dt))
    ##change length for >i to adjust number of
    ##points needed to build MCP
    if (length(x)>0) {
      ##generate MCP as .shp
      my.mcp=mcp(df[x,])
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/Day_MCPs",
               layer=paste(df$BandNmbr[1], "MCP", date, "day", sep="_"), driver="ESRI Shapefile")
    }
  }     
  #Night dates
  for (date in dates){
    date1 <- paste(date, "19")
    date2 <- paste(date, "2[0-3]")
    ##increments date by 1 to continue drawing
    ##points from that night
    date3 <- paste(as.character(as.Date(date)+1), "0[0-6]")
    x <- grep(date1, df$dt)
    x <- c(x, grep(date2, df$dt))
    x <- c(x, grep(date3, df$dt))
    if (length(x)>0){
      ##generate MCP as .shp
      my.mcp=mcp(df[x,])
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/MCPs_band/",
               layer=paste(df$BandNmbr[1], "MCP", date, "night", sep="_"), driver="ESRI Shapefile")
    }
  }
}