library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)

############################################################
#######Takes pinpoint files and generates MCP (.shp) for each
#######day and night of each bandnumber
############################################################

##set working directory to folder with pinpoint files
setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/")


##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")




##the times for use point production
##CST
hrs1 = c('07', '08', '09', 10:17)
hrs2 = c(18:24, paste(0, 1:6, sep=""))

bd=read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=TRUE)

fileName=fileNames[3]

for(fileName in fileNames){
  #browser()
  x <- read.csv(fileName, header=TRUE, sep=,, skip=4)
  df=x
  df$BandNmbr = substr(fileName, 1, 9)
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
    if (length(x)>5) {
      ##generate MCP as .shp
      my.mcp=mcp(df[x,])
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/Day_MCPs/new",
               layer=paste(df$BandNmbr[1], "MCP", date, "day", sep="_"), driver="ESRI Shapefile")
      
##############################################################################################################    
      ##use points
      dfp = subset(df, format (df$dt, '%d') %in% date)
      dfp = subset(dfp, format (dfp$dt, '%H') %in% hrs1)

      writeOGR(dfp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/MCP_pts/new",
               layer=paste(dfp$BandNmbr[1], "pts", date, "day", sep="_"), driver="ESRI Shapefile")
##############################################################################################################
      
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
      writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/Night_MCPs/new",
               layer=paste(df$BandNmbr[1], "MCP", date, "night", sep="_"), driver="ESRI Shapefile")
      
      ##use points
      dfp = subset(df, format (df$dt, '%d') %in% date)
      dfp = subset(dfp, format (dfp$dt, '%H') %in% hrs2)
      
      writeOGR(dfp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/MCP_pts/new",
               layer=paste(dfp$BandNmbr[1], "pts", date, "night", sep="_"), driver="ESRI Shapefile")
    }
  }
}


}
