library(rgdal)


getwd()
Filenames <- Sys.glob("*.csv")

bd=read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=TRUE)


for(i in Filenames){
  df = read.csv(i, skip=4, header=T)
  
  ##add in band number
  df$BandNmbr= substr(i, 1, 9)
  
  ##add in site
  bd1 = bd[which(bd$BandNmbr == df$BandNmbr[1]),]
  df$site= bd1$site[1]
  
  df$Latitude=as.numeric(as.character(df$Latitude))
  df$Longitude=as.numeric(as.character(df$Longitude))
  ##Create datetime column 
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  
  ##night
  s1 <- subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
  ##day
  s2 <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
  s1$DN = paste("Day")
  s2$DN = paste("Night")
  
  dfp = rbind(s1, s2)
  
  ##omit non-fixes
  om <- with(dfp, (Latitude==0.00000))
  dfp <-dfp[!om,]
  
  ##reorder columns in alphabetical order
  dfp = dfp[order(colnames(dfp))]

  ##create spatial dataframe  
  coordinates(dfp)=c("Longitude", "Latitude")
  proj4string(dfp)<-CRS('+proj=longlat +datum=WGS84')
  
  writeOGR(dfp, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/GIS",
            layer=(paste(dfp$BandNmbr[1], dfp$site[1],sep="_")), driver="ESRI Shapefile")
}












####
switch_field2 = switch[which(switch$Field==0),]

###
switch_forest2 = switch[which(switch$Field==1),]

coordinates(switch_field2)=c("POINT_X", "POINT_Y")
proj4string(switch_field2)<-CRS('+proj=longlat +datum=WGS84')

writeOGR(switch_field2, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/GIS",
         layer="switch_field", driver="ESRI Shapefile")


coordinates(switch_forest2)=c("POINT_X", "POINT_Y")
proj4string(switch_forest2)<-CRS('+proj=longlat +datum=WGS84')

writeOGR(switch_forest2, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/GIS",
         layer="switch_forest", driver="ESRI Shapefile")