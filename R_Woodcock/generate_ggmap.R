###########################
##Generate ggmap for each
##bird
##work in progress
###########################
library(plyr)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)

setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/new")

##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames

##if want to combine files
##for site specific, use which()
#comb.files=ldply(fileNames, read.csv)

#head(comb.files)
#nrow(comb.files)

#df = comb.files

##banding data
bd=read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=TRUE)


for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, skip=4)
  #x <- read.csv("171368632_pinpoint.csv", skip=4)
  df=x
  
  df$BandNmbr = substr(fileName, 1, 9)
  
  bd1 = bd[which(bd$BandNmbr == df$BandNmbr[1]),]
  df$site= bd1$Site[1]
  
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

  ##Get location for base map
  Lon=median(dfp$Longitude)
  Lat=median(dfp$Latitude)

  base=get_map(location=c(Lon, Lat), zoom=14, maptype="satellite", source="google")#location=c(-93.374588, 30.822073)
  ggmap(base)
  ggmap(base, extent='device')+geom_point(data=dfp, aes(Longitude, Latitude, color=DN), size=1)+
    ##labs(title=paste(df$BandNmbr,df$site, "Bird Locations"), x="Longitude", y="Latitude")+
    scale_colour_discrete(name=NULL, labels=c("Day", "Night"))+
    scale_color_manual(expand = c(0, 0), values = c("deepskyblue1", "gold1"))+
    theme(legend.position="none")

    ggsave(file=paste("C:/Users/Elisa/Pictures/Graphs/", "pres3", ".jpeg", sep=""),
           device='jpeg')
    
}

head(df)

ggmap(base, extent='device')+geom_point(data=dfp, aes(Longitude, Latitude, color=gmt), size=1)+
  labs(title=paste(df$BandNmbr,df$site, "Bird Locations"), x="Longitude", y="Latitude")+
  #scale_colour_discrete(name=NULL, labels=c("Day","Night"))+
  #scale_color_manual(expand = c(0, 0), values = c("gold1", "deepskyblue1"))

ggsave(file=paste("C:/Users/Elisa/Pictures/Graphs/", df$BandNmbr,"_", df$site, "_Map", ".tiff", sep=""),
       device='tiff')






#######one file
library(plyr)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)

setwd("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season2/new")

##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames

##if want to combine files
##for site specific, use which()
#comb.files=ldply(fileNames, read.csv)

#head(comb.files)
#nrow(comb.files)

#df = comb.files

##banding data
bd=read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=TRUE)
colnames(bd) = c("index", "BandNmbr", "Datetime", "Site", "Sex", "Age", "Weight", "Wing",
                 "Bill", "Status", "Frequency", "Latitude", "Longitude", "P1", "Comments")

for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, skip=4)
  #x <- read.csv("171368655_pinpoint.csv", skip=4)
  df=x
  
  df$BandNmbr = substr(fileName, 1, 9)
  
  bd1 = bd[which(bd$BandNmbr == df$BandNmbr[1]),]
  df$site= bd1$Site[1]
  
  df$Latitude=as.numeric(as.character(df$Latitude))
  df$Longitude=as.numeric(as.character(df$Longitude))
  ##Create datetime column 
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  
  ##day
  s1 <- subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
  ##night
  s2 <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
  s1$DN = paste("Night")
  s2$DN = paste("Day")
  
  dfp = rbind(s1, s2)
  
  
  ##omit non-fixes
  om <- with(dfp, (Latitude==0.00000))
  dfp <-dfp[!om,]
  
  ##Get location for base map
  Lon=median(dfp$Longitude)
  Lat=median(dfp$Latitude)
  
  base=get_map(location=c(Lon, Lat), zoom=15, maptype="satellite", source="google")#location=c(-93.374588, 30.822073)
  #ggmap(base)
  ggmap(base, extent='device')+geom_point(data=dfp, aes(Longitude, Latitude, color=DN), size=1)+
    labs(title=paste(df$BandNmbr,df$site, "Bird Locations"), x="Longitude", y="Latitude")+
    scale_colour_discrete(name=NULL, labels=c("Day", "Night"))+
    scale_color_manual(expand = c(0, 0), values = c("gold1", "deepskyblue1"))
  
  ggsave(file=paste("C:/Users/Elisa/Pictures/Graphs/", df$BandNmbr,"_", df$site, "_Map", ".tiff", sep=""),
         device='tiff')
  
}

#head(df)

#ggmap(base, extent='device')+geom_point(data=dfp, aes(Longitude, Latitude, color=gmt), size=1)+
#  labs(title=paste(df$BandNmbr,df$site, "Bird Locations"), x="Longitude", y="Latitude")+
  #scale_colour_discrete(name=NULL, labels=c("Day","Night"))+
  #scale_color_manual(expand = c(0, 0), values = c("gold1", "deepskyblue1"))
  
#  ggsave(file=paste("C:/Users/Elisa/Pictures/Graphs/", df$BandNmbr,"_", df$site, "_Map", ".tiff", sep=""),
#         device='tiff')


