###########################
##trying to plot with ggmap
##DERI
###########################
library(plyr)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)

setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/DERI")

##takes all files with ".csv" ending and stores to obj
fileNames <- Sys.glob("*.csv")
fileNames

comb.files=ldply(fileNames, read.csv)

head(comb.files)
nrow(comb.files)

df = comb.files

df$Latitude=as.numeric(as.character(df$Latitude))
df$Longitude=as.numeric(as.character(df$Longitude))
##Create datetime column 
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
df$time=as.POSIXlt(as.numeric(strftime(df$gmt, format="%H:%M:%S")))
##df$DN = if(df$time < format(x,'%H')%in% c(1:12)){dft = df$time}

##day
s1 <- subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
##night
s2 <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))
s1$DN = paste("Day")
s2$DN = paste("Night")

dfp = rbind(s1, s2)


##omit non-fixes
om <- with(dfp, (Latitude==0.00000))
dfp <-dfp[!om,]

nrow(dfp)
#general plot, no background
ggplot()+geom_point(data=dfp, aes(Longitude, Latitude, color=DN))


base=get_map(location=c(-93.378, 30.828), zoom=14, maptype="satellite", source="google")#location=c(-93.374588, 30.822073)
#ggmap(base)
ggmap(base, extent='device')+geom_point(data=dfp, aes(Longitude, Latitude, color=DN), size=1)+
  labs(title="January 2016 Bird Locations", x="Longitude", y="Latitude")+
  scale_colour_discrete(name=NULL, labels=c("Day","Night"))

ggsave(file=paste("C:/Users/Elisa/Pictures/Graphs/", "Map", "DERI", ".png", sep=""))



