install.packages("ggplot2")
install.packages("yaml")
install.packages("colorspace")
install.packages("ggmap")

library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(installr)
updateR()

bd=read.csv("C:/Users/Elisa/Documents/Woodcock/Banding_2016_CSV.csv", header=TRUE)
head(bd)

str(bd)

bd = bd[which(bd$Band.Number != 171368513),]  ##removing a lost band


bd$Date.1=strptime(paste(as.character(bd$Date)), "%m/%d/%Y") ##creating date object

qplot(Sex, Weight, data=bd, color=Age, size=I(4))

qplot(bd$Age)
qplot(bd$Sex)
qplot(Weight,data=bd, fill=Sex)


bd.1 = bd[which(bd$Sex == 'M'),]
bd.1 = bd.1[which(bd.1$Weight > 0),]
bd.1

qplot(Date.1, data=bd, fill=Sex, geom=('histogram'))


bd$Status

qplot(Status, data=bd, fill=Sex, main="Birds Captured", ylab="Number of Birds")

nrow(bd[which(bd$Status=="G"),])
 

ma=read.csv("C:/Users/Elisa/Documents/Woodcock/Data/MCP_area_2.csv", header=TRUE)
head(ma)
nrow(ma)

ma = ma[which(ma$Area..ha != "N/A"),]

ma$meters=as.numeric(ma$Area)*10000

for (i in 1:nrow(ma)){
  for (j in 1:nrow(bd)){
    if (ma$Band..[i] == bd$Band.Number[j]){
      ma$Sex[i] = as.character(bd$Sex[j])
      ma$Age[i] = as.character(bd$Age[j])
      ma$Weight[i] = as.character(bd$Weight[j])
    }
  }
}

ma$X <- NULL


qplot(ma$Sex, ma$meters)

head(ma)
typeof(as.numeric(ma$Area..ha.))

transform(ma, Area..ha. = as.numeric(Area..ha.))

ggplot(ma, aes(ma$Sex, as.numeric(as.character(x=Area..ha.))))+geom_boxplot()




#####################
##random summarizing
##2015-2016 banding
####################
bd$Date.1


bd_Oct= subset(bd, format(Date.1, '%m')=='10')
bd_Nov= subset(bd, format(Date.1, '%m') == '11')
bd_Dec= subset(bd, format(Date.1, '%m') == '12')
bd_Jan= subset(bd, format(Date.1, '%m') == '01')

cat(paste("\n","Nov:", nrow(bd_Nov),"\n", "Dec:", nrow(bd_Dec),"\n", "Jan:", nrow(bd_Jan)))


###########################
##trying to plot with ggmap
##take 1
###########################

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GPS/DERI_F_171368611_44009_149.180/PinPoint_171368611.csv",
              header=TRUE, skip=4)
head(df)
df$Latitude=as.numeric(as.character(df$Latitude))
df$Longitude=as.numeric(as.character(df$Longitude))
##Create datetime column 
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
#df$time=as.POSIXlt(as.numeric(strftime(df$gmt, format="%H:%M:%S")))
##df$DN = if(df$time < format(x,'%H')%in% c(1:12)){dft = df$time}

##day
s1 <- subset(df, format(df$gmt, '%H') %in% c(paste(0, 1:9, sep=""), 10:12))
##night
s2 <- subset(df, format(df$gmt, '%H') %in% c(13:24, '00'))

s1$DN = paste("Day")
s2$DN = paste("Night")

dfp = rbind(s1, s2)

##check to make sure all rows present
##nrow(dfp)
##nrow(df)


##omit non-fixes
om <- with(dfp, (Latitude==0.00000))
dfp <-dfp[!om,]

dfb = dfp

################################################################
coordinates(dfp)=c("Longitude", "Latitude")
proj4string(dfp)<-CRS('+proj=longlat +datum=WGS84')
my.mcp=mcp(dfp)
plot(my.mcp)
typeof(my.mcp)

coords = cbind(dfp$Latitude, dfp$Longitude)
sp = SpatialPoints(coords)
plot(sp)

##can feed to location to give boundary box
##won't work with google, get median value
box1=bbox(sp)
box1

head(sp)
df$Latitude
Lat = as.numeric(sp$coords.x1)
Lon = as.numeric(sp$coords.x2)

min(Lat)
min(dfb$Latitude)
max(Lat)
median(Lat)

base =  get_map(location=c(median(Lon), median(Lat)), zoom=14, maptype="satellite", source="google")#location=c(-93.374588, 30.822073)
ggmap(base)
################################################################


#general plot, no background
ggplot()+geom_point(data=dfb, aes(Longitude, Latitude, color=DN))

 
qmap('DeRidder')
base =  get_map(location=c(-93.378, 30.82308), zoom=14, maptype="satellite", source="google")#location=c(-93.374588, 30.822073)
#ggmap(base)
ggmap(base)+geom_point(data=dfb, aes(Lon, Lat, color=DN), size=0.8)
#location=c(-120,33,-112.5,38.5), zoom=7, maptype="terrain-background")
#'1118 Par Road 998, DeRidder, LA 70634'
#make_bbox(lon = sisquoc$lon, lat = sisquoc$lat, f = .1)
