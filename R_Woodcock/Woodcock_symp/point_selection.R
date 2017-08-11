##only working with one bird here
df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/season1/171368602_pinpoint.csv",
              header=TRUE)

##omit non-fixes
om <- with(df, (Latitude==0.00000))
df <-df[!om,]
##add date-times
df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
df$dt=df$gmt-(60*60*6)
dfp = df
##project
coordinates(df)=c("Longitude", "Latitude")
proj4string(df)<-CRS('+proj=longlat +datum=WGS84')

##need to select random point from each day between 1/8 and 1/19
##isolating day points
df <- subset(df, format(dfp$gmt, '%H') %in% c(13:24, '00'))
##Sequence to iterate through (days for the dates)
seq = 8:19

for(i in seq){
sub.1 <- subset(df, format(df$dt,'%d')==paste(0, i, sep=""))
sub.2 <-subset(df, format (df$dt, '%d')==i)

if(nrow(sub.1)>0){
  l1 = nrow(sub.1)
  samp1 = sample(1:l1, 3)
  pts1 = sub.1[samp1,]
  writeOGR(pts1, "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/points_selected",
           layer=paste(substr(pts1$dt[1], 1, 10)), driver="ESRI Shapefile")
  }

if(nrow(sub.2)>0){
  l2 = nrow(sub.2)
  samp2 = sample(1:l2, 3)
  pts2 = sub.2[samp2,]
  writeOGR(pts2, "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/points_selected",
           layer=paste(substr(pts2$dt[1], 1, 10)), driver="ESRI Shapefile")
  }

}


##Select a random number
l = 10
p = sample(1:l, 6)
p
