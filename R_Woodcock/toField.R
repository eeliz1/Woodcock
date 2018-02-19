
##need to get 0 and 1s for travel to field

all_pins = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/all_pins_fields_MOVE.csv", header=T)
head(all_pins)
switch_field = read.csv(file="C:/Users/Elisa/Documents/Woodcock/Data/R_output/switch_field.csv", header=T)

all_pins$unq = paste(all_pins$BandNmbr, all_pins$gmt)
switch_field$unq = paste(switch_field$BandNmbr, switch_field$gmt)

all_pins$toField = match(all_pins$unq, switch_field$unq)

for (i in 1:nrow(all_pins)){
  if(is.na(all_pins$toField[i]==TRUE)){
    all_pins$toField[i] = 0
  } else (all_pins$toField[i]=1)
}

write.csv(all_pins, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/all_pins_toField.csv")



























#all_pins = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/allpins_fields_CORRECTED.csv", header=T)
#inds=unique(all_pins$BandNmbr)

#all = NULL
#for (i in inds){
#  md <- all_pins[which(all_pins$BandNmbr==i),]

#  ##create POSIX object
#  md$gmt=strptime(paste(md$gmt),"%m/%d/%Y %H:%M")
#  md$dt=md$gmt-(60*60*6)
#  md=md[order(md$dt),]
#  
#  ##from Woodcock move
#  xy=cbind(md$POINT_X, md$POINT_Y)
#  
#  spdf=SpatialPointsDataFrame(xy, md, proj4string = CRS("+proj=longlat +datum=WGS84"))
#  #plot(spdf)
#  spdf1=spdf[-nrow(spdf),]
#  spdf2=spdf[-1,]
#  md=md[-nrow(md),]
#  md$spdfdist=as.numeric(distVincentyEllipsoid(spdf1, spdf2))
#  
#  all = rbind(all, md)
#  
#}

#write.csv(all, file="C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/all_pins_fields_MOVE.csv")



