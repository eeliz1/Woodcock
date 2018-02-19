###################################
##Generate database of location 
##switches between field/forest
###################################


all_pins = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GPS/Pinpoint/allpins_fields.csv", header=T)

inds=unique(all_pins$BandNmbr)

switch=NULL
for (i in inds){
  md <- all_pins[which(all_pins$BandNmbr==i),]
  
  ##create POSIX object
  md$gmt=strptime(paste(md$GMT_Time),"%m/%d/%Y %H:%M")
  md$dt=md$gmt-(60*60*6)
  md=md[order(md$dt),]
  
  ##from Woodcock move
  xy=cbind(md$POINT_X, md$POINT_Y)
  
  spdf=SpatialPointsDataFrame(xy, md, proj4string = CRS("+proj=longlat +datum=WGS84"))
  #plot(spdf)
  spdf1=spdf[-nrow(spdf),]
  spdf2=spdf[-1,]
  md=md[-nrow(md),]
  md$spdfdist=as.numeric(distVincentyEllipsoid(spdf1, spdf2))
  
  ##get field/forest shifts
  md = md[order(md$dt),]
  x= rle(md$Field)
  x2=x[1]
  
  store=NULL
  z=0
  for(j in 1:length(x2$lengths)){
    temp= x2$lengths[j]
    z=z+temp
    store=c(store, z)
  }
  
  ##md[store,] <--- the skip points
  skips = md[store,]
  skips = skips[1:(nrow(skips)),]  ##skips-1
  #md[order(md$dt),]
  
  switch = rbind(switch, skips)
  ##set time
  #dfp = subset(md, format (md$dt, '%H') %in% hrs)
  
}

