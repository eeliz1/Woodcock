
install.packages("dplyr")
install.packages("knitr", "devtools", "DT")


library(dplyr)

##From Report.Rmd; pulls columns and creates new data frames
##with combinations of those columns
Band_Nmbr = veg_table$BandNmbr
daub = data.frame(veg_table$Litter, veg_table$Herbac, veg_table$Shrub, veg_table$Tree,
                  veg_table$Bare_ground)
SamplePos = veg_table$SamplePos
d = veg_table$Density
cc= veg_table$Canopy
ba = veg_table$Basal_Area
Date = veg_table$Date_utilized

daub = rename(daub, c("veg_table.Litter"="Litter", 
                      "veg_table.Herbac"="Herbac", 
                      "veg_table.Shrub"="Shrub",
                      "veg_table.Tree"="Tree",
                      "veg_table.Bare_ground"="Bare ground"
))


Daub_table=data.frame(Band_Nmbr, SamplePos, Date, daub)


###############################
##Generates MCPs for each file
###############################

for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, sep=,)
  df=x
  ##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
##parse out days??
for (df$dt){
  if(i>paste(grep("-*-*", " "), 12:00:00, "CST", sep=""))
    & (i<paste(grep("-*-*"," ", df), 23:59)){
      print(paste(fileName,i,"day"))
    }
  else{
    print(paste(fileName, i,"night"))
  }
  ##assign coordinate reference
  coordinates(df)=c("Longitude", "Latitude")
  proj4string(df)<-CRS('+proj=longlat +datum=WGS84')
  ##generate MCP as .shp
  my.mcp=mcp(df)
  writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/Temp",
           layer=paste(df$BandNmbr, "MCP", "yay", sep="_"), driver="ESRI Shapefile")
}
}

