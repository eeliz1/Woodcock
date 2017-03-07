install.packages("rgdal")
install.packages("sp")
install.packages("maptools")

library(rgdal)
library(sp)
library(maptools)
library(adehabitatHR)
#install.packages("packagename")

#####################################
##Program to generate MCPs
##8/26/2016- now iterates over files, generates MCP by band number but not by time
##(cont'd) need to parse out each day/night per bird
#####################################

setwd("C:/Users/Elisa/Documents/Woodcock/R_Woodcock/Temp")

fileNames <- Sys.glob("*.csv")
fileNames
df=fileNames[1]

##Generate MCPs
for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, sep=,)
  df=x
##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]

head(df)
print(df$dt)
y = as.list(df$dt)

z = cat(df$dt)


a= array()
x=0
for (i in y){
  i = as.character(i)
  a[x] = strsplit(i, " ")
  x = x+1
}

head(a)
a[3][1]

##parse out based on time
  df1 <- subset(df, gmt==paste(2016-, grep("..-.."), " ", 05:56:00, "CST", sep="")

}

head(df)
head(dftest)

print(df$dt)
print(df$BandNmbr)
for (i in df$dt){
  
  browser()
  print(df$dt[i])
}


dftest$time=strptime(paste(dftest$GMT.Time), "%H:%M")

##testing parsing
head(df)

head(as.character(df$dt))

df1 <- subset(df, dt==

df1

##assign coordinate reference
  coordinates(df1)=c("Longitude", "Latitude")
  proj4string(df1)<-CRS('+proj=longlat +datum=WGS84')
##generate MCP as .shp
  my.mcp=mcp(df1)
  writeOGR(my.mcp, "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/Temp",
        layer=paste(df1$BandNmbr, "MCP", "yay", sep="_"), driver="ESRI Shapefile")






####kept here as a reminder of a flow that works correctly
for(fileName in fileNames){
  x <- read.csv(fileName, header=TRUE, sep=,)
  df=x
  ##create POSIX object
  df$gmt=strptime(paste(df$GMT.Time),"%m/%d/%Y %H:%M")
  df$dt=df$gmt-(60*60*6)
  df=df[order(df$dt),]
  ##parse out days??
     for (i in df$GMT.Time){
        if (i<24 & i>12)
          print("day")

        else
          print(i)
      }
}




###testing regex

z =c("10/27/2016", "9/30/2016", "cat", 2, 3, "m", "l")
cat(z)

##define regex pattern
pattern <- "*/*/2016"
##adds a 1 to each occasion found by the regular expression
paste(grep(pattern, z, ignore.case=FALSE, perl=FALSE, value=TRUE), 1, sep="")

##input syntax for grep
#grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
#     fixed = FALSE, useBytes = FALSE, invert = FALSE)



