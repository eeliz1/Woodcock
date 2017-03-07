library(rgdal)

####################
##argos data
####################

df1 = read.delim("C:/Users/Elisa/Downloads/020217 Argos Data DS_Con_166375_170202141445.txt", sep=",",header=FALSE, skip=2)
df2 = read.delim("C:/Users/Elisa/Downloads/020217 Argos Data DS_Con_166376_170202141445.txt", sep=",",header=FALSE, skip=2)
df3 = read.delim("C:/Users/Elisa/Downloads/DS (1)_Con_166377_170111122520.txt", sep=",",header=FALSE, skip=2)
df4 = read.delim("C:/Users/Elisa/Downloads/DS (1)_Con_166374_170111122520.txt", sep=",",header=FALSE, skip=2)

##success only

df3 = df3[1:21,]
df3$Longitude = as.numeric(substr(as.character(df3$V5), 1, 11))
df3$Latitude = as.numeric(substr(as.character(df3$V4), 1, 10))

##Latitude = V21
##Longitude=V24


coordinates(df3)=c("Longitude", "Latitude")
proj4string(df3)<-CRS('+proj=longlat +datum=WGS84')

plot(df3)

writeOGR(df3, "C:/Users/Elisa/Documents/Woodcock/Data/R_output",layer="argos_3", driver="ESRI Shapefile")
str(df1)

plot(df1)
