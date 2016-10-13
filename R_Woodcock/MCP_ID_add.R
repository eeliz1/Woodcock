###################################
###MCP vegetation 
###################################
library(plyr)

setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/")

veg_files = Sys.glob("*.csv")
area = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/MCP_area_2.csv")

head(area)
df = read.csv("171368602_Jan08_2016_MCP.csv")
veg_files

##writes each file back out with an MCP_ID 
##column included
for (file in veg_files){
#browser()
  df = read.csv(file)
  df$MCP_ID = paste(substr(file, 0, nchar(file) -4), sep="_")
#  for (i in length(nrow(area))){
#    if (array(area$Day.Night[i]) == "Day"){
#      if (df$Date_utilized[1]==array(area$Date)[i])
#      df$area = area$Area..ha[i]
  write.csv(df, file = paste("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/MCP_veg/",
                             df$MCP_ID[1], ".csv", sep=""))
  }

area$Day.Night[5]

setwd("C:/Users/Elisa/Documents/Woodcock_Git/R_Woodcock/Veg_csv/MCP_veg")

MCP_veg_files = Sys.glob("*.csv")
MCP_veg_files
MCP_comb_file=ldply(MCP_veg_files, read.csv)

head(MCP_comb_file)

tail(MCP_comb_file)

r###################
##testing ggplot
g = MCP_comb_file
head(g)
g$SampPos <- NULL


qplot(Basal_Area, data=g, 
      fill=SamplePos,
      geom="histogram")

####################
MCF = subset(MCP_comb_file, SamplePos=="MCP")
MCF
MCPs <- sort(unique(gsub(".........._.....", "\\1", MCP_comb_file$MCP_ID)))
MCPs







y = array()


for (file in veg_files){
  browser()
  df = read.csv(file)
  df$MCP_ID = paste(substr(file, 0, nchar(file) -4), sep="_")
  for (i in length(nrow(area))){
    if (df$Date_utilized[1]==array(area$Date)[i])
      df$area = array(area$Date)[i]
      print("True")
  }
}