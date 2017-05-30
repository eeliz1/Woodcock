

###proofing veg data


df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GIS/veg_attributes.csv",
              head=TRUE)

df = df[,c(5:14, 16, 25)]

comm = df[which(df$Comments!=""),]
comm

head(df)

df$add = df$Litter+df$Herbac+df$Shrub+df$Tree+df$Other+df$bareground
