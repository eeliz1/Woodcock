library(ggplot2)


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
bd.1 = bd.1[which(bd.1$Weight > 170),]
bd.1

qplot(Date.1, data=bd, fill=Sex, geom=('histogram'))


bd$Status

qplot(Status, data=bd, fill=Sex, main="Birds Captured", ylab="Number of Birds")
 

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


