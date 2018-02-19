library("Kmisc")
library(RMark)

#.libPaths()
##formatting files for RMark


#setwd("C:/Users/Lisa/Documents/Woodcock/Thesis/Survival")
#list.files()

##choose all_seasons_ch_covars.csv
#df = read.csv(file.choose(), header=TRUE)
df = read.csv("C:/Users/Lisa/Documents/Woodcock/Thesis/Survival/all_seasons_ch_covars.csv")
df2 = df[,c(1, 17:24)]
df = df[,2:(ncol(df)-8)]


##making 00 show up

cols.num <- c(1:ncol(df))
df[1:ncol(df)] <- dapply(df[cols.num],as.character)

df[1:ncol(df)] <- dapply(df[1:15], function(x){sprintf("%02s", x)})
df[1:ncol(df)] <- dapply(df[1:15], function(x){sub(" 0", "00", x, fixed=TRUE)})

##need to make df with capture history as chr, all else as numeric
ch = NULL

for (i in 1:nrow(df)){
  ch=c(ch, paste(df[i,], collapse = ''))
  }
##capture histories stored in ch now

#df2 = read.csv("all_seasons_ch_covars.csv", header=TRUE)
#df2 = df2[,c(1, 17:24)]

############################################################
##didn't do for this analysis
#test1 = dapply(df2[0:ncol(df2)], function(x){as.numeric(factor(x))-1}) ##-1 because R isn't zero indexed
#test1 = as.data.frame(test1)
#############################################################

woodcock = cbind(ch, df2)
names(woodcock)[8]<- "w.age"

woodcock$ch = as.character(woodcock$ch)

##adding in weights for NAs
##one NA is male, other female
##dataset contains 74 males and 54 females
woodcock$weight
woodcock[7,]
woodcock[19,]
woodcock[106,]


x=woodcock[which(woodcock$sex== "M"),]
y=woodcock[which(woodcock$sex=="F"),]

##average weight for males and females
mal = as.integer(mean(x$weight, na.rm=TRUE))
fem = as.integer(mean(y$weight, na.rm=TRUE))

woodcock$weight[7] = fem
woodcock$weight[19] = mal
woodcock$weight[106] = fem


##tag info
woodcock$Tag = as.numeric(woodcock$tag)-1  ##0 is GPS, 1 is VHF
woodcock$w.age = as.numeric(woodcock$w.age)-1
  




