library("Kmisc")
library(RMark)

##formatting files for RMark


setwd("C:/Users/Elisa/Documents/Woodcock/Thesis/Survival")
list.files()

df = read.csv("all_seasons_ch_covars.csv", header=TRUE)
df = df[,2:(ncol(df)-7)]

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

df2 = read.csv("all_seasons_ch_covars.csv", header=TRUE)
df2 = df2[,c(1, 17:24)]

############################################################
##didn't perform for first run analysis
test1 = dapply(df2[0:ncol(df2)], function(x){as.numeric(factor(x))-1}) ##-1 because R isn't zero indexed
test1 = as.data.frame(test1)
#############################################################

woodcock = cbind(ch, df2)
names(woodcock)[8]<- "w.age"

woodcock$ch = as.character(woodcock$ch)








