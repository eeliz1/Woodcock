library(chron)



df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/Field/all_pts_wcovars.csv", header=TRUE)
df$dt = strptime(df$dt, "%m/%d/%Y %H:%M")
df$gmt = strptime(df$gmt, "%m/%d/%Y %H:%M")
df$hour = chron(times=paste(as.character(df$hour), ":00", sep=""))

##get a column with each unique bird-day
df = subset(df, format(df$dt, '%H') %in% c(paste(16:20)))
df$unq = substr(paste(df$BandNmbr, df$dt), 1, 20)

##list of unique bird-days
unq = unique(df$unq)

##storage df
df4 = NULL

for (i in 1:length(unq)){
  #browser()
  ##df2 will store value to be added to final df (df4)
  df2 = NULL
  
  ##set bird-day
  unq_temp = unq[i]
  
  ##subset complete data (df) to one bird-day
  df_temp = df[which(unq_temp==df$unq),]
  
  ##get column (x) with T/F for moving to field
  x = df_temp$toField %in% 1
  df_temp = cbind(df_temp, x)
  
  ##if there is a move to field, add that record to df2
  for(j in 1:nrow(df_temp)){
    if(df_temp$x[j]==TRUE){
      df2 = rbind(df_temp[j,])
    } 
  }
  
  ##if not, add the earliest record to df2
  if(length(df2[,1])<1){
    df2 = rbind(df_temp[1,])
  }
  df4 = rbind(df4, df2)
}


str(df4)

df3 = df4[,c(1, 4:ncol(df4)-1)]


write.csv(df3, "C:/Users/Elisa/Documents/Woodcock/Data/Field/dusk_pts_wcovars.csv")









