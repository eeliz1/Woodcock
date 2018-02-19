
df = read.csv("C:/Users/Lisa/Documents/Woodcock/Data/Field/dusk_pts_wcovars.csv", header=TRUE)
head(df)


######################################################
##days since travel to field
######################################################
df = df[order(df$BandNmbr),] ##order rows by band number (also should be in date order)

unq = unique(df$BandNmbr)

dff = NULL

for(i in 1:length(unq)){
  df_temp = df[which(df$BandNmbr==unq[i]),]
  df_temp$dsf = NA
  
  count=1
  df_temp$dsf[1] = 1 ##first day is one day since field since trapped night before
  if(df_temp$toField[1]==0){count=2}
  
  if(nrow(df_temp)>1){
  for(j in 2:(nrow(df_temp))){
    #browser()
    if(df_temp$toField[j] == 1){
      df_temp$dsf[j] = count
      count=1
    } else{
      df_temp$dsf[j] = count
      count = count+1
    }
  }
  }
  dff = rbind(dff, df_temp)
}

dff = dff[,c(2:6,11, 23, 8:10, 12:22)]

#test = cbind(as.character(df_temp$dt), df_temp$toField, df_temp$dsf)
#View(test)


########################################################
##moon rise
########################################################










########################################################
##field area
########################################################



