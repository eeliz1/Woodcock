

##adding covariates to capture histories

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Survival/season1_ch.csv", header=T)

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Survival/season2_ch.csv", header=T)

bd = read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=T)

##conversions to facilitate adding data from one df to another
bd$site=as.character(bd$site)
bd$sex=as.character(bd$sex)
bd$status=as.character(bd$status)
bd$age=as.character(bd$age)

for (i in 1:nrow(df)){
  for (j in 1:nrow(bd))
  if(df$Band.Number[i] == as.character(bd$BandNmbr[j])){
    df$site[i] = bd$site[j]
    df$sex[i] = bd$sex[j]
    df$tag[i]=bd$status[j]
    df$age[i]=bd$age[j]
  }
}



write.csv(df, "C:/Users/Elisa/Documents/Woodcock/Survival/season2_ch_covars.csv")



##adding in capture dates 

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Thesis/Survival/seasons_combined.csv")
#bd = read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=T) #if not read in above

bd$dt = strptime(paste(bd$date), '%m/%d/%Y')

for (i in 1:nrow(df)){
  for(j in 1:nrow(bd)){
  if(df$Band.Number[i]==bd$BandNmbr[j]){
    df$cd[i] = as.character(bd$dt[j])
  }
  }
}

write.csv(df, "C:/Users/Elisa/Documents/Woodcock/Thesis/Survival/all_seasons_ch_covars.csv")

