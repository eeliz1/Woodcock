

w = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/R_output/weather.csv", header=T)
df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/R_output/all_pins_toField.csv", header=T)

##preparing dataframes
w$Time = as.POSIXct(w$Time)
names(w)[12] = "site"
w = w[,c(2, 4:12)]
w = w[order(w$Time),]

df = df[,c(3:8, 14:18)]
df$dt = as.POSIXct(df$dt)
df = df[order(df$dt),]
df = df[order(df$site),]

#reset = w
sites = c("BOYC", "DERI", "SHER","YANC", "TENS", "BAPI")

time_df<-NULL
#w= reset
for(i in 1:length(sites)){
  #browser()
  ##get relevant dataframes
  w1 = w[which(as.character(w$site)==sites[i]),]
  df_temp = df[which(df$site==sites[i]),]
  
  for(j in 1:nrow(df_temp)){
    t = df_temp$dt[j]
    w_temp = w1[which(w1$Time>t),]
    time_df = rbind(time_df, w_temp[1,])
  }
}

weather_append = time_df[order(time_df$site),]

with_covars = cbind(df, weather_append)


####add in moon phase
#test = unique(format(full_covars$dt, "%m/%Y"))

moon = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/moon_phase_raw.csv", header=T)
head(moon)

moon=moon[,c(1:2)]
moon$date = as.Date(moon$Date, "%m/%d/%Y")
with_covars$date = as.Date(substr(with_covars$dt, 1, 10), "%Y-%m-%d")

head(with_covars)

full_covars = merge(moon, with_covars, by="date")
full_covars = full_covars[,c(4:24, 3)]


#for(i in 1:nrow(test)){
#if(test$site[i]==test$site.1[i]){
#  x = 1
#} else{
#  print("FALSE")
#}
#}

write.csv(full_covars, "C:/Users/Elisa/Documents/Woodcock/Data/Field/all_pts_wcovars.csv")
##see "test" in downloads for points to correct


#x = full_covars[which(full_covars$toField==1),]
#View(x)
#table(x$Illumination)
