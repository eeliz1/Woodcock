library(weatherData)

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/R_output/all_pins_toField.csv", header=T)

#df[,c(1, 3, 8, 9, 10, 11, 12, 13)]<- NULL
df$site = as.factor(df$site)
df$dt = as.POSIXct(df$dt)

sites = c("BOYC", "YANC", "DERI", "SHER", "TENS", "BAPI")
ids = c("KLAGARDN2", "KLAMARKS5", "KLADERID14", "KLAFORDO2",
        "KLANEWEL3", "KLASHREV11")
cols = c(1, 2, 3, 7, 8, 9, 10, 12, 13)


weather = NULL
for(i in 1:length(sites)){
  site = sites[i]
  temp = df[which(df$site==site),]
  idt = ids[i]
  
  start = substr(min(temp$dt), 1, 10)
  end = substr(max(temp$dt), 1, 10)
  
  weather_temp = getWeatherForDate(station_id = idt, start_date = start, end_date = end,
                              station_type = "id", opt_detailed = TRUE, opt_custom_columns = TRUE,
                              custom_columns = cols)

  x = rep(site, nrow(weather_temp))
  weather_temp2 = cbind(weather_temp, x)
  
  weather = rbind(weather, weather_temp2)
}


write.csv(weather, "C:/Users/Elisa/Documents/Woodcock/Data/R_output/weather.csv")

