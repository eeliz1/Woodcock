#install.packages("weatherData")
library(weatherData)

###########################################
##Checking weather availability
###########################################

head(switch_field)

df = switch_field
df$site = as.factor(df$site)
df$gmt = as.POSIXct(df$gmt, tz="GMT")

dfB = df[which(df$site=="BOYC"),]
dfD = df[which(df$site=="DERI"),]
dfY = df[which(df$site=="YANC"),]
dfS = df[which(df$site=="SHER"),]
dfT = df[which(df$site=="TENS"),]
dfBP = df[which(df$site=="BAPI"),]
#temp = all_pins[which(all_pins$BandNmbr == 171368648),] ##171368648, 157374414, 157382985

##BOYC- data is available
start = substr(min(dfB$gmt), 1, 10)
end = substr(max(dfB$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLAGARDN2", start, end, station_type="id")


##DERI- data is available
start = substr(min(dfD$gmt), 1, 10)
end = substr(max(dfD$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLADERID14", start, end, station_type="id")


##YANC- data is available 
start = substr(min(dfY$gmt), 1, 10)
end = substr(max(dfY$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLAMARKS5", start, end, station_type="id")


##SHER- data is available
start = substr(min(dfS$gmt), 1, 10)
end = substr(max(dfS$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLAFORDO2", start, end, station_type="id")


##TENS- data is available
start = substr(min(dfT$gmt), 1, 10)
end = substr(max(dfT$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLANEWEL3", start, end, station_type="id")


##BAPI- data is availabe 
start = substr(min(dfBP$gmt), 1, 10)
end = substr(max(dfBP$gmt), 1, 10)

checkDataAvailabilityForDateRange("KLASHREV11", start, end, station_type="id")


list = c("Time", "Temperature", "WindDirectionDegrees","WindSpeedMPH", "WindSpeedGustMPH", "HourlyPrecipIn","Clouds", "dailyrainin", "DateUTC")


weather_temp = getWeatherForDate(station_id = "KLASHREV11", start_date = start, end_date = end,
                                 station_type = "id", opt_detailed = TRUE, opt_custom_columns = TRUE,
                                 custom_columns = list)

