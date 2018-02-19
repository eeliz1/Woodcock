library(chron)

df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/Field/all_pts_wcovars.csv", header=T)
bd = read.csv("C:/Users/Elisa/Documents/Woodcock/Banding.csv", header=TRUE)

df$dt = strptime(df$dt, "%m/%d/%Y %H:%M")
df$date = as.Date(substr(df$dt, 1, 10))
df$hour = chron(times=paste(as.character(df$hour), ":00", sep=""))
df$BandNmbr = as.factor(df$BandNmbr)
##
#df$TemperatureF[which(df$TemperatureF==-999.9)] <- NA
#df$WindSpeedMPH[which(df$WindSpeedMPH==-999.9)] <- NA
#df$WindSpeedGustMPH[which(df$WindSpeedGustMPH==-999.9)] <- NA
#df$Humidity[which(df$Humidity==-999.9)] <- NA
##
str(df)

#df = df[which(df$TemperatureF!=-999.9),]
df$TemperatureF[which(df$TemperatureF==-999.99)] <- NA
df$WindSpeedMPH[which(df$TemperatureF==-999.99)] <- NA
df$Humidity[which(df$TemperatureF==-999.99)] <- NA
df$WindSpeedGustMPH[which(df$TemperatureF==-999.99)] <- NA

##add in age, sex, habitat type
for (i in 1:nrow(df)){
  m = df$BandNmbr[i]
  for (j in 1:nrow(bd)){
    if (m == bd$Band.Number[j]){
      df$age[i] = as.character(bd$Age[j])
      df$sex[i] = as.character(bd$Sex[j])
    }
  }
  
  if (df$site[i]=="BOYC" | df$site[i]=="DERI"){
    df$type[i]= "pine"
  } else{df$type[i]= "bothar"}
}

df$age = as.factor(df$age)
df$sex = as.factor(df$sex)
df$type = as.factor(df$type)


##create models
fit1 <- glm(df$spdfdist~1, family=gaussian)
fit2 <- glm(df$spdfdist~df$date, family=gaussian)
fit3 <- glm(df$spdfdist~df$hour, family=gaussian)
fit4 <- glm(df$spdfdist~df$toField, family=gaussian)
fit5 <- glm(df$spdfdist~df$TemperatureF, family=gaussian)
fit6 <- glm(df$spdfdist~df$WindSpeedMPH, family=gaussian)
fit7 <- glm(df$spdfdist~df$site, family=gaussian)
fit8 <- glm(df$spdfdist~df$dailyrainin, family=gaussian)
fit9 <- glm(df$spdfdist~df$HourlyPrecipIn, family=gaussian)
fit10 <- glm(df$spdfdist~df$Field, family=gaussian)
fit11 <- glm(df$spdfdist~df$BandNmbr, family=gaussian)
fit12 <- glm(df$spdfdist~df$age, family=gaussian)
fit13 <- glm(df$spdfdist~df$sex, family=gaussian)
fit14 <- glm(df$spdfdist~df$type, family=gaussian)

models = c("null", "date", "hour", "toField", "Temp", "WindSpeed", "site", "DailyRain",
           "HourlyPrecip", "Field", "BandNmbr", "age", "sex", "type")


K = c(length(coef(fit1)), length(coef(fit2)), length(coef(fit3)), length(coef(fit4)),
  length(coef(fit5)), length(coef(fit6)), length(coef(fit7)), length(coef(fit8)), 
  length(coef(fit9)), length(coef(fit10)), length(coef(fit11)), length(coef(fit12)),
  length(coef(fit13)), length(coef(fit14)))
  
n = nrow(df)
LL = c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4), logLik(fit5),
        logLik(fit6), logLik(fit7), logLik(fit8), logLik(fit9), logLik(fit10), logLik(fit11),
       logLik(fit12), logLik(fit13), logLik(fit14))
AICc =-2*LL + 2*K*(n/(n-K-1))

wc.res = data.frame(model = models,
                    LogLik = LL,
                    Rank = rank(AICc),
                    AICc)

wc.res = wc.res[order(wc.res$Rank),]
wc.res

reset = df
df= reset
########################################################################
##Rerun for night only
########################################################################
df = subset(df, format(df$dt, '%H') %in% c(18:23, paste(0, 0:4, sep="")))

fit1 <- glm(df$spdfdist~1, family=gaussian)
fit2 <- glm(df$spdfdist~df$date, family=gaussian)
fit3 <- glm(df$spdfdist~df$hour, family=gaussian)
fit4 <- glm(df$spdfdist~df$toField, family=gaussian)
fit5 <- glm(df$spdfdist~df$TemperatureF, family=gaussian)
fit6 <- glm(df$spdfdist~df$WindSpeedMPH, family=gaussian)
fit7 <- glm(df$spdfdist~df$site, family=gaussian)
fit8 <- glm(df$spdfdist~df$dailyrainin, family=gaussian)
fit9 <- glm(df$spdfdist~df$HourlyPrecipIn, family=gaussian)
fit10 <- glm(df$spdfdist~df$Field, family=gaussian)
fit11 <- glm(df$spdfdist~df$BandNmbr, family=gaussian)
fit12 <- glm(df$spdfdist~df$age, family=gaussian)
fit13 <- glm(df$spdfdist~df$sex, family=gaussian)
fit14 <- glm(df$spdfdist~df$type, family=gaussian)

models = c("null", "date", "hour", "toField", "Temp", "WindSpeed", "site", "DailyRain",
           "HourlyPrecip", "Field", "BandNmbr", "age", "sex", "type")


K = c(length(coef(fit1)), length(coef(fit2)), length(coef(fit3)), length(coef(fit4)),
      length(coef(fit5)), length(coef(fit6)), length(coef(fit7)), length(coef(fit8)), 
      length(coef(fit9)), length(coef(fit10)), length(coef(fit11)), length(coef(fit12)),
      length(coef(fit13)), length(coef(fit14)))

n = nrow(df)
LL = c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4), logLik(fit5),
       logLik(fit6), logLik(fit7), logLik(fit8), logLik(fit9), logLik(fit10), logLik(fit11),
       logLik(fit12), logLik(fit13), logLik(fit14))
AICc =-2*LL + 2*K*(n/(n-K-1))


wc.res2 = data.frame(model = models,
                    LogLik = LL,
                    Rank = rank(AICc),
                    AICc)

wc.res2 = wc.res2[order(wc.res2$Rank),]
wc.res2


df = reset
########################################################################
##Rerun for day only 
########################################################################
df = subset(df, format(df$dt, '%H') %in% c(paste(0, 7:9, sep=""), 10:13))

fit1 <- glm(df$spdfdist~1, family=gaussian)
fit2 <- glm(df$spdfdist~df$date, family=gaussian)
fit3 <- glm(df$spdfdist~df$hour, family=gaussian)
fit4 <- glm(df$spdfdist~df$toField, family=gaussian)
fit5 <- glm(df$spdfdist~df$TemperatureF, family=gaussian)
fit6 <- glm(df$spdfdist~df$WindSpeedMPH, family=gaussian)
fit7 <- glm(df$spdfdist~df$site, family=gaussian)
fit8 <- glm(df$spdfdist~df$dailyrainin, family=gaussian)
fit9 <- glm(df$spdfdist~df$HourlyPrecipIn, family=gaussian)
fit10 <- glm(df$spdfdist~df$Field, family=gaussian)
fit11 <- glm(df$spdfdist~df$BandNmbr, family=gaussian)
fit12 <- glm(df$spdfdist~df$age, family=gaussian)
fit13 <- glm(df$spdfdist~df$sex, family=gaussian)
fit14 <- glm(df$spdfdist~df$type, family=gaussian)

models = c("null", "date", "hour", "toField", "Temp", "WindSpeed", "site", "DailyRain",
           "HourlyPrecip", "Field", "BandNmbr", "age", "sex", "type")


K = c(length(coef(fit1)), length(coef(fit2)), length(coef(fit3)), length(coef(fit4)),
      length(coef(fit5)), length(coef(fit6)), length(coef(fit7)), length(coef(fit8)), 
      length(coef(fit9)), length(coef(fit10)), length(coef(fit11)), length(coef(fit12)),
      length(coef(fit13)), length(coef(fit14)))

n = nrow(df)

LL = c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4), logLik(fit5),
       logLik(fit6), logLik(fit7), logLik(fit8), logLik(fit9), logLik(fit10), logLik(fit11),
       logLik(fit12), logLik(fit13), logLik(fit14))

AICc =-2*LL + 2*K*(n/(n-K-1))
  
wc.res3 = data.frame(model = models,
                       LogLik = LL,
                       Rank = rank(AICc),
                       AICc)

wc.res3 = wc.res3[order(wc.res3$Rank),]
wc.res3



