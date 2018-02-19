#df = read.csv("C:/Users/Lisa/Documents/Woodcock/Data/Field/dusk_pts_wcovars.csv", header=TRUE)
#head(df)

bd = read.csv("C:/Users/Lisa/Documents/Woodcock/Banding.csv", header=TRUE)

#df$type = rep("type", nrow(df))

#####################################################
##add in sex/age
#####################################################

#for (i in 1:nrow(dff)){
#  m = dff$BandNmbr[i]
#  for (j in 1:nrow(bd)){
#    if (m == bd$Band.Nmbr[j]){
#      browser()
#      dff$weight[i] = bd$Weight[j]
#      dff$age[i] = as.character(bd$Age[j])
#      dff$sex[i] = as.character(bd$Sex[j])
#    }
#  }
  
  #if (df$site[i]=="BOYC" | df$site[i]=="DERI"){
  #  df$type[i]= "pine"
  #} else{df$type[i]= "bothar"}
#}

names(dff)[1] = "Band.Nmbr"
plyr::join(dff,bd[,c("Band.Nmbr","Sex","Age","Weight")], by="Band.Nmbr")

df = dff


x = length(which(df$toField==0)) ##nights remaining in forest
y = length(which(df$toField==1)) ##nights traveled to fields

#df = df[which(df$TemperatureF!=-999.9),]
df$TemperatureF[which(df$TemperatureF==-999.99)] <- NA
df$WindSpeedMPH[which(df$TemperatureF==-999.99)] <- NA
df$Humidity[which(df$TemperatureF==-999.99)] <- NA
df$WindSpeedGustMPH[which(df$TemperatureF==-999.99)] <- NA

df$type = as.factor(df$type)

fit1 <- glm(df$toField~1, family=binomial)
fit2 <- glm(df$toField~df$Illumination, family = binomial)
fit3 <- glm(df$toField~df$age, family = binomial)
fit4 <- glm(df$toField~df$sex, family = binomial)
fit5 <- glm(df$toField~df$age*df$sex, family = binomial)
fit6 <- glm(df$toField~df$site, family=binomial)
fit7 <- glm(df$toField~df$BandNmbr, family=binomial)
#fit8 <- glm(df$toField~df$type, family=binomial) #50/50 for bothar
#fit9 <- glm(df$toField~df$Humidity, family = binomial)
##impacted by weather station failure
fit10 <- glm(df$toField~df$HourlyPrecipIn, family = binomial)
fit11 <- glm(df$toField~df$dailyrainin, family = binomial)
fit12 <- glm(df$toField~df$WindSpeedMPH+df$TemperatureF, family=binomial)
fit13<- glm(df$toField~df$TemperatureF, family = binomial)
fit14<- glm(df$toField~df$WindSpeedMPH, family = binomial)


models = c("Null", "Moonlight", "Age", "Sex", "Age*Sex", "Site", "Individual",
            "HourlyRain", "DailyRain","Wind+Temp", "Temp", "WindSpeed")


K = c(length(coef(fit1)), length(coef(fit2)), length(coef(fit3)), length(coef(fit4)),
      length(coef(fit5)), length(coef(fit6)), length(coef(fit7)), #length(coef(fit8)),
       length(coef(fit10)), length(coef(fit11)), length(coef(fit12)),
      length(coef(fit13)), length(coef(fit14)))#, length(coef(fit15)))

n = nrow(df)

LL = c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4), logLik(fit5),
       logLik(fit6), logLik(fit7), logLik(fit10), logLik(fit11),
       logLik(fit12), logLik(fit13), logLik(fit14))#, logLik(fit15))
AICc =-2*LL + 2*K*(n/(n-K-1))


wc.res4 = data.frame(model = models,
                    Rank = rank(AICc),
                    LogLik = LL,
                    AICc)


wc.res4 = wc.res4[order(wc.res4$Rank),]

wc.res4



fit12
fit13
fit14


##best fit
fit6

summary(fit6)

exp(fit6$coefficients[1])
exp(fit6$coefficients[2])
exp(fit6$coefficients[3])
exp(fit6$coefficients[4])
exp(fit6$coefficients[5])
exp(fit6$coefficients[6])


fit6

summary(fit6)
newdata6=data.frame(site=df$site)
newdata6$phat <- predict(fit6, newdata6, type = "response")
newdata6=newdata6[!duplicated(newdata6$site),]
p3 = ggplot(newdata6, aes(x=site, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
  xlab("Site")+theme_minimal()
p3
?ggplot

ggsave("C:/Users/Elisa/Pictures/fit6", plot=p3,
       device="tiff")

##next best
fit14

exp(fit14$coefficients[2])

summary(fit14)
newdata14=data.frame(wind=df$WindSpeedMPH)
newdata14$phat <- predict(fit14, newdata14, type = "response")
#newdata14=newdata14[!duplicated(newdata14$site),]
newdata14=newdata14[which(as.numeric(newdata14$wind)!=-999.9),]
p1 = ggplot(newdata14, aes(x=wind, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
  xlab("Wind Speed")
p1


ggsave("C:/Users/Elisa/Pictures/wind", plot=p1,
       device="tiff")
newdata14
#wc.res.test = wc.res
##next next best
fit13
newdata13=data.frame(temp=df$TemperatureF)
newdata13$phat <- predict(fit13, newdata13, type = "response")
#newdata14=newdata14[!duplicated(newdata14$site),]
newdata13=newdata13[which(as.numeric(newdata13$temp)!=-999.9),]
p2 = ggplot(newdata13, aes(x=temp, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
  xlab("Temperature (F)")

p2

test = predict(fit14, type="response")


