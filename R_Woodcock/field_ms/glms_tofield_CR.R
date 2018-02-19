#df = read.csv("C:/Users/Lisa/Documents/Woodcock/Data/Field/dusk_pts_wcovars.csv", header=TRUE)


##to get dff, run "add_covars2.R"

bd = read.csv("C:/Users/Lisa/Documents/Woodcock/Banding.csv", header=TRUE)

#join Bd and dff via plyr
names(bd)[2] = "BandNmbr"
dff = plyr::join(dff,bd[,c("BandNmbr","Sex","Age","Weight")], by="BandNmbr")



# x = length(which(dff$toField==0)) ##nights remaining in forest
# y = length(which(dff$toField==1)) ##nights traveled to fields
# 
dff = dff[which(dff$TemperatureF!=-999.9),]
dff$TemperatureF[which(dff$TemperatureF==-999.99)] <- NA
dff$WindSpeedMPH[which(dff$TemperatureF==-999.99)] <- NA
dff$Humidity[which(dff$TemperatureF==-999.99)] <- NA
dff$WindSpeedGustMPH[which(dff$TemperatureF==-999.99)] <- NA
dff$type = as.factor(rep("type", nrow(dff)))
# 
# fit1 <- glm(dff$toField~1, family=binomial)
# fit2 <- glm(dff$toField~dff$Illumination, family = binomial)
# fit3 <- glm(dff$toField~dff$age, family = binomial)
# fit4 <- glm(dff$toField~dff$sex, family = binomial)
# fit5 <- glm(dff$toField~dff$age*dff$sex, family = binomial)
# fit6 <- glm(dff$toField~dff$site, family=binomial)
# fit7 <- glm(dff$toField~dff$BandNmbr, family=binomial)
# #fit8 <- glm(dff$toField~dff$type, family=binomial) #50/50 for bothar
# #fit9 <- glm(dff$toField~dff$Humidity, family = binomial)
# ##impacted by weather station failure
# fit10 <- glm(dff$toField~dff$HourlyPrecipIn, family = binomial)
# fit11 <- glm(dff$toField~dff$dailyrainin, family = binomial)
# fit12 <- glm(dff$toField~dff$WindSpeedMPH+dff$TemperatureF, family=binomial)
# fit13<- glm(dff$toField~dff$TemperatureF, family = binomial)
# fit14<- glm(dff$toField~dff$WindSpeedMPH, family = binomial)


##################################################
#####rescale explnatroy variables for convergence
dff$sc_Illumination = scale(dff$Illumination)[,1]
dff$sc_TemperatureF = scale(dff$TemperatureF)[,1]
dff$sc_HourlyPrecipIn = scale(dff$HourlyPrecipIn)[,1]
dff$sc_dailyrainin = scale(dff$dailyrainin)[,1]
dff$sc_WindSpeedMPH = scale(dff$WindSpeedMPH)[,1]
#####
library(lme4)
library(AICcmodavg)
####simply cheat and check the structure of the random effect
#create a llist of candiate vector
fitlist2 = vector("list", 3)
#run each model and assign to the list
fitlist2[[1]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|site) +(1|BandNmbr), family=binomial, data=dff)
fitlist2[[2]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|site), family=binomial, data=dff)
fitlist2[[3]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|BandNmbr), family=binomial, data=dff)
#name the model for ease of interpretation
names(fitlist2) = c("both","Onlysite","Onlybird")
#chekc AIC table
aictab(fitlist2)
#best model only has individul as a random effect

####Build model selection for other variables
###create a list for the  models
fitlist = vector("list", 12)
#save model as item in a list
fitlist[[1]] <- glmer(toField ~ 1 + (1|BandNmbr), family=binomial, data=dff)
fitlist[[2]] <- glmer(toField ~ site + (1|BandNmbr), family=binomial, data=dff)
fitlist[[3]] <- glmer(toField ~ sc_Illumination + (1|BandNmbr), family=binomial, data=dff)
fitlist[[4]] <- glmer(toField ~ sc_WindSpeedMPH + sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
fitlist[[5]] <- glmer(toField ~ Age + Sex + (1|BandNmbr), family=binomial, data=dff)
fitlist[[6]] <- glmer(toField ~ Age + Sex + Age:Sex   +(1|BandNmbr), family=binomial, data=dff)
fitlist[[7]] <- glmer(toField ~ site + Age + Sex  +(1|BandNmbr), family=binomial, data=dff)
fitlist[[8]] <- glmer(toField ~ site + sc_WindSpeedMPH +  sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
fitlist[[9]] <- glmer(toField ~ Age + Sex + sc_WindSpeedMPH +  sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
fitlist[[10]] <- glmer(toField ~ sc_Illumination + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +(1|BandNmbr), family=binomial, data=dff)
### model 11 needed more iterations to converge
fitlist[[11]] <- glmer(toField ~ site + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF  +(1|BandNmbr), family=binomial, data=dff, 
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
### model 12 needed more iterations to converge
fitlist[[12]] <- glmer(toField ~ site + sc_Illumination + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF  +(1|BandNmbr), family=binomial, data=dff, 
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

###name models
names(fitlist) = c("Null", "Site", "Moonlight", "Climate", "Age + Sex", "Age*Sex",
                   "Site + Age + Sex", "Site + Climate", "Age + Sex + Climate",
                   "Moonlight + Age + Sex + Climate",
                   "Site + Age + Sex + Climate", 
                   "Full model")
##check best models
mod.table = aictab(fitlist)
print(mod.table)




# K = c(length(coef(fit1)), length(coef(fit2)), length(coef(fit3)), length(coef(fit4)),
#       length(coef(fit5)), length(coef(fit6)), length(coef(fit7)), #length(coef(fit8)),
#        length(coef(fit10)), length(coef(fit11)), length(coef(fit12)),
#       length(coef(fit13)), length(coef(fit14)))#, length(coef(fit15)))
# 
# n = nrow(df)
# 
# LL = c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4), logLik(fit5),
#        logLik(fit6), logLik(fit7), logLik(fit10), logLik(fit11),
#        logLik(fit12), logLik(fit13), logLik(fit14))#, logLik(fit15))
# AICc =-2*LL + 2*K*(n/(n-K-1))
# 
# 
# wc.res4 = data.frame(model = models,
#                     Rank = rank(AICc),
#                     LogLik = LL,
#                     AICc)
# 
# 
# wc.res4 = wc.res4[order(wc.res4$Rank),]
# 
# wc.res4
# 
# 
# 
# fit12
# fit13
# fit14
# 
# 
# ##best fit
# fit6
# 
# summary(fit6)
# 
# exp(fit6$coefficients[1])
# exp(fit6$coefficients[2])
# exp(fit6$coefficients[3])
# exp(fit6$coefficients[4])
# exp(fit6$coefficients[5])
# exp(fit6$coefficients[6])
# 
# 
# fit6
# 
# summary(fit6)
# newdata6=data.frame(site=df$site)
# newdata6$phat <- predict(fit6, newdata6, type = "response")
# newdata6=newdata6[!duplicated(newdata6$site),]
# p3 = ggplot(newdata6, aes(x=site, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
#   xlab("Site")+theme_minimal()
# p3
# ?ggplot
# 
# ggsave("C:/Users/Elisa/Pictures/fit6", plot=p3,
#        device="tiff")
# 
# ##next best
# fit14
# 
# exp(fit14$coefficients[2])
# 
# summary(fit14)
# newdata14=data.frame(wind=df$WindSpeedMPH)
# newdata14$phat <- predict(fit14, newdata14, type = "response")
# #newdata14=newdata14[!duplicated(newdata14$site),]
# newdata14=newdata14[which(as.numeric(newdata14$wind)!=-999.9),]
# p1 = ggplot(newdata14, aes(x=wind, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
#   xlab("Wind Speed")
# p1
# 
# 
# ggsave("C:/Users/Elisa/Pictures/wind", plot=p1,
#        device="tiff")
# newdata14
# #wc.res.test = wc.res
# ##next next best
# fit13
# newdata13=data.frame(temp=df$TemperatureF)
# newdata13$phat <- predict(fit13, newdata13, type = "response")
# #newdata14=newdata14[!duplicated(newdata14$site),]
# newdata13=newdata13[which(as.numeric(newdata13$temp)!=-999.9),]
# p2 = ggplot(newdata13, aes(x=temp, y=phat)) + geom_point() + ylab("Probability of travel to field")+ 
#   xlab("Temperature (F)")
# 
# p2
# 
# test = predict(fit14, type="response")


