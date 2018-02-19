#df = read.csv("C:/Users/Lisa/Documents/Woodcock/Data/Field/dusk_pts_wcovars.csv", header=TRUE)


##to get dff, run "add_covars2.R"

bd = read.csv("C:/Users/Lisa/Documents/Woodcock/Banding.csv", header=TRUE)

reset = dff
#dff=reset
#join Bd and dff via plyr
names(bd)[2] = "BandNmbr"
dff = plyr::join(dff,bd[,c("BandNmbr","Sex","Age","Weight")], by="BandNmbr")


#x = length(which(dff$toField==0)) ##nights remaining in forest
#y = length(which(dff$toField==1)) ##nights traveled to fields
 
#dff = dff[which(dff$TemperatureF!=-999.9),]
dff$TemperatureF[which(dff$TemperatureF==-999.99)] <- NA
dff$WindSpeedMPH[which(dff$TemperatureF==-999.99)] <- NA
dff$Humidity[which(dff$TemperatureF==-999.99)] <- NA
dff$WindSpeedGustMPH[which(dff$TemperatureF==-999.99)] <- NA
dff$type = rep("type", nrow(dff))

for(i in 1:nrow(dff)){
if(dff$site[i]=="BOYC" | dff$site[i]=="DERI"){
    dff$type[i]= "pine"
    } else{dff$type[i]= "bothar"}
}

dff$type = as.factor(dff$type)

table(dff$site)
table(dff$type)
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
fitlist2 = vector("list", 4)
#run each model and assign to the list
fitlist2[[1]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|site) +(1|BandNmbr), family=binomial, data=dff)
fitlist2[[2]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|site), family=binomial, data=dff)
fitlist2[[3]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF +  (1|BandNmbr), family=binomial, data=dff)
fitlist2[[4]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF + (1|type), family=binomial, data=dff)
fitlist2[[5]] <- glmer(toField ~ 1 + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF + (1|type) + (1|BandNmbr), family=binomial, data=dff)
#name the model for ease of interpretation
names(fitlist2) = c("site/bird","Onlysite","Onlybird", "OnlyHType", "HType/bird")
#chekc AIC table
aictab(fitlist2)
#best model only has individul as a random effect

####Build model selection for other variables
###create a list for the  models
fitlist = vector("list", 15)
#save model as item in a list
fitlist[[1]] <- glmer(toField ~ 1 + (1|BandNmbr), family=binomial, data=dff)
fitlist[[2]] <- glmer(toField ~ sc_WindSpeedMPH + sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
fitlist[[3]] <- glmer(toField ~ Age + Sex + (1|BandNmbr), family=binomial, data=dff)
fitlist[[4]] <- glmer(toField ~ Age + Sex + Age:Sex   +(1|BandNmbr), family=binomial, data=dff)
fitlist[[5]] <- glmer(toField ~ type + Age + Sex  +(1|BandNmbr), family=binomial, data=dff)
##model failed to converge with max|grad| = 0.0024
fitlist[[6]] <- glmer(toField ~ type + sc_WindSpeedMPH +  sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
fitlist[[7]] <- glmer(toField ~ Age + Sex + sc_WindSpeedMPH +  sc_TemperatureF + (1|BandNmbr), family=binomial, data=dff)
### model 11 needed more iterations to converge
fitlist[[8]] <- glmer(toField ~ type + Age + Sex + sc_WindSpeedMPH + sc_TemperatureF  +(1|BandNmbr), family=binomial, data=dff)
### model 12 needed more iterations to converge
fitlist[[9]] <- glmer(toField~dsf+type+sc_TemperatureF+Age+Sex+sc_WindSpeedMPH+(1|BandNmbr), family=binomial, data=dff)
fitlist[[10]] <- glmer(toField ~ dsf+(1|BandNmbr),family=binomial, data=dff)
fitlist[[11]] <- glmer(toField ~ dsf + sc_WindSpeedMPH +sc_TemperatureF+(1|BandNmbr), family=binomial, data=dff)
fitlist[[12]] <- glmer(toField ~ dsf + Age + Sex + (1|BandNmbr), family=binomial, data=dff)
fitlist[[13]] <- glmer(toField ~ type+(1|BandNmbr), family=binomial, data=dff)
fitlist[[14]] <- glmer(toField ~ dsf+type+(1|BandNmbr), family=binomial, data=dff)
fitlist[[15]] <- glmer(toField ~ dsf+type+sc_TemperatureF+sc_WindSpeedMPH+(1|BandNmbr), family=binomial, data=dff)
fitlist[[16]] <- glmer(toField ~ Age+Sex+sc_TemperatureF+sc_WindSpeedMPH+sc_dailyrainin+
                         (1|BandNmbr), family=binomial, data=dff)
fitlist[[17]] <- glmer(toField ~ sc_TemperatureF+sc_WindSpeedMPH+sc_dailyrainin+(1|BandNmbr), family=binomial, data=dff)

###name models
names(fitlist) = c("Null", "Climate","Age + Sex", "Age*Sex","HType + Age + Sex",
                   "HType + Climate", "Age + Sex + Climate","
                   HType + Age + Sex + Climate","DSF+HType+Climate+Age+Sex", 
                   "DSF", "DSF+Climate", "DSF+Age+Sex", "HType",
                   "DSF+HType", "DSF+Htype+Climate", "Rain+Age+Sex+Climate",
                   "Rain+Climate")
##check best models
mod.table = aictab(fitlist)
print(mod.table)
View(mod.table)

fitlist[[9]]


