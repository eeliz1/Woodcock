
#read in combined file

x = read.csv("C:/Users/Elisa/Documents/Woodcock/R_Woodcock/comb_woodcock.csv",header=TRUE)
head(x)


#this creates new variables for the metrics you need to edit via division
## note it has to be x$name = x$oldname/divisor
x$Density_percentage=x$Density/40*100
x$canopy_percentage = x$Canopy/37*100

head(x)

#this is a piece of summary test code for you to start building off of.
summarize_samplepos=ddply(x, .(SamplePos), summarise,
             mean_BasalArea=mean(Basal_Area),
             sd_Basal_Area=sd(Basal_Area),
             mean_canopy_percentage=mean(canopy_percentage),
             sd_canopy_percentage=sd(canopy_percentage),
             mean_Litter=mean(Litter),
             sd_Litter=sd(Litter),
             mean_Herbac=mean(Herbac),
             sd_Herbac=sd(Herbac),
             mean_Shrub=mean(Shrub),
             sd_Shrub=sd(Shrub),
             mean_Tree=mean(Tree),
             sd_Tree=sd(Tree),
             mean_Bare_ground=mean(Bare_ground),
             sd_Bare_ground=sd(Bare_ground),
             mean_Other=mean(Other),
             sd_Other=sd(Other),
             mean_Density=mean(Density_percentage),
             sd_Density=sd(Density_percentage)
)


summarize_dateut=ddply(x, .(Date_utilized), summarise,
              mean_BasalArea=mean(Basal_Area),
              sd_Basal_Area=sd(Basal_Area),
              mean_canopy_percentage=mean(canopy_percentage),
              sd_canopy_percentage=sd(canopy_percentage),
              mean_Litter=mean(Litter),
              sd_Litter=sd(Litter),
              mean_Herbac=mean(Herbac),
              sd_Herbac=sd(Herbac),
              mean_Shrub=mean(Shrub),
              sd_Shrub=sd(Shrub),
              mean_Tree=mean(Tree),
              sd_Tree=sd(Tree),
              mean_Bare_ground=mean(Bare_ground),
              sd_Bare_ground=sd(Bare_ground),
              mean_Other=mean(Other),
              sd_Other=sd(Other),
              mean_Density=mean(Density_percentage),
              sd_Density=sd(Density_percentage)
              )


summarize_BandNmbr=ddply(x, .(BandNmbr), summarise,
                       mean_BasalArea=mean(Basal_Area),
                       sd_Basal_Area=sd(Basal_Area),
                       mean_canopy_percentage=mean(canopy_percentage),
                       sd_canopy_percentage=sd(canopy_percentage),
                       mean_Litter=mean(Litter),
                       sd_Litter=sd(Litter),
                       mean_Herbac=mean(Herbac),
                       sd_Herbac=sd(Herbac),
                       mean_Shrub=mean(Shrub),
                       sd_Shrub=sd(Shrub),
                       mean_Tree=mean(Tree),
                       sd_Tree=sd(Tree),
                       mean_Bare_ground=mean(Bare_ground),
                       sd_Bare_ground=sd(Bare_ground),
                       mean_Other=mean(Other),
                       sd_Other=sd(Other),
                       mean_Density=mean(Density_percentage),
                       sd_Density=sd(Density_percentage)
)


summarize_samplepos
summarize_dateut
summarize_BandNmbr

#export to table; if file name same as preexisting file, will replace
write.csv(summarize_samplepos, "R:/Lisa/R/samplepos_export.csv",
            append = FALSE)

write.csv(summarize_dateut, "R:/Lisa/R/dateut_export.csv",
          append = FALSE)

write.csv(summarize_BandNmbr, "R:/Lisa/R/BandNmbr_export.csv",
          append = FALSE)


#colnames(my.out)=c("BandNmbr", "Date_utilized", "Sample_Pos", "Mean", 
"SD", "Max")
#my.out
#
#
#
xx=x
summary(aov(xx$Litter~xx$SamplePos))
summary(aov(xx$Herbac~xx$SamplePos))
summary(aov(xx$Shrub~xx$SamplePos))
summary(aov(xx$Tree~xx$SamplePos))
summary(aov(xx$Bare_ground~xx$SamplePos))
summary(aov(xx$Density~xx$SamplePos))
aov(xx$Litter~xx$SamplePos)