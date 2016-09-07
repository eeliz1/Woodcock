#This creates a new data frame for you to work on.
x=read.csv("C:/Users/Elisa/Documents/Woodcock/R_Woodcock/comb_woodcock.csv")

head(x)
#this creates new variables for the metrics you need to edit via division
## note it has to be x$name = x$oldname/divisor
x$Density_percentage=x$Density/40*100
x$canopy_percentage=x$Canopy/37*100

str(x)

#this is a piece of summary test code for you to start building off of.
my.out=ddply(x, .(BandNmbr, Date_utilized), summarise,
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

my.out
#colnames(my.out)=c("BandNmbr", "Date_utilized", "Sample_Pos", "Mean", 
"SD", "Max")

write.csv(my.out, "C:/Users/Elisa/Documents/Woodcock/R_Woodcock/date_means.csv")
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