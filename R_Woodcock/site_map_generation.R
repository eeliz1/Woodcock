library(ggplot2)
library(ggmap)

############################################################################################
##Helpful source: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
############################################################################################

##creating data frame with site locations
Sites = c("Deridder", "Boyce", "Dewey Wills WMA", "Sherburne WMA",
          "Tensas NWR", "Red River NWR", "Yancey WMA", "Palmetto")
region = c("West Gulf Coastal Plains","West Gulf Coastal Plains", "Mississippi Alluvial Valley","Mississippi Alluvial Valley",
           "Mississippi Alluvial Valley", "West Gulf Coastal Plains", "Mississippi Alluvial Valley", "Mississippi Alluvial Valley")
lats= c(30.826063, 31.299694, 31.433662, 30.515264, 32.311818, 32.204581, 31.205506, 30.689852) 
longs= c(-93.377845, -92.665665, -92.039573,  -91.718223, -91.421592, -93.578833, -91.644359, -91.870362)

sites<- as.data.frame(cbind(Sites,region, lats, longs))
sites$lats = as.numeric(lats)
sites$longs = as.numeric(longs)

##pulling state of Louisiana
states <- map_data("state")
LA <- subset(states, region %in% c("louisiana"))

##get basemap for LA and surrounding area
base <- get_map("Louisiana", source="google", maptype="satellite", zoom=7)


ggmap(base, extent="device")+
  geom_polygon(data=LA, aes(x = long, y = lat), alpha=0.3, fill="cornsilk4")+
  #geom_polygon(data=LA, aes(x = long, y = lat), fill = "cornsilk2", alpha=0.7, color = "black")+
  coord_fixed(1.3)+
  geom_point(data=sites, aes(x=longs, y = lats, shape=region, color=Sites), fill="cornsilk4", size=5)

#########################
##dull figure for Bret
#########################
sites2 = sites[5,]
sites3= sites[7, ]

ggplot()+geom_polygon(data=LA, aes(x = long, y = lat), fill="white", color="black") + 
  coord_fixed(1.3)+
  geom_point(data=sites, aes(x=longs, y=lats), size=3.5)+
#  geom_point(data=sites2, aes(x=longs, y=lats), shape=1, fill="white", size=3)+
#  geom_point(data=sites3, aes(x=longs, y=lats), shape=2, fill="white", size=3)+
  geom_label(data=sites, aes(x=longs, y=lats, label=Sites),fill="gray92",label.size=0.01, alpha=0.7, hjust=-0.15, vjust=0.48)+
  theme_void()
