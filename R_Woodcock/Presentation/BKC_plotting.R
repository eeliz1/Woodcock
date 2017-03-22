library(ggplot2)
library(grid)

##plotting diurnal MCPs
df = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/MCP_attribute_export.csv", header=T)
head(df)

x = which(df$area.ha < 2)
df = df[x,]

mean = mean(df$area.ha)

##Day MCP figure
ggplot()+geom_histogram(aes(df$area.ha),fill="cornsilk2", binwidth = 0.005)+geom_vline(xintercept=mean, color="firebrick1", size=2)+
  #labs(x='', y='')+
  xlab("Area (ha)")+ylab("Number of MCPs")+
  scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, -0.2))+
  geom_text(aes(0.06, 38, label=round(mean, 2)), color="firebrick1", size=10)+
  theme(plot.background = element_rect(fill = "black"), text=element_text(size=30),
        panel.background = element_rect(fill="black"), axis.text=element_text(colour="cornsilk2"),
        axis.title = element_text(color="cornsilk2"))


##Night MCPs
df2 = read.csv("C:/Users/Elisa/Documents/Woodcock/Data/GIS/Night_MCP_export.csv")

df2$area.ha = df2$area/10000
y = which(df2$area.ha < 2)
df2 = df2[y,]

mean2 = mean(as.numeric(df2$area.ha))


ggplot()+geom_histogram(aes(df2$area.ha),fill="cornsilk2", binwidth = 0.05)+geom_vline(xintercept=mean2, color="firebrick1", size=2)+
  #labs(x='', y='')+
  xlab("Area (ha)")+ylab("Number of MCPs")+
  #scale_x_continuous(expand = c(0, 0))+scale_y_continuous(expand = c(0, -0.2))+
  geom_text(aes(0.15, 60, label=round(mean2, 2)), color="firebrick1", size=10)+
  theme(plot.background = element_rect(fill = "black"), text=element_text(size=30),
        panel.background = element_rect(fill="black"), axis.text=element_text(colour="cornsilk2"),
        axis.title = element_text(color="cornsilk2"))





