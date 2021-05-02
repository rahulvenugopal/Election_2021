# Visualising Kerala election results of 20201
# @author Rahul Venugopal

# Loading libraries
library(ggplot2)
library(ggpol) # ggplot2 extension for drawing parliament charts
library(readxl)
library(tidyverse)


# loading data
df<-read_csv("kerala_elections.csv")

df1<-df%>%group_by(Party)%>%summarise(Seats=sum(Seats))%>%arrange(Seats)
df1$legend <- paste0(df1$Party," (", df1$Seats,")")

#set colors manually
colors<-c("#004387","#cd0000")

#draw a parliament diagram 
p<-ggplot(df1) + 
  geom_parliament(aes(seats =Seats, fill =  Party), color = "white") + 
  scale_fill_manual(values = colors , labels = df1$legend) +
  coord_fixed() + 
  theme_void()+

# Theming styles
  
  labs(title  = 'Kerala Assembly election result 2021',
       subtitle="Each elected member represents one of the 140 constituencies within the borders of Kerala",
       caption = "Dataviz | Rahul Venugopal")+
  theme(title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(vjust =7,hjust = 0.95),    
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.spacing.y = unit(0.1,"cm"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.key.size = unit(0.8, 'lines'),
        legend.text = element_text(margin = margin(r = 1, unit = 'cm')),
        legend.text.align = 0)+
  annotate("text", x = 0, y = 0.4, label = "140 seats",colour = "black",size=18)+
  guides(fill=guide_legend(nrow=1,byrow=TRUE,reverse = TRUE,title=NULL)) + 
  theme(plot.margin = margin(t = 25, r = 25, b = 10, l = 25)) + 
  
ggsave("Election_Kerala.jpeg",width = 11, height = 6, dpi = 600)

# Finding x and y ranges to plan annotations
#ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
#ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
