library(ggplot2)
library(ggpol) # ggplot2 extension for drawing parliament charts
library(readxl)
library(tidyverse)

#load data
df<-read_csv("parliament.csv")

df1<-df%>%group_by(Party)%>%summarise(Seats=sum(Seats))%>%arrange(Seats)
df1$legend <- paste0(df1$Party," (", df1$Seats,")")

#set colors manually
colors<-c("#cdd41f","#55a8ce","#c4ba7b","#d2044d","#f75914","#c3537f","#92b6be","#b3a3fa","#26aa5e")

#draw a parliament diagram 
p<-ggplot(df1) + 
  geom_parliament(aes(seats =Seats, fill =  Party), color = "white") + 
  scale_fill_manual(values = colors , labels = df1$legend) +
  coord_fixed() + 
  theme_void()+
  
  
  labs(title  = "Ukrainian Parliament of the 9th convocation",
       subtitle="The number and distribution of seats in the Verkhovna Rada of Ukraine",
       caption = "Dataviz | Rahul Venugopal")+
  theme(title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(vjust =7,hjust = 1.1),    
        legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.spacing.y = unit(0.1,"cm"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.key.size = unit(0.8, 'lines'),
        legend.text = element_text(margin = margin(r = 1, unit = 'cm')),
        legend.text.align = 0)+
  annotate("text", x = 0, y = 0.4, label = "Seats in the Parliament :\n 423 occupied \n 27 vacant",colour = "black",size=6)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE,reverse = TRUE,title=NULL))

p

# Extra options
geom_highlight_government(Party == "Servant of the People")
geom_parliament_bar(colour = colour, party = party_long)
theme_ggparliament(legend = FALSE)
draw_majoritythreshold(n = 100, label = TRUE, type = 'semicircle')