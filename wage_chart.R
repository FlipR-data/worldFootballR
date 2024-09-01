library(worldfootballR)
library(dplyr)
library(ggplot2)



#install.packages("rstatix")
#library(rstatix)
#mapped_players <- player_dictionary_mapping()
haaland <- tm_player_transfer_history(player_url = "https://www.transfermarkt.com/erling-haaland/profil/spieler/418560")
mbappe<-   tm_player_transfer_history(player_url = "https://www.transfermarkt.com/kylian-mbappe/profil/spieler/342229")
kane<-    tm_player_transfer_history(player_url = "https://www.transfermarkt.com/harry-kane/profil/spieler/132098")
rodri<-    tm_player_transfer_history(player_url  = "https://www.transfermarkt.com/rodri/profil/spieler/357565")



the_four_best<-rbind(haaland[1,],mbappe[1,],kane[1,],rodri[1,])
 
# Scatterplot

gg1<-ggplot(the_four_best,
            aes(x = market_value,
                xend = 0,
                y = player_name,
                
                colour = player_name)) +
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  
  labs(x = "Market Value",
       y = "",
       title = "Four best players  Market Values",
       caption = "Data source: www.transfermarkt.com") +
  theme(legend.position = "off")


gg1
###Why right midfield has the most value 

kane_2<- kane[complete.cases(kane[,c("transfer_date","market_value")]),]
rodri_2<- rodri[complete.cases(rodri[,c("transfer_date","market_value")]),]
haaland_2<- haaland[complete.cases(haaland[,c("transfer_date","market_value")]),]
mbappe_2<- mbappe[complete.cases(mbappe[,c("transfer_date","market_value")]),]

the_four_best<-rbind(haaland_2,mbappe_2,kane_2,rodri_2)

ggplot(the_four_best,   aes(x = transfer_date,  y = market_value,color=player_name )) +
  geom_line() +
  geom_point()  

plottt<-ggplot(the_four_best,   aes(x = transfer_date,  y = market_value,color=player_name)) +
    geom_line() +
    geom_point(  aes(label1 = team_to ,label2 =market_value))  + 
    labs(x = "Market Value",y= "Transfer Date",
         title = "Top 4 Transfer Market History" ,
         colour = "Player Name" )


library(plotly)
ggplotly(plottt, tooltip = c("label1","label2"))


