library(worldfootballR)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(patchwork)
library(ggtext)
 
mbappe  <- understat_player_shots(player_url = "https://understat.com/player/3423")  %>% 
           filter(season  == 2023,result=="Goal")                                    %>% 
           mutate(shotType = as.factor(shotType))
rodri   <- understat_player_shots(player_url = "https://understat.com/player/2496")  %>% 
           filter(season  == 2023,result=="Goal")                                    %>% 
           mutate(shotType = as.factor(shotType))
kane    <- understat_player_shots(player_url = "https://understat.com/player/647")   %>% 
           filter(season  == 2023,result=="Goal")                                    %>% 
           mutate(shotType = as.factor(shotType))
haaland <- understat_player_shots(player_url = "https://understat.com/player/8260")  %>% 
           filter(season  == 2023,result=="Goal")                                    %>% 
           mutate(shotType = as.factor(shotType))
 

shotType_levels <- c("Head", "LeftFoot","OtherBodyPart","RightFoot")
mbappe$shoTypeT<-factor(mbappe$shotType,levels=shotType_levels )
rodri$shoTypeT<-factor(rodri$shotType,levels=shotType_levels )
kane$shoTypeT<-factor(kane$shotType,levels=shotType_levels )
haaland$shoTypeT<-factor(haaland$shotType,levels=shotType_levels )



map_shot <- function(data,player_name) {
  
   data <- data  %>%  mutate(x=X*100,y=Y*100)     
   n<-nrow(data)
   
   res_shot<- ggplot(data) +
              annotate_pitch(colour = "white", fill = "springgreen4",  limits = FALSE)          +
              geom_jitter(aes(x = x, y = y,colour = shoTypeT), width = 1, height =  1,size = 2) +
              scale_fill_manual(
              values = c("Head", "LeftFoot","OtherBodyPArt","RightFoot"))                       +
              theme_pitch()                                                                     +
              theme(panel.background = element_rect(fill = "springgreen4"))                     +
              coord_flip(xlim = c(49, 101))                                                     +
              scale_y_reverse()                                                                 +
              scale_color_discrete(drop = FALSE)                                                +  
              scale_size_discrete(drop = FALSE)                                                 + 
              ggtitle(player_name,   paste0("Number of goals:",n))                              + 
              theme(panel.border = element_rect(color = "black", fill = NA, size = 3))          + 
              theme(plot.title = element_text(hjust = 0.5))    
               
  return(res_shot)
}
 
plot1 <- map_shot(mbappe  ,"Kylian Mbappe-Lottin") + guides(colour = "none")
plot2 <- map_shot(rodri   ,"Rodri")                + guides(colour = "none")
plot3 <- map_shot(kane    ,"Harry Kane")           + guides(colour = "none")
plot4 <- map_shot(haaland ,"Erling Haaland")  


###Final display in a 2x2 presentation
overall<-plot1 + plot2 + plot3 + plot4  + plot_layout(guides = "collect") +
                                          patchwork::plot_layout(design = "12  
                                                                           34")  +

  patchwork::plot_annotation(
    title    = paste0('Goals position for top 4 players'),
    subtitle = "Season 2023/2024" ,
    caption  = 'Visualization: https://www.youtube.com/@FlipR2024 source: https://www.https://understat.com/',
    theme    = theme(plot.subtitle = ggtext::element_markdown()  ))
   
overall

###Be interactive

library(plotly)
 


library(plotly)


map_shot_v <- function(data,player_name) {
  
  data <- data                               %>%  
    mutate(x=X*100,y=Y*100)                  %>% 
    mutate(xG2=round(xG,digits = 2))         %>%  
    mutate(match_=paste0(home_team," - ",away_team))
  n <- nrow(data)
  
  res_shot <- ggplot(data) +
    annotate_pitch(colour = "white",
                   fill   = "springgreen4",
                   limits = FALSE) +
    geom_point(aes(x = x, y = y, label1 = xG2,
                   label2 = shotType,
                   label3 = player_assisted,
                   label4 = match_), 
               size = 2, colour = "blue") +
    theme_pitch() +
    theme(panel.background = element_rect(fill = "springgreen4")) +
    coord_flip(xlim = c(49, 101)) +
    scale_y_reverse() +
    ggtitle(paste0(player_name ,  ": Number of goals ",n , ", Season: 2023/2024 "   ," "))
  
  return(res_shot)
  
}


v_plot1<-map_shot_v(kane,"Harry Kane")

ggplotly(v_plot1, tooltip = c("label1", "label2", "label3" , "label4"))










