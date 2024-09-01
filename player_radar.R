#install.packages("fmsb") #to get beautiful radar charts 
install.packages("worldfootballR")
library(worldfootballR)
library(tidyverse)
library(fmsb)
####Get player dAta from FBREF (url)
 
data_player <- function(url) {
###import data from FBREF
  ##Add a if condition as Messi is not anymore in an Euriopean league
player_data<-fb_player_scouting_report( url,
                                        "primary",
                                        time_pause = 3,
                                        league_comp_name = "Last 365 Days Men's Big 5 Leagues, UCL, UEL"
                                         ) 
 
####Selected parameter (player characteristics we would like to focus on)
parameters<-c("Goals","xG: Expected Goals","Pass Completion %", "Key Passes", "Assists","Touches",
              "Successful Take-Ons","Tackles","GCA (Defensive Action)","Blocks","Clearances" )

# Define the variable ranges: maximum and minimum required by the radar plot
max_min <- data.frame(
  Goals = c(1, 0), `xG: Expected Goals`  = c(1, 0), `Pass Completion %` = c(100, 0),
  Assists = c(1, 0), `Key Passes` = c(4, 0),  `GCA (Defensive Action)` = c(0.05, 0),
  Tackles = c(40, 0), Blocks = c(1, 0), Clearances  = c(2, 0) ,
  Touches = c(300, 0),  `Successful Take-Ons` = c(10, 0))
 
###data wrangling to keep and be able to use for the radar chart
study_player<<-player_data %>% filter(Statistic %in% parameters)  %>% 
                               filter(StatGroup != "Standard")    %>%
                               select(  Statistic,Per90)          %>%
                               pivot_wider(names_from = Statistic, values_from = Per90) 

name_player       <- paste0(player_data[1,"Player"]," (",player_data[1,"Versus"],")")
names(max_min)    <- names(study_player) 
df                <- rbind(max_min, study_player ) 
rownames(df)      <- c("max","min", name_player )
names(df)         <- c( "Goals",  "Exp. Goal", "Pass complation" ,"Assist", "Key Pass" ,"GCA Defensive)" ,
                        "Tackles" ,"Blocks", "Clearance","Touches","Successful Take-Ons" )

###radar plot  settings
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9)  )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4)  )

# plot with default options:
plot_player <- 
  
  radarchart( df  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.2), cglwd=1,
            #custom labels
            vlcex=0.8 
)
# Add a legend
 legend(x=-0.8, y=-1.2, legend = rownames(df[-c(1,2),]), bty = "n", pch=10 , col=colors_in , text.col = "grey", cex=1, pt.cex=3)

}
 


###Generate and save figures in a PDF file

pdf("top4_radarplot.pdf")
par(mfrow = c(2,2),mar = c(1, 1, 1, 1),mgp = c(2, 1, 0), xpd = NA)     

p1<-data_player("https://fbref.com/en/players/21a66f6a/Harry-Kane")
p2<-data_player("https://fbref.com/en/players/42fd9c7f/Kylian-Mbappe")
p3<-data_player("https://fbref.com/en/players/1f44ac21/Erling-Haaland")
p4<-data_player("https://fbref.com/en/players/6434f10d/Rodri")
mtext("Top 4 Players 2023", side = 3, line = - 1.3, outer = TRUE,cex = 1.5, font=4, col="red")

dev.off() 



 #WOw WorldfootballR don t stop to surpise me. After my fsirst tu what do I find ? THe ggshaker library !!
 


 