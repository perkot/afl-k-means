library(dplyr)
M1 %>%
  group_by(Match) %>%
  tally() %>% 
  mutate(percent = n / sum(n)*100)


M1.pca.pc3[,"Player"] <- df.players$Player_Name


M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Harris Andrews"] <- "Harris Andrews"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Steven May"] <- "Steven May"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Darcy Moore"] <- "Darcy Moore"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Jacob Weitering"] <- "Jacob Weitering"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Jeremy McGovern"] <- "Jeremy McGovern"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Jake Lloyd"] <- "Jake Lloyd"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Caleb Daniel"] <- "Caleb Daniel"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Rory Laird"] <- "Rory Laird"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Daniel Rich"] <- "Daniel Rich"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Bachar Houli"] <- "Bachar Houli"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Toby Greene"] <- "Toby Greene"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Chad Wingard"] <- "Chad Wingard"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Shai Bolton"] <- "Shai Bolton"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Isaac Heeney"] <- "Isaac Heeney"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Liam Ryan"] <- "Liam Ryan"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Tom Hawkins"] <- "Tom Hawkins"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==   "Lance Franklin"] <-  "Lance Franklin"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Josh J. Kennedy"] <- "Josh J. Kennedy"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Jack Gunston"] <- "Jack Gunston"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Lachie Neale"] <- "Lachie Neale"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Dustin Martin"] <- "Dustin Martin"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Travis Boak"] <- "Travis Boak"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Jack Steele"] <- "Jack Steele"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Nat Fyfe"] <- "Nat Fyfe"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Max Gawn"] <- "Max Gawn"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Nic Naitanui"] <- "Nic Naitanui"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Todd Goldstein"] <- "Todd Goldstein"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==  "Rowan Marshall"] <- "Rowan Marshall"
M1.pca.pc3$Player_Top[M1.pca.pc3$Player ==   "Jarrod Witts"] <-  "Jarrod Witts"

# devtools::install_github("jimmyday12/fitzRoy")
# library(fitzRoy)
# playerstats <- fetch_player_stats(season = 2020, source = "fryzigg")

positions.SI <- read.csv("afl_stats_position_stats_insider.csv")

unique.positions.SI <- unique(positions.SI)
unique.positions.SI

positions.SI$PositionSI[positions.SI$Position ==   "Gen Def"] <-  "Defender"
positions.SI$PositionSI[positions.SI$Position ==   "Key Def"] <-  "Defender"
positions.SI$PositionSI[positions.SI$Position ==   "Gen Fwd"] <-  "Forward"
positions.SI$PositionSI[positions.SI$Position ==   "Key Fwd"] <-  "Forward"
positions.SI$PositionSI[positions.SI$Position ==   "Mid"] <-  "Midfield"
positions.SI$PositionSI[positions.SI$Position ==   "Ruck"] <-  "Ruck"

positions.SI <- positions.SI %>% 
  select(Player, PositionSI)

M1 <- left_join(M1, positions.SI, by = "Player")

M1$PositionSI <- ifelse(is.na(M1$PositionSI), M1$Position, M1$PositionSI)

# Change cluster names to ~ position
M1$PositionSI[M1$PositionSI == "1"] <- "Defender"
M1$PositionSI[M1$PositionSI == "2"] <- "Forward"
M1$PositionSI[M1$PositionSI == "3"] <- "Midfield"
M1$PositionSI[M1$PositionSI == "4"] <- "Ruck"


M1$PositionSI.x <- NULL
M1$PositionSI.y <- NULL

M1$PositionSI <- NULL



library(plyr)
counts <- ddply(M1, .(M1$Cluster, M1$Position), nrow)

mismatch_position.s <- M1 %>% 
  filter(PSP.Match == FALSE) %>% # only mismatches
  ddply(.(Cluster, Position), nrow) %>% # count combinations
  arrange(desc(V1)) # order descending


Group <- M1 %>% 
  select(c(1:16)) %>% 
  group_by(Cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1) 

long <- Group %>% gather(Statistic, # name of column being truncated
                         SD_Units, # name of values
                         hitouts:disposal_efficiency_percentage) # columns to truncate
  
long <- long %>%
  mutate(above_mean = SD_Units > 0)

plot_theme2 <-
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.title.x = element_text(colour = "#4E4F4E",
                                    size = 9,
                                    face = "bold",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "#4E4F4E",
                                    size = 9,
                                    face = "bold",
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        panel.border = element_blank(),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.title = element_text(colour = "#4E4F4E",
                                  hjust = 0,
                                  size = 10,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "#6b6e6b",
                                     hjust = 0,
                                     size = 9),
        plot.caption = element_text(colour = "#4E4F4E",
                                    hjust = 0,
                                    vjust = 1,
                                    size = 6,
                                    face = "italic",
                                    margin = margin(-5,0,0,0)))

## flip axes

# #F15151 RED 
# #FF9D3F ORANGE 
# #FFC52B YELLOW 
# #6CCA62 GREEN
# #48C4AD TEAL 
# #8CD04F GREEN 
# #2BAAED BLUE
# #DA8EDA VIOLET 

colour_theme_2 <- c("#F15151","#6CCA62")

long$Statistic[long$Statistic == "hitouts"] <- "HO"
long$Statistic[long$Statistic == "hitouts_to_advantage"] <- "HOTA"
long$Statistic[long$Statistic == "ruck_contests"] <- "RC"
long$Statistic[long$Statistic == "score_launches"] <- "SL"
long$Statistic[long$Statistic == "clearances"] <- "C"
long$Statistic[long$Statistic == "contested_possessions"] <- "CP"
long$Statistic[long$Statistic == "uncontested_possessions"] <- "UP"
long$Statistic[long$Statistic == "centre_clearances"] <- "CC"
long$Statistic[long$Statistic == "stoppage_clearances"] <- "SC"
long$Statistic[long$Statistic == "goals"] <- "G"
long$Statistic[long$Statistic == "tackles_inside_fifty"] <- "TIF"
long$Statistic[long$Statistic == "f50_ground_ball_gets"] <- "FFGG"
long$Statistic[long$Statistic == "rebounds"] <- "R"
long$Statistic[long$Statistic == "contest_def_one_on_ones"] <- "CDOOO"
long$Statistic[long$Statistic == "disposal_efficiency_percentage"] <- "DE"





p <-
ggplot(data = long,
       aes(x = Statistic, y = SD_Units, fill = above_mean))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colour_theme_2) + 
  coord_flip() +
  labs(x = "",
       y = "",
       title = "Profile of match-day statistics for each cluster-grouping",
       subtitle = "Measured in standard deviation units above or below the mean"
  ) +
  theme_minimal() +
  plot_theme2 +
  theme(strip.text.x = element_text(size = 10, color = "#4E4F4E"),
        strip.text.y = element_text(size = 8, color = "#4E4F4E"),
        strip.background = element_blank(),
        legend.position = "none") 
p + facet_wrap(~ Cluster, nrow = 2) 


# SILHOUETTE WIDTHS
# pull apart function to show specific results for k = 4
km.res <- kmeans(M1, 
                 centers = 4, 
                 nstart = 25)
ss <- silhouette(km.res$cluster, 
                 dist(M1))

# shows us indidvidual silhouette widths 
summary(ss)


