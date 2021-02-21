
# https://www.statsinsider.com.au/afl/player-ratings

players_top <- read.csv("top_20_players_position.csv", 
                header = TRUE, 
                stringsAsFactors = FALSE)


players_join <- players %>% 
  select(,c(1, 5:48))

# didn't work
# gen forward 16 = Harley Bennell
# ruck 19 = Marc Pittonet
# ruck 16 = Peter Ladhams


players_top <- inner_join(players_top, players_join, by = c("player" = "Player_Name" ))

set.seed(123)

# scale all of our variables to add into k-means model 
all_scaled <- as.data.frame(scale(players_top[,7:50], 
                                  center = TRUE, 
                                  scale = TRUE))

# raw data - only variables to include in model
all_numeric <- dplyr::select_if(players_top[,7:50], is.numeric)


# k means

# verify by plotting variance of columns
# metres gained dominates variance
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(all_numeric, var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(all_numeric, var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)


M1 <- all_scaled %>%  
  select(
  # these ruck-related stats distinguish ruckmen entirely from the group
  # 90.6% variance explained with just these stats
  hitouts,
  hitouts_to_advantage,
  ruck_contests,
  score_launches,
  # this midfield related fields produces some separation of midfielders, reducing overall variance
  clearances,
  contested_possessions,
  uncontested_possessions,
  centre_clearances,
  stoppage_clearances,
  # forwards
  goals,
  tackles_inside_fifty,
  f50_ground_ball_gets,
  # defenders
  rebounds,
  # one_percenters, keep this out, makes too similar to rucks
  # intercepts, # really improves variance, but muddies model
  contest_def_one_on_ones,
  disposal_efficiency_percentage
  )

# Proceed with principal components
pc <- princomp(M1)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(M1)
# First for principal components
comp <- data.frame(pc$x)
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

# Determine number of clusters
wss <- (nrow(M1)-1)*sum(apply(M1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(M1,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 4
# Apply k-means with k=4
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

k # 74.7% 

p <- fviz_cluster(k, data = M1) # save to access $data
p

data <- p$data # this is all you need

# calculate the convex hull using chull(), for each cluster
hull_data <- data %>%
  group_by(cluster) %>%
  slice(chull(x, y))

colour_theme <- c("#E4796A", "#E7B573", "#B0AA96", "#638DCB")

ggplot(data, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_polygon(data = hull_data, 
               alpha = 0.5, 
               aes(fill = cluster, linetype = cluster)) +
  scale_color_manual(values = colour_theme) +
  scale_fill_manual(values = colour_theme) +
  
  theme_grey(base_size = 10)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(colour = "#4E4F4E",
                                    size = 8,
                                    face = "bold"),
        # legend.margin = margin(grid::unit(0,"cm")),
        legend.text = element_text(colour = "#4E4F4E",
                                   size = 8),
        legend.key.height = grid::unit(0.6,"cm"),
        legend.key.width = grid::unit(0.6,"cm"),
        legend.margin = margin(0,0,0,0.2,"cm"), # move a little away from plot, to the right
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        axis.text.x = element_text(size = 8,
                                   colour = "#4E4F4E"),
        axis.text.y = element_text(size = 8,
                                   vjust = 0.2,
                                   colour = "#4E4F4E"),
        axis.ticks = element_line(size = 0.2, 
                                  colour = "#878683"),
        plot.background = element_rect(fill = "#fcf9f0"),
        panel.background = element_blank(),
        legend.background = element_rect(fill = "#fcf9f0"),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        strip.text.x = element_text(size = 8, colour = "#6b6e6b"),
        strip.background = element_rect(fill="#fffdf2"),
        plot.margin = margin(0.7,0.4,0.1,0.2,"cm"),
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
                                    margin = margin(-5,0,0,0))) # adjust position ... top, bottom, left, right



# join back groupings
k$cluster
players_top[,"Cluster"] <- k$cluster

# summarizes statistics for groupings 
player_top_gowers <- players_top %>% 
  group_by(Cluster) %>% 
  summarise_all(funs(mean)) %>% 
  mutate_if(is.numeric, round, 1)


# hierarchical clustering 

# Dissimilarity matrix
d <- dist(M1, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Compute with agnes
hc2 <- agnes(M1, method = "ward")

# Agglomerative coefficient
hc2$ac
## [1] 0.8531583


# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(M1, method = x)$ac
}

map_dbl(m, ac)
##   average    single  complete      ward 
## 0.7379371 0.6276128 0.8531583 0.9346210

hc3 <- agnes(M1, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 


# Cut tree into 4 groups
sub_grp <- cutree(hc3, k = 4)


# Number of members in each cluster
table(sub_grp)
## sub_grp
# 1   2   3   4 
# 153 247 124  30 

players_top %>%
  mutate(cluster = sub_grp) %>%
  head

fviz_cluster(list(data = M1, cluster = sub_grp))




p4 <- M1 %>% 
  ggplot(aes(label = Player))

p4 <- p4 +
autoplot(M1.pca, 
         data = M1,
         colour = "Cluster"
) +
  scale_colour_manual(values = colour_theme) +
  scale_x_continuous(name = "Principal Component 1 (37.4%)") +
  scale_y_continuous(name = "Principal Component 2 (27.3%)") +
  geom_point(alpha = 0.01) +
  geom_text(data = player_labels,
            size = 2,
            colour = "#730202") +
  labs(title = "Cluster Plot",
       subtitle = "Depicting k = 4 for first two principal components + variable loadings") +
  theme_minimal() +
  plot_theme





