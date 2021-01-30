set.seed(123)

# convert games played to numeric
players$Games <- as.numeric(players$Games)

# filter to only players who have played at least an entire season worth of games 
players2 <- players %>% 
  dplyr::filter(Games >= 60)

players2 <- players2 %>%  
  select(
    Player_Name,
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

players_scaled2 <- scale(players2[,2:16], center = TRUE, scale = TRUE)
players_numeric2 <- dplyr::select_if(players2, is.numeric)

# https://discuss.analyticsvidhya.com/t/interpretation-result-of-k-means-algorithm/17750

# high similarity within a group = low variance within the cluster, or within_SS.
# low similarity between the groups = high variance between the clusters, or between_SS.


# fit model into 3 clusters 
kfit3 <- kmeans(players_scaled2, 
                centers = 4)
kfit3 # 67.1%

# Clusters with facto-extra 
fviz_cluster(kfit3, 
             data = players_scaled2)

p <- fviz_cluster(kfit3, 
                  data = players_scaled2) # save to access $data
p

data <- p$data # this is all you need

# calculate the convex hull using chull(), for each cluster
hull_data <- data %>%
  group_by(cluster) %>%
  slice(chull(x, y))

colour_theme <- c("#E4796A", "#E7B573", "#B0AA96", "#638DCB")


# plot: you can now customize this by using ggplot sintax
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





wss <- function(k) 
{
  kmeans(players_scaled2, k, nstart = 10)$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15
# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type = "b", 
     pch = 19, # dots
     frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# function to compute average silhouette for k clusters
avg_sil <- function(k) 
{
  km.res <- kmeans(players_scaled2, 
                   centers = k, 
                   nstart = 25)
  ss <- silhouette(km.res$cluster, 
                   dist(players_scaled2))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")







set.seed(123)
# no ruck contests or hit-outs
players3 <- players %>%  
  select(
    Player_Name,
    clearances,
    contested_possessions,
    centre_clearances,
    score_launches,
    pressure_acts, #adds .1%
    stoppage_clearances, # adds ~ 2%
    free_kicks_for, # adds .3%
    tackles, # adds 1%
    goals,
    contest_def_one_on_ones, # makes a large difference to cluster
    handballs,
    inside_fifties,
    uncontested_possessions,
    spoils,
    intercept_marks,
    intercepts,
    ground_ball_gets,
    score_involvements,
    one_percenters
  )

# ----------------
# NOTES 
# ----------------
# CONTRIBUTE NEGATIVELY 

players_scaled3 <- scale(players3[,2:20], center = TRUE, scale = TRUE)
players_numeric3 <- dplyr::select_if(players3, is.numeric)

# https://discuss.analyticsvidhya.com/t/interpretation-result-of-k-means-algorithm/17750

# high similarity within a group = low variance within the cluster, or within_SS.
# low similarity between the groups = high variance between the clusters, or between_SS.


# fit model into 3 clusters 
kfit3 <- kmeans(players_scaled3, 
                centers = 4)
kfit3 # 87.3%

# Clusters with facto-extra 
fviz_cluster(kfit3, 
             data = players_scaled3)

