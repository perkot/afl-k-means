# https://uc-r.github.io/hc_clustering

set.seed(123)

# scale all of our variables to add into k-means model 
all_scaled <- as.data.frame(scale(players[,5:48], 
                                  center = TRUE, 
                                  scale = TRUE))

# raw data - only variables to include in model
all_numeric <- dplyr::select_if(players[,5:48], is.numeric)

all_scaled_m <- all_scaled %>%  
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



# Dissimilarity matrix
d <- dist(all_scaled_m, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Compute with agnes
hc2 <- agnes(all_scaled_m, method = "ward")

# Agglomerative coefficient
hc2$ac
## [1] 0.8531583


# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(all_scaled_m, method = x)$ac
}

map_dbl(m, ac)
##   average    single  complete      ward 
## 0.7379371 0.6276128 0.8531583 0.9346210

hc3 <- agnes(all_scaled_m, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 


# Cut tree into 4 groups
sub_grp <- cutree(hc3, k = 3)


# Number of members in each cluster
table(sub_grp)
## sub_grp
# 1   2   3   4 
# 153 247 124  30 

players %>%
  mutate(cluster = sub_grp) %>%
  head

fviz_cluster(list(data = all_scaled_m, cluster = sub_grp))



