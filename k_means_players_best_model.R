
#### 93.41% ###

set.seed(123)
# SIMPLER MODEL 
players2 <- players %>%  
  select(
    Player_Name,
    hitouts,
    clearances,
    contested_possessions,
    centre_clearances,
    hitouts_to_advantage,
    ruck_contests,
    score_launches,
    pressure_acts, #adds .1%
    stoppage_clearances, # adds ~ 2%
    free_kicks_for, # adds .3%
    tackles # adds 1%
    # score_involvements
  )

players2[,2:12] <- sapply(players2[,2:12],as.numeric)
players_scaled2 <- scale(players2[,2:12], center = TRUE, scale = TRUE)
players_numeric2 <- dplyr::select_if(players2, is.numeric)

# ----------------
# NOTES
# ----------------

# Intercept marks makes this model worse 
# Intercept marks drastically makes model worse 
# inside fifties makes model worse
# disposals makes worse
# kicks makes worse
# spoils makes worse

# ----------------
# MANHATTAN DISTANCE
# ----------------

# The use of the Manhattan distance is advised in those situations where for example 
# a difference of 1 in the first variable,and of 3 in the second variable is the same as 
# a difference of 2 in the first variable and of 2 in the second

d3 <- 
  players_scaled2 %>% 
  daisy(metric = "manhattan") 

# fit model into 3 clusters 
kfit3 <- kmeans(d3, 4)
kfit

# visualise 3 clusters
clusplot(as.matrix(d3), 
         kfit3$cluster, 
         color = T, 
         shade = F, 
         labels = 1, 
         lines = 0,
         cex = 0.7,
         cex.txt = 0.8,
         cex.axis = 0.8,
         cex.lab = 0.8,
         cex.main = 1,
         lwd = 0.8,
         main = '2D PCA-plot of k-means clustering using Manhattan Distance')



#### 77.89% ###

set.seed(123)
# SIMPLER MODEL 
players3 <- players %>%  
  select(
    disposals,
    ruck_contests,
    contest_def_one_on_ones,
    contest_off_one_on_ones
    )

players3[,1:4] <- sapply(players3[,1:4],as.numeric)
players_scaled3 <- scale(players3[,1:4], center = TRUE, scale = TRUE)
players_numeric3 <- dplyr::select_if(players3, is.numeric)

# ----------------
# MANHATTAN DISTANCE
# ----------------

# The use of the Manhattan distance is advised in those situations where for example 
# a difference of 1 in the first variable,and of 3 in the second variable is the same as 
# a difference of 2 in the first variable and of 2 in the second

d3 <- 
  players_scaled3 %>% 
  daisy(metric = "manhattan") 

# fit model into 3 clusters 
kfit3 <- kmeans(d3, 4)

# visualise 3 clusters
clusplot(as.matrix(d3), 
         kfit3$cluster, 
         color = T, 
         shade = F, 
         labels = 1, 
         lines = 0,
         cex = 0.7,
         cex.txt = 0.8,
         cex.axis = 0.8,
         cex.lab = 0.8,
         cex.main = 1,
         lwd = 0.8,
         main = '2D PCA-plot of k-means clustering using Manhattan Distance')




